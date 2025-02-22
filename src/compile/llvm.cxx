module;
#pragma warning(push, 0)
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/NoFolder.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#pragma warning(pop)
#include <generator.hpp>
#include <memory>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
module backend.llvm;
import ast;
import utils;
import type;
import operators;
import operations;
import naming;
import backend.base;

namespace {
struct LlvmVisitor final : backend::Base<LlvmVisitor, llvm::Value *, llvm::Value *> {
	llvm::LLVMContext &llvm_context;
	std::unique_ptr<llvm::Module> mod;
	using IRBuilder = llvm::IRBuilder<llvm::NoFolder>;
	std::unique_ptr<IRBuilder> builder;
	LlvmVisitor(llvm::LLVMContext &llvm_context, std::unique_ptr<llvm::Module> mod)
		: Base{},
		  llvm_context{llvm_context}, mod{std::move(mod)},
		  builder{std::make_unique<IRBuilder>(llvm_context)} {
	}
	llvm::Type *llvm_type(const type::Primitive &type) {
		switch (type.type) {
			case type::Primitive::boolean:
				return llvm::Type::getInt1Ty(llvm_context);
			case type::Primitive::uint8:
			case type::Primitive::int8:
				return llvm::Type::getInt8Ty(llvm_context);
			case type::Primitive::uint16:
			case type::Primitive::int16:
				return llvm::Type::getInt16Ty(llvm_context);
			case type::Primitive::uint32:
			case type::Primitive::int32:
				return llvm::Type::getInt32Ty(llvm_context);
			case type::Primitive::uint64:
			case type::Primitive::int64:
				return llvm::Type::getInt64Ty(llvm_context);
			case type::Primitive::uint128:
			case type::Primitive::int128:
				return llvm::Type::getInt128Ty(llvm_context);
			case type::Primitive::half:
				return llvm::Type::getHalfTy(llvm_context);
			case type::Primitive::float_:
				return llvm::Type::getFloatTy(llvm_context);
			case type::Primitive::double_:
				return llvm::Type::getDoubleTy(llvm_context);
		}
		std::unreachable();
	}
	llvm::Type *llvm_type(const type::Array &type) {
		return llvm::ArrayType::get(llvm_type(*type.member), type.size);
	}
	llvm::Type *llvm_type(const type::Pointer &type) {
		return llvm::PointerType::get(llvm_type(*type.pointed), 0);
	}
	llvm::Type *llvm_type(const type::Slice &type) {
		auto member = llvm::PointerType::get(llvm_type(*type.sliced), 0);
		return llvm::StructType::get(llvm_context, {member, member});
	}
	utils::string_map<llvm::Type *> named_struct_llvm_types;
	llvm::Type *llvm_type(const type::NamedStruct &type) {
		auto name = type.mangle();
		if (const auto it = named_struct_llvm_types.find(name); it != named_struct_llvm_types.end())
			return it->second;
		auto struct_ = llvm::StructType::create(llvm_context, name);
		named_struct_llvm_types.emplace(name, struct_);
		std::vector<llvm::Type *> elements;
		for (const auto &field : type.members | std::views::values)
			elements.push_back(llvm_type(field));
		struct_->setBody(elements, false);
		return struct_;
	}
	llvm::Type *llvm_type(const type::Function &type) {
		std::vector<llvm::Type *> args;
		for (const auto &arg : type.parameters)
			args.push_back(llvm_type(arg));
		return llvm::FunctionType::get(llvm_type(*type.return_type), args, false);
	}
	llvm::Type *llvm_type(const type::Type &type) {
		if (type.is_ref) {
			auto copy = type;
			copy.is_ref = false;
			return llvm::PointerType::get(llvm_type(copy), 0);
		}
		return type.visit([&](const auto &type) { return llvm_type(type); });
	}
	llvm::BasicBlock *
	create_block(const std::string_view name = "tmp",
				 llvm::BasicBlock *insert_before = nullptr) const {
		return llvm::BasicBlock::Create(llvm_context, name,
										builder->GetInsertBlock()->getParent(),
										insert_before);
	}
	llvm::BasicBlock *
	create_block(llvm::Function *parent, const std::string_view name = "tmp",
				 llvm::BasicBlock *insert_before = nullptr) const {
		return llvm::BasicBlock::Create(llvm_context, name, parent, insert_before);
	}
	llvm::Value *generate_function_value(const TopLevelFunction &tlf, const type::Function &type, std::string mangled_name) override {
		const auto &function = *tlf.ast;
		if (const auto fn = mod->getFunction(mangled_name))
			return fn; // already generated
		auto c = mod->getOrInsertFunction(
				mangled_name, llvm::cast<llvm::FunctionType>(llvm_type(type)));
		const auto llvmfn = llvm::cast<llvm::Function>(c.getCallee());
		for (const auto &[astarg, llvmarg] : std::views::zip(
					 function.parameters, llvmfn->args()))
			llvmarg.setName(astarg.name.str);
		if (!function.statement) {
			// extern fun :)
			return llvmfn;
		}
		const auto pre_generate_insert_point = builder->GetInsertBlock(); // save the caller's insert point
		NewScopeHere nsh_preargs{&locals};
		if (!tlf.ast->type_arguments.empty())
			llvmfn->setLinkage(llvm::GlobalValue::LinkOnceODRLinkage); // template functions are linkonce_odr
		const auto entry = create_block(llvmfn, "entry");
		builder->SetInsertPoint(entry);
		for (const auto &[astarg, typarg, llvmarg] : std::views::zip(
					 function.parameters, type.parameters, llvmfn->args()))
			if (typarg.is_ref) {
				locals.back().members.emplace(astarg.name.str, Value{typarg, &llvmarg});
			} else { // turn it into a reference
				auto alloc =
						builder->CreateAlloca(llvm_type(typarg), nullptr, astarg.name.str);
				builder->CreateStore(&llvmarg, alloc);
				type::Type argtype = typarg; // copy
				argtype.is_ref = true;
				locals.back().members.emplace(astarg.name.str, Value{argtype, alloc});
			}
		NewScopeHere nsh_postargs{&locals};
		visit(*function.statement);
		// restore to the caller's insert point
		builder->SetInsertPoint(pre_generate_insert_point);
		return llvmfn;
	}
	// used through CRTP - "maybe_unused" to prevent IDE auto-refactor
	[[maybe_unused]] void impl_if(llvm::Value *condition, auto then, auto otherwise) {
		const auto true_block = create_block("true");
		const auto false_block = create_block("false");
		const auto end_block = create_block();
		builder->CreateCondBr(condition, true_block, false_block);
		builder->SetInsertPoint(true_block);
		then();
		builder->CreateBr(end_block);
		builder->SetInsertPoint(false_block);
		otherwise();
		builder->CreateBr(end_block);
		end_block->moveAfter(builder->GetInsertBlock());
		builder->SetInsertPoint(end_block);
	}
	// used through CRTP - "maybe_unused" to prevent IDE auto-refactor
	[[maybe_unused]] void impl_while(auto condition, auto body) {
		const auto cond_block = create_block("whilecond");
		const auto end_block = create_block("whileend");
		builder->CreateBr(cond_block);
		builder->SetInsertPoint(cond_block);
		llvm::Value *cond_val = condition();
		const auto body_block = create_block("whilebody");
		builder->CreateCondBr(cond_val, body_block, end_block);
		builder->SetInsertPoint(body_block);
		body();
		builder->CreateBr(cond_block);
		end_block->moveAfter(builder->GetInsertBlock());
		builder->SetInsertPoint(end_block);
	}
	llvm::Value *get_bool_value(bool value) override {
		return llvm::ConstantInt::getBool(llvm_context, value);
	}
	void impl_return(Value value) override {
		builder->CreateRet(value.value);
	}
	void impl_expr_stmt(Value value, const ast::stmt::Expr &statement) override {
		value = deref(value);
		const char *format_str;
		if (value.type == type::t_int32)
			format_str = "\"%s\" => %d\n";
		else if (value.type == type::t_uint64)
			format_str = "\"%s\" => %llu\n";
		else if (value.type == type::t_double)
			format_str = "\"%s\" => %f\n";
		else if (value.type == type::t_boolean)
			format_str = "\"%s\" => %d\n";
		else
			throw std::runtime_error(
					fmt("Unknown type to printf ", statement.location));
		const auto printf = mod->getOrInsertFunction(
				"printf",
				llvm::FunctionType::get(
						llvm::Type::getInt32Ty(llvm_context),
						llvm::PointerType::get(llvm::Type::getInt8Ty(llvm_context), 0),
						true));
		builder->CreateCall(
				printf, {builder->CreateGlobalStringPtr(format_str), builder->CreateGlobalStringPtr(statement.location.content), value.value}, "tmp");
	}

	/// If value is NOT a reference, returns it.
	/// Otherwise, `load` and returns this value.
	llvm::Value *impl_deref(const type::Type &when_loaded, llvm::Value *ref) override {
		return builder->CreateLoad(llvm_type(when_loaded), ref, "tmp");
	}
	/// Store the value on the stack, return a pointer to that value.
	llvm::Value *impl_mkref(Value value) override {
		const auto alloca = builder->CreateAlloca(llvm_type(value.type), nullptr, "tmp");
		builder->CreateStore(value.value, alloca);
		return alloca;
	}

	/// Store rvalue into the reference lvalue
	void impl_store(Value lvalue, Value rvalue) override {
		builder->CreateStore(rvalue.value, lvalue.value);
	}

	// used through CRTP - "maybe_unused" to prevent IDE auto-refactor
#define UNARY_IMPL_FUNCTION(name, operation)                   \
	[[maybe_unused]] llvm::Value *name(llvm::Value *operand) { \
		return builder->operation(operand, "tmp");             \
	}

	UNARY_IMPL_FUNCTION(unary_b_not_u8, CreateNot)
	UNARY_IMPL_FUNCTION(unary_b_not_u16, CreateNot)
	UNARY_IMPL_FUNCTION(unary_b_not_u32, CreateNot)
	UNARY_IMPL_FUNCTION(unary_b_not_u64, CreateNot)
	UNARY_IMPL_FUNCTION(unary_b_not_u128, CreateNot)

	UNARY_IMPL_FUNCTION(unary_neg_u8, CreateNeg)
	UNARY_IMPL_FUNCTION(unary_neg_u16, CreateNeg)
	UNARY_IMPL_FUNCTION(unary_neg_u32, CreateNeg)
	UNARY_IMPL_FUNCTION(unary_neg_u64, CreateNeg)
	UNARY_IMPL_FUNCTION(unary_neg_u128, CreateNeg)
	UNARY_IMPL_FUNCTION(unary_neg_i8, CreateNeg)
	UNARY_IMPL_FUNCTION(unary_neg_i16, CreateNeg)
	UNARY_IMPL_FUNCTION(unary_neg_i32, CreateNeg)
	UNARY_IMPL_FUNCTION(unary_neg_i64, CreateNeg)
	UNARY_IMPL_FUNCTION(unary_neg_i128, CreateNeg)

	UNARY_IMPL_FUNCTION(unary_neg_half, CreateFNeg)
	UNARY_IMPL_FUNCTION(unary_neg_float, CreateFNeg)
	UNARY_IMPL_FUNCTION(unary_neg_double, CreateFNeg)

	UNARY_IMPL_FUNCTION(unary_l_not_boolean, CreateNot)

#undef UNARY_IMPL_FUNCTION


	// used through CRTP - "maybe_unused" to prevent IDE auto-refactor
#define BINARY_IMPL_FUNCTION(name, operation)                                \
	[[maybe_unused]] llvm::Value *name(llvm::Value *lhs, llvm::Value *rhs) { \
		return builder->operation(lhs, rhs, "tmp");                          \
	}


	BINARY_IMPL_FUNCTION(binary_b_and_u8, CreateAnd)
	BINARY_IMPL_FUNCTION(binary_b_and_u16, CreateAnd)
	BINARY_IMPL_FUNCTION(binary_b_and_u32, CreateAnd)
	BINARY_IMPL_FUNCTION(binary_b_and_u64, CreateAnd)
	BINARY_IMPL_FUNCTION(binary_b_and_u128, CreateAnd)

	BINARY_IMPL_FUNCTION(binary_b_or_u8, CreateOr)
	BINARY_IMPL_FUNCTION(binary_b_or_u16, CreateOr)
	BINARY_IMPL_FUNCTION(binary_b_or_u32, CreateOr)
	BINARY_IMPL_FUNCTION(binary_b_or_u64, CreateOr)
	BINARY_IMPL_FUNCTION(binary_b_or_u128, CreateOr)

	BINARY_IMPL_FUNCTION(binary_b_xor_u8, CreateXor)
	BINARY_IMPL_FUNCTION(binary_b_xor_u16, CreateXor)
	BINARY_IMPL_FUNCTION(binary_b_xor_u32, CreateXor)
	BINARY_IMPL_FUNCTION(binary_b_xor_u64, CreateXor)
	BINARY_IMPL_FUNCTION(binary_b_xor_u128, CreateXor)

	BINARY_IMPL_FUNCTION(binary_b_shl_u8, CreateShl)
	BINARY_IMPL_FUNCTION(binary_b_shl_u16, CreateShl)
	BINARY_IMPL_FUNCTION(binary_b_shl_u32, CreateShl)
	BINARY_IMPL_FUNCTION(binary_b_shl_u64, CreateShl)
	BINARY_IMPL_FUNCTION(binary_b_shl_u128, CreateShl)

	BINARY_IMPL_FUNCTION(binary_b_shr_u8, CreateLShr)
	BINARY_IMPL_FUNCTION(binary_b_shr_u16, CreateLShr)
	BINARY_IMPL_FUNCTION(binary_b_shr_u32, CreateLShr)
	BINARY_IMPL_FUNCTION(binary_b_shr_u64, CreateLShr)
	BINARY_IMPL_FUNCTION(binary_b_shr_u128, CreateLShr)
	BINARY_IMPL_FUNCTION(binary_b_shr_i8, CreateAShr)
	BINARY_IMPL_FUNCTION(binary_b_shr_i16, CreateAShr)
	BINARY_IMPL_FUNCTION(binary_b_shr_i32, CreateAShr)
	BINARY_IMPL_FUNCTION(binary_b_shr_i64, CreateAShr)
	BINARY_IMPL_FUNCTION(binary_b_shr_i128, CreateAShr)

	BINARY_IMPL_FUNCTION(binary_add_u8, CreateAdd)
	BINARY_IMPL_FUNCTION(binary_add_u16, CreateAdd)
	BINARY_IMPL_FUNCTION(binary_add_u32, CreateAdd)
	BINARY_IMPL_FUNCTION(binary_add_u64, CreateAdd)
	BINARY_IMPL_FUNCTION(binary_add_u128, CreateAdd)
	BINARY_IMPL_FUNCTION(binary_add_i8, CreateAdd)
	BINARY_IMPL_FUNCTION(binary_add_i16, CreateAdd)
	BINARY_IMPL_FUNCTION(binary_add_i32, CreateAdd)
	BINARY_IMPL_FUNCTION(binary_add_i64, CreateAdd)
	BINARY_IMPL_FUNCTION(binary_add_i128, CreateAdd)

	BINARY_IMPL_FUNCTION(binary_sub_u8, CreateSub)
	BINARY_IMPL_FUNCTION(binary_sub_u16, CreateSub)
	BINARY_IMPL_FUNCTION(binary_sub_u32, CreateSub)
	BINARY_IMPL_FUNCTION(binary_sub_u64, CreateSub)
	BINARY_IMPL_FUNCTION(binary_sub_u128, CreateSub)
	BINARY_IMPL_FUNCTION(binary_sub_i8, CreateSub)
	BINARY_IMPL_FUNCTION(binary_sub_i16, CreateSub)
	BINARY_IMPL_FUNCTION(binary_sub_i32, CreateSub)
	BINARY_IMPL_FUNCTION(binary_sub_i64, CreateSub)
	BINARY_IMPL_FUNCTION(binary_sub_i128, CreateSub)

	BINARY_IMPL_FUNCTION(binary_mul_u8, CreateMul)
	BINARY_IMPL_FUNCTION(binary_mul_u16, CreateMul)
	BINARY_IMPL_FUNCTION(binary_mul_u32, CreateMul)
	BINARY_IMPL_FUNCTION(binary_mul_u64, CreateMul)
	BINARY_IMPL_FUNCTION(binary_mul_u128, CreateMul)
	BINARY_IMPL_FUNCTION(binary_mul_i8, CreateMul)
	BINARY_IMPL_FUNCTION(binary_mul_i16, CreateMul)
	BINARY_IMPL_FUNCTION(binary_mul_i32, CreateMul)
	BINARY_IMPL_FUNCTION(binary_mul_i64, CreateMul)
	BINARY_IMPL_FUNCTION(binary_mul_i128, CreateMul)

	BINARY_IMPL_FUNCTION(binary_div_u8, CreateUDiv)
	BINARY_IMPL_FUNCTION(binary_div_u16, CreateUDiv)
	BINARY_IMPL_FUNCTION(binary_div_u32, CreateUDiv)
	BINARY_IMPL_FUNCTION(binary_div_u64, CreateUDiv)
	BINARY_IMPL_FUNCTION(binary_div_u128, CreateUDiv)
	BINARY_IMPL_FUNCTION(binary_div_i8, CreateSDiv)
	BINARY_IMPL_FUNCTION(binary_div_i16, CreateSDiv)
	BINARY_IMPL_FUNCTION(binary_div_i32, CreateSDiv)
	BINARY_IMPL_FUNCTION(binary_div_i64, CreateSDiv)
	BINARY_IMPL_FUNCTION(binary_div_i128, CreateSDiv)

	BINARY_IMPL_FUNCTION(binary_mod_u8, CreateURem)
	BINARY_IMPL_FUNCTION(binary_mod_u16, CreateURem)
	BINARY_IMPL_FUNCTION(binary_mod_u32, CreateURem)
	BINARY_IMPL_FUNCTION(binary_mod_u64, CreateURem)
	BINARY_IMPL_FUNCTION(binary_mod_u128, CreateURem)
	BINARY_IMPL_FUNCTION(binary_mod_i8, CreateSRem)
	BINARY_IMPL_FUNCTION(binary_mod_i16, CreateSRem)
	BINARY_IMPL_FUNCTION(binary_mod_i32, CreateSRem)
	BINARY_IMPL_FUNCTION(binary_mod_i64, CreateSRem)
	BINARY_IMPL_FUNCTION(binary_mod_i128, CreateSRem)

	BINARY_IMPL_FUNCTION(binary_add_half, CreateFAdd)
	BINARY_IMPL_FUNCTION(binary_add_float, CreateFAdd)
	BINARY_IMPL_FUNCTION(binary_add_double, CreateFAdd)

	BINARY_IMPL_FUNCTION(binary_sub_half, CreateFSub)
	BINARY_IMPL_FUNCTION(binary_sub_float, CreateFSub)
	BINARY_IMPL_FUNCTION(binary_sub_double, CreateFSub)

	BINARY_IMPL_FUNCTION(binary_mul_half, CreateFMul)
	BINARY_IMPL_FUNCTION(binary_mul_float, CreateFMul)
	BINARY_IMPL_FUNCTION(binary_mul_double, CreateFMul)

	BINARY_IMPL_FUNCTION(binary_div_half, CreateFDiv)
	BINARY_IMPL_FUNCTION(binary_div_float, CreateFDiv)
	BINARY_IMPL_FUNCTION(binary_div_double, CreateFDiv)

	BINARY_IMPL_FUNCTION(binary_mod_half, CreateFRem)
	BINARY_IMPL_FUNCTION(binary_mod_float, CreateFRem)
	BINARY_IMPL_FUNCTION(binary_mod_double, CreateFRem)

	// used through CRTP - "maybe_unused" to prevent IDE auto-refactor
	[[maybe_unused]] llvm::Value *binary_pow_float(llvm::Value *lhs, llvm::Value *rhs) {
		const auto ty = llvm::Type::getFloatTy(llvm_context);
		return builder->CreateCall(
				mod->getOrInsertFunction("llvm.pow.f32", ty, ty, ty),
				{lhs, rhs}, "tmp");
	}
	// used through CRTP - "maybe_unused" to prevent IDE auto-refactor
	[[maybe_unused]] llvm::Value *binary_pow_double(llvm::Value *lhs, llvm::Value *rhs) {
		const auto ty = llvm::Type::getDoubleTy(llvm_context);
		return builder->CreateCall(
				mod->getOrInsertFunction("llvm.pow.f64", ty, ty, ty),
				{lhs, rhs}, "tmp");
	}
#undef BINARY_IMPL_FUNCTION
	Value impl_logical(const ast::expr::Binop &expr) override {
		auto left = visit(expr.left);
		if (left.type != type::boolean)
			throw std::runtime_error(fmt(
					"Left side of logical expression is not a boolean ", expr.location));
		auto left_bb = builder->GetInsertBlock();
		auto func = left_bb->getParent();
		auto right_bb = create_block(func);
		auto merge_bb = create_block(func);
		if (expr.op == operators::l_or) {
			// if left is true, skip right
			builder->CreateCondBr(left.value, merge_bb, right_bb);
		} else /* expr.op == operators::l_and */ {
			// if left is false, skip right
			builder->CreateCondBr(left.value, right_bb, merge_bb);
		}
		builder->SetInsertPoint(right_bb);
		auto right = visit(expr.right);
		builder->CreateBr(merge_bb);
		// right can change the current block, update right_bb for the PHI.
		right_bb = builder->GetInsertBlock();
		// merge
		builder->SetInsertPoint(merge_bb);
		auto phi = builder->CreatePHI(llvm_type(type::boolean), 2, "tmp");
		phi->addIncoming(left.value, left_bb);
		phi->addIncoming(right.value, right_bb);
		return Value{type::boolean, phi};
	}
	llvm::Value *impl_call(const type::Function &type, llvm::Value *fn, std::span<llvm::Value *> values) override {
		const llvm::ArrayRef args{values.data(), values.size()};
		return builder->CreateCall(llvm::cast<llvm::FunctionType>(llvm_type(type)),
								   fn, args, "tmp");
	}
	llvm::Value *impl_value_call(const type::Function &type, llvm::Value *fn, std::span<llvm::Value *> values) override {
		return impl_call(type, fn, values);
	}


#define COMPARE_IMPL_FUNCTION(name, operation)              \
	llvm::Value *name(llvm::Value *lhs, llvm::Value *rhs) { \
		return builder->operation(lhs, rhs, "tmp");         \
	}
	COMPARE_IMPL_FUNCTION(comparison_less_u8, CreateICmpULT)
	COMPARE_IMPL_FUNCTION(comparison_less_u16, CreateICmpULT)
	COMPARE_IMPL_FUNCTION(comparison_less_u32, CreateICmpULT)
	COMPARE_IMPL_FUNCTION(comparison_less_u64, CreateICmpULT)
	COMPARE_IMPL_FUNCTION(comparison_less_u128, CreateICmpULT)
	COMPARE_IMPL_FUNCTION(comparison_less_ptr, CreateICmpULT)

	COMPARE_IMPL_FUNCTION(comparison_less_eq_u8, CreateICmpULE)
	COMPARE_IMPL_FUNCTION(comparison_less_eq_u16, CreateICmpULE)
	COMPARE_IMPL_FUNCTION(comparison_less_eq_u32, CreateICmpULE)
	COMPARE_IMPL_FUNCTION(comparison_less_eq_u64, CreateICmpULE)
	COMPARE_IMPL_FUNCTION(comparison_less_eq_u128, CreateICmpULE)
	COMPARE_IMPL_FUNCTION(comparison_less_eq_ptr, CreateICmpULE)

	COMPARE_IMPL_FUNCTION(comparison_greater_u8, CreateICmpUGT)
	COMPARE_IMPL_FUNCTION(comparison_greater_u16, CreateICmpUGT)
	COMPARE_IMPL_FUNCTION(comparison_greater_u32, CreateICmpUGT)
	COMPARE_IMPL_FUNCTION(comparison_greater_u64, CreateICmpUGT)
	COMPARE_IMPL_FUNCTION(comparison_greater_u128, CreateICmpUGT)
	COMPARE_IMPL_FUNCTION(comparison_greater_ptr, CreateICmpUGT)

	COMPARE_IMPL_FUNCTION(comparison_greater_eq_u8, CreateICmpUGE)
	COMPARE_IMPL_FUNCTION(comparison_greater_eq_u16, CreateICmpUGE)
	COMPARE_IMPL_FUNCTION(comparison_greater_eq_u32, CreateICmpUGE)
	COMPARE_IMPL_FUNCTION(comparison_greater_eq_u64, CreateICmpUGE)
	COMPARE_IMPL_FUNCTION(comparison_greater_eq_u128, CreateICmpUGE)
	COMPARE_IMPL_FUNCTION(comparison_greater_eq_ptr, CreateICmpUGT)

	COMPARE_IMPL_FUNCTION(comparison_eq_eq_u8, CreateICmpEQ)
	COMPARE_IMPL_FUNCTION(comparison_eq_eq_u16, CreateICmpEQ)
	COMPARE_IMPL_FUNCTION(comparison_eq_eq_u32, CreateICmpEQ)
	COMPARE_IMPL_FUNCTION(comparison_eq_eq_u64, CreateICmpEQ)
	COMPARE_IMPL_FUNCTION(comparison_eq_eq_u128, CreateICmpEQ)
	COMPARE_IMPL_FUNCTION(comparison_eq_eq_ptr, CreateICmpEQ)

	COMPARE_IMPL_FUNCTION(comparison_not_equal_u8, CreateICmpNE)
	COMPARE_IMPL_FUNCTION(comparison_not_equal_u16, CreateICmpNE)
	COMPARE_IMPL_FUNCTION(comparison_not_equal_u32, CreateICmpNE)
	COMPARE_IMPL_FUNCTION(comparison_not_equal_u64, CreateICmpNE)
	COMPARE_IMPL_FUNCTION(comparison_not_equal_u128, CreateICmpNE)
	COMPARE_IMPL_FUNCTION(comparison_not_equal_ptr, CreateICmpNE)

	COMPARE_IMPL_FUNCTION(comparison_less_i8, CreateICmpSLT)
	COMPARE_IMPL_FUNCTION(comparison_less_i16, CreateICmpSLT)
	COMPARE_IMPL_FUNCTION(comparison_less_i32, CreateICmpSLT)
	COMPARE_IMPL_FUNCTION(comparison_less_i64, CreateICmpSLT)
	COMPARE_IMPL_FUNCTION(comparison_less_i128, CreateICmpSLT)

	COMPARE_IMPL_FUNCTION(comparison_less_eq_i8, CreateICmpSLE)
	COMPARE_IMPL_FUNCTION(comparison_less_eq_i16, CreateICmpSLE)
	COMPARE_IMPL_FUNCTION(comparison_less_eq_i32, CreateICmpSLE)
	COMPARE_IMPL_FUNCTION(comparison_less_eq_i64, CreateICmpSLE)
	COMPARE_IMPL_FUNCTION(comparison_less_eq_i128, CreateICmpSLE)

	COMPARE_IMPL_FUNCTION(comparison_greater_i8, CreateICmpSGT)
	COMPARE_IMPL_FUNCTION(comparison_greater_i16, CreateICmpSGT)
	COMPARE_IMPL_FUNCTION(comparison_greater_i32, CreateICmpSGT)
	COMPARE_IMPL_FUNCTION(comparison_greater_i64, CreateICmpSGT)
	COMPARE_IMPL_FUNCTION(comparison_greater_i128, CreateICmpSGT)

	COMPARE_IMPL_FUNCTION(comparison_greater_eq_i8, CreateICmpSGE)
	COMPARE_IMPL_FUNCTION(comparison_greater_eq_i16, CreateICmpSGE)
	COMPARE_IMPL_FUNCTION(comparison_greater_eq_i32, CreateICmpSGE)
	COMPARE_IMPL_FUNCTION(comparison_greater_eq_i64, CreateICmpSGE)
	COMPARE_IMPL_FUNCTION(comparison_greater_eq_i128, CreateICmpSGE)

	COMPARE_IMPL_FUNCTION(comparison_eq_eq_i8, CreateICmpEQ)
	COMPARE_IMPL_FUNCTION(comparison_eq_eq_i16, CreateICmpEQ)
	COMPARE_IMPL_FUNCTION(comparison_eq_eq_i32, CreateICmpEQ)
	COMPARE_IMPL_FUNCTION(comparison_eq_eq_i64, CreateICmpEQ)
	COMPARE_IMPL_FUNCTION(comparison_eq_eq_i128, CreateICmpEQ)

	COMPARE_IMPL_FUNCTION(comparison_not_equal_i8, CreateICmpNE)
	COMPARE_IMPL_FUNCTION(comparison_not_equal_i16, CreateICmpNE)
	COMPARE_IMPL_FUNCTION(comparison_not_equal_i32, CreateICmpNE)
	COMPARE_IMPL_FUNCTION(comparison_not_equal_i64, CreateICmpNE)
	COMPARE_IMPL_FUNCTION(comparison_not_equal_i128, CreateICmpNE)


	COMPARE_IMPL_FUNCTION(comparison_less_half, CreateFCmpOLT)
	COMPARE_IMPL_FUNCTION(comparison_less_float, CreateFCmpOLT)
	COMPARE_IMPL_FUNCTION(comparison_less_double, CreateFCmpOLT)

	COMPARE_IMPL_FUNCTION(comparison_less_eq_half, CreateFCmpOLE)
	COMPARE_IMPL_FUNCTION(comparison_less_eq_float, CreateFCmpOLE)
	COMPARE_IMPL_FUNCTION(comparison_less_eq_double, CreateFCmpOLE)

	COMPARE_IMPL_FUNCTION(comparison_greater_half, CreateFCmpOGT)
	COMPARE_IMPL_FUNCTION(comparison_greater_float, CreateFCmpOGT)
	COMPARE_IMPL_FUNCTION(comparison_greater_double, CreateFCmpOGT)

	COMPARE_IMPL_FUNCTION(comparison_greater_eq_half, CreateFCmpOGE)
	COMPARE_IMPL_FUNCTION(comparison_greater_eq_float, CreateFCmpOGE)
	COMPARE_IMPL_FUNCTION(comparison_greater_eq_double, CreateFCmpOGE)

	COMPARE_IMPL_FUNCTION(comparison_eq_eq_half, CreateFCmpOEQ)
	COMPARE_IMPL_FUNCTION(comparison_eq_eq_float, CreateFCmpOEQ)
	COMPARE_IMPL_FUNCTION(comparison_eq_eq_double, CreateFCmpOEQ)

	COMPARE_IMPL_FUNCTION(comparison_not_equal_half, CreateFCmpONE)
	COMPARE_IMPL_FUNCTION(comparison_not_equal_float, CreateFCmpONE)
	COMPARE_IMPL_FUNCTION(comparison_not_equal_double, CreateFCmpONE)

	Value impl_comparison(const ast::expr::Comparison &expr) override {
		auto lvalue = visit(&expr.operands[0]);
		const auto start_block = builder->GetInsertBlock();
		const auto end_block = create_block();
		builder->SetInsertPoint(end_block);
		const auto result_type = type::t_boolean;
		const auto phi = builder->CreatePHI(llvm_type(result_type), 2, "tmp");
		builder->SetInsertPoint(start_block);
		for (const auto &[sindex, expr_op] : expr.ops | std::views::enumerate) {
			const size_t index = sindex;
			const bool is_last = index == expr.ops.size() - 1;
			auto rvalue = visit(&expr.operands[index + 1]);
			auto find_op = [&]() {
				for (auto &[op, gen] : comparison_operations) {
					if (op.op == expr_op && op.left == lvalue.type &&
						op.right == rvalue.type) {
						// do stuff and && for (1 < 2 <= 3)
						const auto result = gen(*this, lvalue, rvalue);
						if (is_last) {
							phi->addIncoming(result.value, builder->GetInsertBlock());
							builder->CreateBr(end_block);
							end_block->moveAfter(builder->GetInsertBlock());
							builder->SetInsertPoint(end_block);
						} else {
							const auto next_block = create_block();
							builder->CreateCondBr(result.value, next_block, end_block);
							phi->addIncoming(llvm::ConstantInt::getFalse(llvm_context),
											 builder->GetInsertBlock());
							builder->SetInsertPoint(next_block);
							// add incoming to phi. if we jumped to end_block it factually
							// means that the comparison was false.
						}
						lvalue = rvalue;
						return true;
					}
				}
				return false;
			};
			if (find_op())
				continue;
			lvalue = deref(lvalue);
			rvalue = deref(rvalue);
			if (!find_op())
				throw std::runtime_error(fmt("No matching comparison ", expr.location));
		}
		return Value{result_type, phi};
	}

	Value impl_conditional(const ast::expr::Conditional &expr) override {
		const auto true_block = create_block("true");
		const auto false_block = create_block("false");
		const auto end_block = create_block();
		const auto condition = visit(expr.condition);
		builder->CreateCondBr(condition.value, true_block, false_block);
		builder->SetInsertPoint(true_block);
		const auto true_value = visit(expr.thenExpr);
		const auto true_end_block = builder->GetInsertBlock();
		builder->SetInsertPoint(end_block);
		const auto phi = builder->CreatePHI(llvm_type(true_value.type), 2, "tmp");
		builder->SetInsertPoint(true_end_block);
		builder->CreateBr(end_block);
		phi->addIncoming(true_value.value, true_end_block);
		builder->SetInsertPoint(false_block);
		const auto false_value = visit(expr.elseExpr);
		if (true_value.type != false_value.type)
			throw std::runtime_error(fmt(
					"Types of true and false expressions do not match ", expr.location));
		phi->addIncoming(false_value.value, builder->GetInsertBlock());
		builder->CreateBr(end_block);
		end_block->moveAfter(builder->GetInsertBlock());
		builder->SetInsertPoint(end_block);
		return Value{true_value.type, phi};
	}

	llvm::Value *impl_create(const type::NamedStruct &type, std::span<llvm::Value *> members) override {
		auto alloca = builder->CreateAlloca(llvm_type(type), nullptr, "alloca");
		auto struct_ = llvm::cast<llvm::StructType>(llvm_type(type));
		for (const auto &[sindex, member] : std::views::enumerate(members)) {
			const size_t index = sindex;
			auto member_alloc = builder->CreateStructGEP(struct_, alloca, static_cast<uint32_t>(index), "tmp");
			builder->CreateStore(member, member_alloc);
		}
		return alloca;
	}

	llvm::Value *impl_arr_ref_start_ref(const type::Array &array_type, llvm::Value *arr_ref) override {
		return builder->CreateStructGEP(llvm_type(array_type), arr_ref, 0, "tmp");
	}

	llvm::Value *impl_slice_start_ptr(const type::Slice &slice_type, llvm::Value *slice_ref) override {
		// index to the first ptr of the two
		auto gep = builder->CreateStructGEP(llvm_type(slice_type), slice_ref, 0, "tmp");
		// load that pointer
		auto load_ptr = builder->CreateLoad(llvm::PointerType::get(llvm_type(*slice_type.sliced), 0), gep, "tmp");
		return load_ptr;
	}

	llvm::Value *impl_slice_end_ptr(const type::Slice &slice_type, llvm::Value *slice_ref) override {
		// index to the second ptr of the two
		auto gep = builder->CreateStructGEP(llvm_type(slice_type), slice_ref, 1, "tmp");
		// load that pointer
		auto load_ptr = builder->CreateLoad(llvm::PointerType::get(llvm_type(*slice_type.sliced), 0), gep, "tmp");
		return load_ptr;
	}

	llvm::Value *impl_subscript_ptr(const type::Type &pointed_type, llvm::Value *pointer, llvm::Value *index) override {
		return builder->CreateGEP(llvm_type(pointed_type), pointer, index, "tmp");
	}

	llvm::Value *impl_slice_ptr(const type::Slice &slice_type, llvm::Value *from, llvm::Value *to) override {
		auto slice_t = llvm_type(slice_type);
		llvm::Value *combined = llvm::PoisonValue::get(slice_t);
		combined = builder->CreateInsertValue(combined, from, 0, "tmp");
		combined = builder->CreateInsertValue(combined, to, 1, "tmp");
		return combined;
	}

	llvm::Value *impl_ptrdiff(const type::Type &element, llvm::Value *left, llvm::Value *right) override {
		return builder->CreatePtrDiff(llvm_type(element), left, right, "tmp");
	}

	llvm::Value *impl_array(const type::Array &type, std::span<llvm::Value *> members) override {
		auto alloca = builder->CreateAlloca(llvm_type(type), nullptr, "alloca");
		auto llvm_array = llvm_type(type);
		for (const auto &[sindex, member] : std::views::enumerate(members)) {
			const size_t index = sindex;
			auto member_ptr = builder->CreateStructGEP(llvm_array, alloca, static_cast<uint32_t>(index), "tmp");
			builder->CreateStore(member, member_ptr);
		}
		return alloca;
	}

	llvm::Value *impl_numeric_convert(llvm::Value *value, type::Primitive from, type::Primitive to) override {
		if (from.is_floating()) {
			if (to.is_floating())
				return builder->CreateFPCast(value, llvm_type(to), "tmp");
			if (to.is_signed())
				return builder->CreateFPToSI(value, llvm_type(to), "tmp");
			if (to.is_unsigned())
				return builder->CreateFPToUI(value, llvm_type(to), "tmp");
		}
		if (from.is_signed() && to.is_floating()) {
			return builder->CreateSIToFP(value, llvm_type(to), "tmp");
		}
		if (from.is_unsigned() && to.is_floating()) {
			return builder->CreateUIToFP(value, llvm_type(to), "tmp");
		}
		return builder->CreateIntCast(value, llvm_type(to), to.is_signed(), "tmp");
	}

	llvm::Value *impl_member(const type::NamedStruct &type, llvm::Value *struct_ref, size_t member_index) override {
		return builder->CreateStructGEP(llvm_type(type), struct_ref, static_cast<uint32_t>(member_index), "tmp");
	}

	llvm::Value *impl_const_int32(int32_t value) override {
		return llvm::ConstantInt::get(
				llvm::Type::getInt32Ty(llvm_context), value);
	}
	llvm::Value *impl_const_uintmax(std::uintmax_t value) override {
		return llvm::ConstantInt::get(
				llvm::Type::getIntNTy(llvm_context, sizeof(std::uintmax_t) * 8), value);
	}
	llvm::Value *impl_const_double(double value) override {
		return llvm::ConstantFP::get(
				llvm::Type::getDoubleTy(llvm_context), value);
	}
};
} // namespace

void backend::llvm::generate_object_file(const std::span<ast::Program> programs,
										 const std::string_view to) {
	auto context = ::llvm::LLVMContext{};
	LlvmVisitor visitor{context, std::make_unique<::llvm::Module>("complete", context)};
	visitor.type_pass = true;
	for (const auto &program : programs)
		visitor.visit(program);
	visitor.type_pass = false;
	for (const auto &program : programs)
		visitor.visit(program);
	std::error_code ec;
	::llvm::raw_fd_stream file(to, ec);
	visitor.mod->print(file, nullptr, false, true);
}