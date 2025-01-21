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
#include <sstream>
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
namespace constexpr_primitives {
constexpr type::Type t_boolean{type::Primitive{type::Primitive::boolean}};
constexpr type::Type t_uint8{type::Primitive{type::Primitive::uint8}};
constexpr type::Type t_int8{type::Primitive{type::Primitive::int8}};
constexpr type::Type t_uint16{type::Primitive{type::Primitive::uint16}};
constexpr type::Type t_int16{type::Primitive{type::Primitive::int16}};
constexpr type::Type t_uint32{type::Primitive{type::Primitive::uint32}};
constexpr type::Type t_int32{type::Primitive{type::Primitive::int32}};
constexpr type::Type t_uint64{type::Primitive{type::Primitive::uint64}};
constexpr type::Type t_int64{type::Primitive{type::Primitive::int64}};
constexpr type::Type t_uint128{type::Primitive{type::Primitive::uint128}};
constexpr type::Type t_int128{type::Primitive{type::Primitive::int128}};
constexpr type::Type t_half{type::Primitive{type::Primitive::half}};
constexpr type::Type t_float{type::Primitive{type::Primitive::float_}};
constexpr type::Type t_double{type::Primitive{type::Primitive::double_}};

constexpr std::array unsigneds{t_uint8, t_uint16, t_uint32, t_uint64, t_uint128};
constexpr std::array signeds{t_int8, t_int16, t_int32, t_int64, t_int128};
constexpr std::array integers{t_uint8, t_int8, t_uint16, t_int16, t_uint32, t_int32, t_uint64, t_int64, t_uint128, t_int128};
constexpr std::array floats{t_half, t_float, t_double};
constexpr std::array numeric{t_uint8, t_int8, t_uint16, t_int16, t_uint32, t_int32, t_uint64, t_int64, t_uint128, t_int128, t_half, t_float, t_double};
} // namespace constexpr_primitives
struct LlvmVisitor final : backend::Base<llvm::Value *, llvm::Value *> {
	llvm::LLVMContext &llvm_context;
	std::unique_ptr<llvm::Module> mod;
	using IRBuilder = llvm::IRBuilder<llvm::NoFolder>;
	std::unique_ptr<IRBuilder> builder;
	LlvmVisitor(llvm::LLVMContext &llvm_context, std::unique_ptr<llvm::Module> mod)
		: Base{},
		  llvm_context{llvm_context}, mod{std::move(mod)},
		  builder{std::make_unique<IRBuilder>(llvm_context)} {
	}
	using Base::visit;
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
	std::unordered_map<std::string, llvm::Type *> named_structs;
	llvm::Type *llvm_type(const type::NamedStruct &type) {
		auto name = type.mangle();
		if (const auto it = named_structs.find(name); it != named_structs.end())
			return it->second;
		auto struct_ = llvm::StructType::create(llvm_context, name);
		named_structs.emplace(name, struct_);
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
	create_block(const std::string_view name = "",
				 llvm::BasicBlock *insert_before = nullptr) const {

		return llvm::BasicBlock::Create(llvm_context, name,
										builder->GetInsertBlock()->getParent(),
										insert_before);
	}
	llvm::BasicBlock *
	create_block(llvm::Function *parent, const std::string_view name = "",
				 llvm::BasicBlock *insert_before = nullptr) const {

		return llvm::BasicBlock::Create(llvm_context, name, parent, insert_before);
	}
	void visit(const ast::Program &program) {
		locals.emplace_back();
		for (auto &top : program.tops)
			visit(&top);
		locals.pop_back();
	}
	void visit(const ast::TopLevelPtr &top) { visit(top.get()); }
	void visit(const ast::TopLevelAst *top) {
		top->visit([&](const auto &top) { visit(top); });
	}
	void visit(const ast::top::Function &function) {
		auto ns = locals.ns->namespace_(function.name, false);
		auto &tlf = ns->emplace_function(this, function.name.final.str, ns, &function);
		all_function_container.emplace_function(this, tlf);
		if (function.type_arguments.empty())
			tlf.value(this, {}, true); // always compile non-templated functions
	}
	llvm::Value *generate_function_value(const TopLevelFunction &tlf, const type::Function &type) override {
		const auto pre_generate_insert_point = builder->GetInsertBlock(); // save the caller's insert point
		auto ns = tlf.ns;
		const auto &function = *tlf.ast;
		naming::FullName name{ns->path(), function.name.final.str};
		// LocalScope old_locals(ns);
		// std::swap(locals, old_locals); // save the caller's locals // TopLevelFunction::value already does this and is this function's only caller. it also sets up template arguments into locals.
		locals.emplace_back(); // preargs
		auto mangled_name = name.mangle() + type.mangle();
		if (name == naming::FullName{{root_namespace.name}, "main"} &&
			*type.return_type == type::t_int32) {
			// int main is special <3 until we eventually
			// change it and probably have a YIPPEE-universal
			// main function to make args a little nicer
			mangled_name = "main";
		}

		if (auto fn = mod->getFunction(mangled_name))
			return fn; // already generated
		auto c = mod->getOrInsertFunction(
				mangled_name, llvm::cast<llvm::FunctionType>(llvm_type(type)));
		const auto llvmfn = llvm::cast<llvm::Function>(c.getCallee());
		if (!tlf.ast->type_arguments.empty())
			llvmfn->setLinkage(llvm::GlobalValue::LinkOnceODRLinkage); // template functions are linkonce_odr
		const auto entry = create_block(llvmfn, "entry");
		builder->SetInsertPoint(entry);
		for (const auto &[astarg, typarg, llvmarg] : std::views::zip(
					 function.parameters, type.parameters, llvmfn->args())) {
			if (typarg.is_ref) {
				llvmarg.setName(astarg.first.str);
				locals.back().values.emplace(astarg.first.str, Value{typarg, &llvmarg});
			} else { // turn it into a reference
				llvmarg.setName(astarg.first.str);
				auto alloc =
						builder->CreateAlloca(llvm_type(typarg), nullptr, astarg.first.str);
				builder->CreateStore(&llvmarg, alloc);
				type::Type argtype = typarg; // copy
				argtype.is_ref = true;
				locals.back().values.emplace(astarg.first.str, Value{argtype, alloc});
			}
		}
		locals.emplace_back(); // postargs
		visit(function.statement);
		// locals.pop_back(); // postargs
		// locals.pop_back(); // preargs
		// restore to the caller's locals
		// std::swap(locals, old_locals);
		// restore to the caller's insert point
		builder->SetInsertPoint(pre_generate_insert_point);
		return llvmfn;
	}
	void visit(const ast::top::Namespace &ast) {
		Namespace *ns = locals.ns;
		// make every part of the namespace
		for (auto &part : ast.name.parts) {
			Namespace new_ns{ns, part.str};
			ns = &ns->children.try_emplace(new_ns.name, std::move(new_ns))
						  .first->second;
		}
		Namespace _new_ns{ns, ast.name.final.str};
		ns = &ns->children.try_emplace(_new_ns.name, std::move(_new_ns))
					  .first->second;
		LocalScope old_locals(ns);
		std::swap(locals, old_locals);
		locals.emplace_back();
		for (auto &top : ast.tops)
			visit(&top);
		locals.pop_back();
		std::swap(locals, old_locals);
	}
	void visit(const ast::StatementPtr &statement) { visit(statement.get()); }
	void visit(const ast::StatementAst *statement) {
		statement->visit(
				[&](const auto &statement) { visit(statement); });
	}
	void visit(const ast::stmt::Block &statement) {
		locals.emplace_back();
		for (const auto &stmt : statement.statements)
			visit(&stmt);
		locals.pop_back();
	}
	void visit(const ast::stmt::If &statement) {
		const auto true_block = create_block("true");
		const auto false_block = create_block("false");
		const auto end_block = create_block();
		const auto condition = visit(statement.condition);
		builder->CreateCondBr(condition.value, true_block, false_block);
		builder->SetInsertPoint(true_block);
		visit(statement.then);
		builder->CreateBr(end_block);
		builder->SetInsertPoint(false_block);
		if (statement.otherwise)
			visit(*statement.otherwise);
		builder->CreateBr(end_block);
		end_block->moveAfter(builder->GetInsertBlock());
		builder->SetInsertPoint(end_block);
	}
	void visit(const ast::stmt::For &statement) {
		locals.emplace_back(); // init variable is local
		const auto init_block = create_block("forinit");
		const auto end_block = create_block("forend");
		builder->CreateBr(init_block);
		builder->SetInsertPoint(init_block);
		if (statement.init)
			visit(*statement.init);
		const auto cond_block = create_block("forcond");
		builder->CreateBr(cond_block);
		builder->SetInsertPoint(cond_block);
		const auto condition =
				statement.cond
						? visit(*statement.cond)
						: Value{type::boolean, llvm::ConstantInt::getTrue(llvm_context)};
		const auto body_block = create_block("forbody");
		builder->CreateCondBr(condition.value, body_block, end_block);
		builder->SetInsertPoint(body_block);
		visit(statement.body);
		const auto incr_block = create_block("forincr");
		builder->CreateBr(incr_block);
		builder->SetInsertPoint(incr_block);
		if (statement.incr)
			visit(*statement.incr);
		builder->CreateBr(cond_block);
		end_block->moveAfter(builder->GetInsertBlock());
		builder->SetInsertPoint(end_block);
		locals.pop_back();
	}
	void visit(const ast::stmt::While &statement) {
		const auto cond_block = create_block("whilecond");
		const auto end_block = create_block("whileend");
		builder->CreateBr(cond_block);
		builder->SetInsertPoint(cond_block);
		const auto condition = visit(statement.expr);
		const auto body_block = create_block("whilebody");
		builder->CreateCondBr(condition.value, body_block, end_block);
		builder->SetInsertPoint(body_block);
		visit(statement.body);
		builder->CreateBr(cond_block);
		end_block->moveAfter(builder->GetInsertBlock());
		builder->SetInsertPoint(end_block);
	}
	void visit(const ast::stmt::Return &statement) {
		const auto value = deref(visit(statement.expr));
		builder->CreateRet(value.value);
	}
	void visit(const ast::stmt::Expr &statement) {
		auto value = deref(visit(statement.expr));
		const char *format_str;
		if (value.type == type::t_int32)
			format_str = "%d\n";
		else if (value.type == type::t_double)
			format_str = "%f\n";
		else if (value.type == type::t_boolean)
			format_str = "%d\n";
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
				printf, {builder->CreateGlobalStringPtr(format_str), value.value});
	}

	void visit(const ast::stmt::Variable &statement) {
		const auto value = deref(visit(statement.expr));
		auto type = statement.type ? visit(*statement.type) : value.type;
		const auto alloc =
				builder->CreateAlloca(llvm_type(type), nullptr, statement.name);
		builder->CreateStore(value.value, alloc);
		auto &values = locals.back().values;
		if (values.contains(statement.name))
			throw std::runtime_error(fmt("Variable '", statement.name,
										 "' already defined ", statement.location));
		type.is_ref = true;
		type.is_const = false;
		values.emplace(statement.name, Value{std::move(type), alloc});
	}

	Value visit(const ast::ExprPtr &expr) { return visit(expr.get()); }
	Value visit(const ast::ExprAst *expr) {
		return expr->visit(
				[&](const auto &expr) -> Value { return visit(expr); });
	}

	using bop_generator = Value (*)(LlvmVisitor &, const Value &, const Value &);
	using unary_generator = Value (*)(LlvmVisitor &, const Value &);

	/// If value is NOT a reference, returns it.
	/// Otherwise, `load` and returns this value.
	Value deref(Value value) {
		if (!value.type.is_ref)
			return value;
		auto loaded_type = value.type; // copy
		loaded_type.is_const = false;
		loaded_type.is_ref = false;
		const auto loaded =
				builder->CreateLoad(llvm_type(loaded_type), value.value);
		return Value{loaded_type, loaded};
	}
	/// If value is a reference, returns it.
	/// Otherwise, `alloca`s and returns a const T&.
	Value mkref(Value value) {
		if (value.type.is_ref)
			return value;
		auto loaded_type = value.type; // copy
		loaded_type.is_const = false;
		loaded_type.is_ref = false;
		const auto loaded =
				builder->CreateLoad(llvm_type(loaded_type), value.value);
		return Value{loaded_type, loaded};
	}
	template<const type::Type &left, const operators::binary op, llvm::Value *(*Lambda)(LlvmVisitor &, llvm::Value *, llvm::Value *), const type::Type &right = left, const type::Type &result = left>
	static std::pair<operation::Binary, bop_generator>
	bop() {
		return std::make_pair<operation::Binary, bop_generator>(
				operation::Binary{left, right, op},
				static_cast<bop_generator>(+[](LlvmVisitor &visitor, const Value &lhs, const Value &rhs) -> Value {
					return Value{result, Lambda(visitor, visitor.deref(lhs).value, visitor.deref(rhs).value)};
				}));
	}
#define BUILDER_FUNCTION(name)                                        \
	[](LlvmVisitor &visitor, llvm::Value *left, llvm::Value *right) { \
		return visitor.builder->name(left, right);                    \
	}
	static std::vector<std::pair<operation::Binary, bop_generator>>
	builtin_binary_operations() {
		std::vector<std::pair<operation::Binary, bop_generator>> bops{};
		using namespace constexpr_primitives;
		utils::integer_range_loop<0, integers.size()>([&]<size_t Index>() {
			constexpr auto &same = integers[Index];
			bops.push_back(bop<same, operators::b_and, BUILDER_FUNCTION(CreateAnd)>());
			bops.push_back(bop<same, operators::b_or, BUILDER_FUNCTION(CreateOr)>());
			bops.push_back(bop<same, operators::b_xor, BUILDER_FUNCTION(CreateXor)>());
			bops.push_back(bop<same, operators::b_shl, BUILDER_FUNCTION(CreateShl)>());
			bops.push_back(bop<same, operators::add, BUILDER_FUNCTION(CreateAdd)>());
			bops.push_back(bop<same, operators::sub, BUILDER_FUNCTION(CreateSub)>());
			bops.push_back(bop<same, operators::mul, BUILDER_FUNCTION(CreateMul)>());
		});
		utils::integer_range_loop<0, unsigneds.size()>([&]<size_t Index>() {
			constexpr auto &uns = unsigneds[Index];
			bops.push_back(bop<uns, operators::b_shr, BUILDER_FUNCTION(CreateLShr)>());
			bops.push_back(bop<uns, operators::div, BUILDER_FUNCTION(CreateUDiv)>());
			bops.push_back(bop<uns, operators::mod, BUILDER_FUNCTION(CreateURem)>());
			constexpr auto &sig = signeds[Index];
			// int32 + uint32 = int32
			bops.push_back(bop<sig, operators::b_and, BUILDER_FUNCTION(CreateAnd), uns, sig>());
			bops.push_back(bop<sig, operators::b_or, BUILDER_FUNCTION(CreateOr), uns, sig>());
			bops.push_back(bop<sig, operators::b_xor, BUILDER_FUNCTION(CreateXor), uns, sig>());
			bops.push_back(bop<sig, operators::b_shl, BUILDER_FUNCTION(CreateShl), uns, sig>());
			bops.push_back(bop<sig, operators::add, BUILDER_FUNCTION(CreateAdd), uns, sig>());
			bops.push_back(bop<sig, operators::sub, BUILDER_FUNCTION(CreateSub), uns, sig>());
			bops.push_back(bop<sig, operators::mul, BUILDER_FUNCTION(CreateMul), uns, sig>());
			// uint32 + int32 = int32
			bops.push_back(bop<uns, operators::b_and, BUILDER_FUNCTION(CreateAnd), sig, sig>());
			bops.push_back(bop<uns, operators::b_or, BUILDER_FUNCTION(CreateOr), sig, sig>());
			bops.push_back(bop<uns, operators::b_xor, BUILDER_FUNCTION(CreateXor), sig, sig>());
			bops.push_back(bop<uns, operators::b_shl, BUILDER_FUNCTION(CreateShl), sig, sig>());
			bops.push_back(bop<uns, operators::add, BUILDER_FUNCTION(CreateAdd), sig, sig>());
			bops.push_back(bop<uns, operators::sub, BUILDER_FUNCTION(CreateSub), sig, sig>());
			bops.push_back(bop<uns, operators::mul, BUILDER_FUNCTION(CreateMul), sig, sig>());
		});
		utils::integer_range_loop<0, signeds.size()>([&]<size_t Index>() {
			constexpr auto &sig = signeds[Index];
			bops.push_back(bop<sig, operators::b_shr, BUILDER_FUNCTION(CreateAShr)>());
			bops.push_back(bop<sig, operators::div, BUILDER_FUNCTION(CreateSDiv)>());
			bops.push_back(bop<sig, operators::mod, BUILDER_FUNCTION(CreateSRem)>());
		});
		utils::integer_range_loop<0, floats.size()>([&]<size_t Index>() {
			constexpr auto &flo = floats[Index];
			bops.push_back(bop<flo, operators::add, BUILDER_FUNCTION(CreateFAdd)>());
			bops.push_back(bop<flo, operators::sub, BUILDER_FUNCTION(CreateFSub)>());
			bops.push_back(bop<flo, operators::mul, BUILDER_FUNCTION(CreateFMul)>());
			bops.push_back(bop<flo, operators::div, BUILDER_FUNCTION(CreateFDiv)>());
			bops.push_back(bop<flo, operators::mod, BUILDER_FUNCTION(CreateFRem)>());
		});
		bops.push_back(
				bop<t_float, operators::pow,
					[](LlvmVisitor &visitor, llvm::Value *left, llvm::Value *right) {
						const auto ty = llvm::Type::getFloatTy(visitor.llvm_context);
						return static_cast<llvm::Value *>(visitor.builder->CreateCall(
								visitor.mod->getOrInsertFunction("llvm.pow.f32", ty, ty, ty),
								{left, right}));
					}>());
		bops.push_back(
				bop<t_double, operators::pow,
					[](LlvmVisitor &visitor, llvm::Value *left, llvm::Value *right) {
						const auto ty = llvm::Type::getDoubleTy(visitor.llvm_context);
						return static_cast<llvm::Value *>(visitor.builder->CreateCall(
								visitor.mod->getOrInsertFunction("llvm.pow.f64", ty, ty, ty),
								{left, right}));
					}>());
		return bops;
	}

	std::vector<std::pair<operation::Binary, bop_generator>> binary_operations{
			std::move(builtin_binary_operations())};

	Value assignment(const ast::expr::Binop &expr) {
		const auto lvalue = visit(expr.left);
		const auto rvalue = deref(visit(expr.right));
		if (!lvalue.type.is_ref)
			throw std::runtime_error(
					fmt("Left side of assignment is not an lvalue ", expr.location));
		if (lvalue.type.is_const)
			throw std::runtime_error(
					fmt("Left side of assignment is constant ", expr.location));
		builder->CreateStore(rvalue.value, lvalue.value);
		return lvalue;
	}
	Value logical(const ast::expr::Binop &expr) {
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
		auto phi = builder->CreatePHI(llvm_type(type::boolean), 2);
		phi->addIncoming(left.value, left_bb);
		phi->addIncoming(right.value, right_bb);
		return Value{type::boolean, phi};
	}
	Value visit(const ast::expr::Binop &expr) {
		if (expr.op == operators::assign)
			return assignment(expr);
		if (expr.op == operators::l_and || expr.op == operators::l_or)
			return logical(expr);
		auto lvalue = visit(expr.left);
		auto rvalue = visit(expr.right);
		for (auto &[op, gen] : binary_operations) {
			if (op.op == expr.op && op.left == lvalue.type &&
				op.right == rvalue.type) {
				return gen(*this, lvalue, rvalue);
			}
		}
		std::string_view op_str = string(expr.op);
		auto types = std::array{lvalue.type, rvalue.type};
		if (auto function = all_function_container.get_function(this, op_str, {}, types)) {
			auto call = builder->CreateCall(llvm::cast<llvm::FunctionType>(llvm_type(function->type)),
											function->value, std::array{lvalue.value, rvalue.value});
			return Value{
					*function->type.return_type,
					call};
		}
		lvalue = deref(lvalue);
		rvalue = deref(rvalue);
		for (auto &[op, gen] : binary_operations) {
			if (op.op == expr.op && op.left == lvalue.type &&
				op.right == rvalue.type) {
				return gen(*this, lvalue, rvalue);
			}
		}
		types = std::array{lvalue.type, rvalue.type};
		if (auto function = all_function_container.get_function(this, op_str, {}, types)) {
			auto call = builder->CreateCall(llvm::cast<llvm::FunctionType>(llvm_type(function->type)),
											function->value, std::array{lvalue.value, rvalue.value});
			return Value{
					*function->type.return_type,
					call};
		}
		throw std::runtime_error(
				fmt("No matching binary operation ", expr.location));
	}

	template<const type::Type &left, const operators::comparison op, llvm::Value *(*Lambda)(LlvmVisitor &, llvm::Value *, llvm::Value *), const type::Type &right = left, const type::Type &result = left>
	static std::pair<operation::Comparison, bop_generator>
	cop() {
		return std::make_pair<operation::Comparison, bop_generator>(
				operation::Comparison{left, right, op},
				static_cast<bop_generator>(+[](LlvmVisitor &visitor, const Value &lhs, const Value &rhs) -> Value {
					return Value{result, Lambda(visitor, visitor.deref(lhs).value, visitor.deref(rhs).value)};
				}));
	}
	static std::vector<std::pair<operation::Comparison, bop_generator>>
	builtin_comparison_operations() {
		std::vector<std::pair<operation::Comparison, bop_generator>> cops{};
		using namespace constexpr_primitives;
		utils::integer_range_loop<0, unsigneds.size()>([&]<size_t Index>() {
			constexpr auto &uns = unsigneds[Index];
			cops.push_back(cop<uns, operators::less, BUILDER_FUNCTION(CreateICmpULT)>());
			cops.push_back(cop<uns, operators::less_eq, BUILDER_FUNCTION(CreateICmpULE)>());
			cops.push_back(cop<uns, operators::greater, BUILDER_FUNCTION(CreateICmpUGT)>());
			cops.push_back(cop<uns, operators::greater_eq, BUILDER_FUNCTION(CreateICmpUGE)>());
			cops.push_back(cop<uns, operators::eq_eq, BUILDER_FUNCTION(CreateICmpEQ)>());
			cops.push_back(cop<uns, operators::not_equal, BUILDER_FUNCTION(CreateICmpNE)>());
		});
		utils::integer_range_loop<0, signeds.size()>([&]<size_t Index>() {
			constexpr auto &sig = signeds[Index];
			cops.push_back(cop<sig, operators::less, BUILDER_FUNCTION(CreateICmpSLT)>());
			cops.push_back(cop<sig, operators::less_eq, BUILDER_FUNCTION(CreateICmpSLE)>());
			cops.push_back(cop<sig, operators::greater, BUILDER_FUNCTION(CreateICmpSGT)>());
			cops.push_back(cop<sig, operators::greater_eq, BUILDER_FUNCTION(CreateICmpSGE)>());
			cops.push_back(cop<sig, operators::eq_eq, BUILDER_FUNCTION(CreateICmpEQ)>());
			cops.push_back(cop<sig, operators::not_equal, BUILDER_FUNCTION(CreateICmpNE)>());
		});
		utils::integer_range_loop<0, floats.size()>([&]<size_t Index>() {
			constexpr auto &flo = floats[Index];
			cops.push_back(cop<flo, operators::less, BUILDER_FUNCTION(CreateFCmpOLT)>());
			cops.push_back(cop<flo, operators::less_eq, BUILDER_FUNCTION(CreateFCmpOLE)>());
			cops.push_back(cop<flo, operators::greater, BUILDER_FUNCTION(CreateFCmpOGT)>());
			cops.push_back(cop<flo, operators::greater_eq, BUILDER_FUNCTION(CreateFCmpOGE)>());
			cops.push_back(cop<flo, operators::eq_eq, BUILDER_FUNCTION(CreateFCmpOEQ)>());
			cops.push_back(cop<flo, operators::not_equal, BUILDER_FUNCTION(CreateFCmpONE)>());
		});
		return cops;
	}
	std::vector<std::pair<operation::Comparison, bop_generator>>
			comparison_operations{std::move(builtin_comparison_operations())};
	Value visit(const ast::expr::Comparison &expr) {
		auto lvalue = visit(&expr.operands[0]);
		const auto start_block = builder->GetInsertBlock();
		const auto end_block = create_block();
		builder->SetInsertPoint(end_block);
		const auto result_type = type::t_boolean;
		const auto phi = builder->CreatePHI(llvm_type(result_type), 2);
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

	template<const type::Type &type, const operators::unary op, llvm::Value *(*Lambda)(LlvmVisitor &visitor, llvm::Value *operand), const type::Type &result = type>
	std::pair<operation::Unary, unary_generator> static uop() {
		return std::make_pair<operation::Unary, unary_generator>(
				operation::Unary{type, op},
				static_cast<unary_generator>(+[](LlvmVisitor &visitor, const Value &operand) -> Value {
					return Value{type, Lambda(visitor, visitor.deref(operand).value)};
				}));
	}
#undef BUILDER_FUNCTION
#define BUILDER_FUNCTION(name) \
	[](LlvmVisitor &visitor, llvm::Value *operand) { return visitor.builder->name(operand); }

	static std::vector<std::pair<operation::Unary, unary_generator>>
	builtin_unary_operations() {
		std::vector<std::pair<operation::Unary, unary_generator>> ops{};
		using namespace constexpr_primitives;
		utils::integer_range_loop<0, numeric.size()>([&]<size_t Index>() {
			constexpr auto &num = numeric[Index];
			ops.push_back(uop<num, operators::pos,
							  [](LlvmVisitor &, llvm::Value *operand) { return operand; }>());
		});
		utils::integer_range_loop<0, unsigneds.size()>([&]<size_t Index>() {
			constexpr auto &uns = unsigneds[Index];
			ops.push_back(uop<uns, operators::b_not, BUILDER_FUNCTION(CreateNot)>());
			ops.push_back(std::make_pair<operation::Unary, unary_generator>(
					operation::Unary{uns, operators::neg},
					static_cast<unary_generator>([](LlvmVisitor &visitor, const Value &operand) -> Value {
						return Value{signeds[Index],
									 visitor.builder->CreateNeg(visitor.deref(operand).value)};
					})));
		});
		utils::integer_range_loop<0, signeds.size()>([&]<size_t Index>() {
			constexpr auto &sig = signeds[Index];
			ops.push_back(uop<sig, operators::neg, BUILDER_FUNCTION(CreateNeg)>());
		});
		utils::integer_range_loop<0, floats.size()>([&]<size_t Index>() {
			constexpr auto &flo = floats[Index];
			ops.push_back(uop<flo, operators::neg, BUILDER_FUNCTION(CreateFNeg)>());
		});
		ops.push_back(
				uop<t_boolean, operators::l_not, BUILDER_FUNCTION(CreateNot)>());
		return ops;
	}
	std::vector<std::pair<operation::Unary, unary_generator>> unary_operations{
			std::move(builtin_unary_operations())};
	Value visit(const ast::expr::Unary &expr) {
		auto operand = visit(expr.expr);
		for (auto &[op, gen] : unary_operations) {
			if (op.op == expr.op && op.operand == operand.type) {
				return gen(*this, operand);
			}
		}
		throw std::runtime_error(
				fmt("No matching unary operation ", expr.location));
	}

	Value visit(const ast::expr::Conditional &expr) {
		const auto true_block = create_block("true");
		const auto false_block = create_block("false");
		const auto end_block = create_block();
		const auto condition = visit(expr.condition);
		builder->CreateCondBr(condition.value, true_block, false_block);
		builder->SetInsertPoint(true_block);
		const auto true_value = visit(expr.thenExpr);
		const auto true_end_block = builder->GetInsertBlock();
		builder->SetInsertPoint(end_block);
		const auto phi = builder->CreatePHI(llvm_type(true_value.type), 2);
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

	Value value_call(const ast::expr::Call &expr) {
		auto function = visit(expr.callee);
		if (function.type.is<type::Pointer>() &&
			function.type.get<type::Pointer>().pointed->is<type::Function>())
			function =
					deref(function); // make sure it's a ptr-fn and not a ref-ptr-fn
		else if (function.type.is<type::Function>() && function.type.is_ref) {
			function.type.is_ref = false;
			function.type =
					type::Pointer{std::make_unique<type::Type>(function.type)};
		} else
			throw std::runtime_error(
					fmt("Not a function ", expr.location,
						", expected function pointer or reference to function."));
		auto &funtype = function.type.get<type::Function>();
		if (funtype.parameters.size() != expr.arguments.size())
			throw std::runtime_error(
					fmt("Wrong number of arguments ", expr.location));
		std::vector<llvm::Value *> args;
		for (const auto &[param, arg] :
			 std::views::zip(funtype.parameters, expr.arguments)) {
			auto value = visit(&arg);
			if (value.type != param)
				throw std::runtime_error(fmt("Argument type mismatch ", expr.location));
			args.push_back(value.value);
		}
		return Value{
				*funtype.return_type,
				builder->CreateCall(llvm::cast<llvm::FunctionType>(llvm_type(funtype)),
									function.value, args)};
	}

	Value name_call(const ast::expr::Call &expr) {
		const auto id = expr.callee->get<ast::expr::Identifier>();
		std::vector<Value> args;
		for (const auto &arg : expr.arguments)
			args.push_back(visit(&arg));
		std::vector<type::Type> type_args;
		for (const auto &arg : id.type_arguments)
			type_args.push_back(visit(&arg));
		std::vector<type::Type> arg_types;
		for (const auto &arg : args)
			arg_types.push_back(arg.type);
		auto function = locals.ns->function(this, id.value, type_args, arg_types);
		if (!function)
			throw std::runtime_error(fmt("Unknown function ", id.value,
										 " for the supplied arguments at ",
										 id.location));
		std::vector<llvm::Value *> llvm_args;
		for (const auto &arg : args)
			llvm_args.push_back(arg.value);
		auto call = builder->CreateCall(llvm::cast<llvm::FunctionType>(llvm_type(function->type)),
										function->value, llvm_args);
		return Value{
				*function->type.return_type,
				call};
	}

	Value visit(const ast::expr::Call &expr) {
		if (expr.callee->is<ast::expr::Identifier>()) {
			return name_call(expr);
		} else {
			return value_call(expr);
		}
	}

	Value visit(const ast::expr::MemberCall &expr) {
		std::vector args{visit(expr.callee)};
		for (const auto &arg : expr.arguments)
			args.push_back(visit(&arg));
		std::vector<type::Type> type_args;
		for (const auto &arg : expr.type_arguments)
			type_args.push_back(visit(&arg));
		std::vector<type::Type> arg_types;
		for (const auto &arg : args)
			arg_types.push_back(arg.type);
		auto function = all_function_container.get_function(this, expr.name, type_args, arg_types);
		if (!function)
			throw std::runtime_error(fmt("Unknown function ", expr.name,
										 " for the supplied arguments at ",
										 expr.location));
		std::vector<llvm::Value *> llvm_args;
		for (const auto &arg : args)
			llvm_args.push_back(arg.value);
		auto call = builder->CreateCall(llvm::cast<llvm::FunctionType>(llvm_type(function->type)),
										function->value, llvm_args);
		return Value{
				*function->type.return_type,
				call};
	}

	Value visit(const ast::expr::Create &expr) {
		auto type = visit(expr.type);
		if (!type.is<type::NamedStruct>())
			throw std::runtime_error(
					fmt("Cannot create non-struct types (", expr.location, ")"));
		const auto &named = type.get<type::NamedStruct>();
		if (expr.args.size() != named.members.size())
			throw std::runtime_error(
					fmt("Wrong number of members for struct ", expr.location));
		auto alloca = builder->CreateAlloca(llvm_type(type), nullptr, "alloca");
		auto struct_ = llvm::cast<llvm::StructType>(llvm_type(type));
		for (const auto &[sindex, member] :
			 std::views::enumerate(type.get<type::NamedStruct>().members)) {
			const size_t index = sindex;
			const auto &[name, arg] = expr.args[index];
			// TODO: support name reordering
			if (!name.empty() && name != member.first)
				throw std::runtime_error(fmt("Member name mismatch: expected '",
											 member.first, "', got '", name, "' ",
											 expr.location));
			auto value = deref(visit(&arg));
			if (value.type != member.second)
				throw std::runtime_error(fmt("Member type mismatch ", expr.location));
			auto member_alloc = builder->CreateStructGEP(struct_, alloca, static_cast<uint32_t>(index));
			builder->CreateStore(value.value, member_alloc);
		}
		type.is_const = true;
		type.is_ref = true;
		return Value{type, alloca};
	}

	Value visit(const ast::expr::Subscript &expr) {
		// todo fix
		auto value = visit(expr.expr);
		auto index = deref(visit(expr.index));
		if (!index.type.is<type::Primitive>())
			throw std::runtime_error(fmt("Index must be a primitive type ", expr.location));
		auto prim = index.type.get<type::Primitive>();
		if (!prim.is_integral())
			throw std::runtime_error(fmt("Index must be an integer type ", expr.location));
		if (value.type.is<type::Pointer>()) {
			value = deref(value); // make sure it's a pointer and not a ref-to-ptr
			const auto &pointer_type = value.type.get<type::Pointer>();
			type::Type member_type = *pointer_type.pointed;
			member_type.is_ref = false;
			member_type.is_const = value.type.is_const;
			auto gep = builder->CreateGEP(llvm_type(member_type), value.value, index.value);
			member_type.is_ref = true;
			return Value{member_type, gep};
		}
		if (value.type.is<type::Array>()) {
			value = mkref(value);
			const auto &array_type = value.type.get<type::Array>();
			type::Type member_type = *array_type.member;
			member_type.is_ref = true;
			member_type.is_const = value.type.is_const;
			auto gep = builder->CreateGEP(llvm_type(array_type), value.value,
										  {llvm::ConstantInt::get(llvm_type(index.type), 0), index.value});
			return Value{member_type, gep};
		}
		throw std::runtime_error(
				fmt("Cannot subscript non-array/pointer types ", expr.location));
	}

	Value visit(const ast::expr::Array &expr) {
		if (expr.values.empty())
			throw std::runtime_error("Empty array");
		auto first = deref(visit(&expr.values.front()));
		auto member_type = first.type;
		type::Type array_type{type::Array{std::make_unique<type::Type>(member_type),
										  expr.values.size()},
							  false, true};
		auto non_ref_array_type = array_type;
		non_ref_array_type.is_ref = false;
		auto alloca = builder->CreateAlloca(llvm_type(array_type), nullptr, "alloca");
		auto llvm_array = llvm_type(non_ref_array_type);
		builder->CreateStore(first.value, builder->CreateStructGEP(llvm_array, alloca, 0));
		for (const auto &[index, value] :
			 std::views::enumerate(expr.values | std::views::drop(1))) {
			auto value_ = deref(visit(&value));
			if (value_.type != member_type)
				throw std::runtime_error(fmt("Array member type mismatch ",
											 expr.location));
			auto member_ptr = builder->CreateStructGEP(llvm_array, alloca, static_cast<uint32_t>(index + 1));
			builder->CreateStore(value_.value, member_ptr);
		}
		return Value{array_type, alloca};
	}

	Value visit(const ast::expr::Member &expr) {
		auto value = visit(expr.expr);
		if (!value.type.is<type::NamedStruct>())
			throw std::runtime_error(fmt(
					"Cannot access members of non-struct types (", expr.location, ")"));
		value = mkref(value); // make sure it's a reference
		const auto &named = value.type.get<type::NamedStruct>();
		for (const auto &[index, member] : std::views::enumerate(named.members)) {
			const auto &[name, type] = member;
			if (expr.name == name) {
				auto member_type = type;
				member_type.is_ref = true;
				member_type.is_const = value.type.is_const;
				auto unref_value_type = value.type;
				unref_value_type.is_ref = false;
				return Value{member_type,
							 builder->CreateStructGEP(llvm_type(unref_value_type), value.value, static_cast<uint32_t>(index))};
			}
		}
		throw std::runtime_error(fmt("Unknown member '", expr.name, "' in struct '",
									 named.name, "' ", expr.location));
	}

	const Value &visit(const ast::expr::Identifier &expr) {
		if (!expr.type_arguments.empty())
			throw std::runtime_error(fmt("Type arguments are not supported for bare identifiers ", expr.location));
		const auto value = locals.value(expr.value);
		if (value == nullptr)
			throw std::runtime_error(
					fmt("Unknown identifier: ", expr.value, ' ', expr.location));
		return *value;
	}

	Value visit(const ast::expr::Number &expr) {
		return expr.value.visit(
				[&](const ast::expr::Number::uint_t uinty) -> Value {
					return Value{type::t_int32,
								 llvm::ConstantInt::get(
										 llvm::Type::getInt32Ty(llvm_context), uinty)};
				},
				[&](const ast::expr::Number::float_t floaty) -> Value {
					return Value{type::t_double,
								 llvm::ConstantFP::get(
										 llvm::Type::getDoubleTy(llvm_context), floaty)};
				});
	}
};
} // namespace

void backend::llvm::generate_object_file(const ast::Program &program,
										 const std::string_view to) {
	auto context = ::llvm::LLVMContext{};
	LlvmVisitor visitor{context, std::make_unique<::llvm::Module>("main", context)};
	visitor.mod->setSourceFileName(program.location.file);
	visitor.visit(program);
	std::error_code ec;
	::llvm::raw_fd_stream file(to, ec);
	visitor.mod->print(file, nullptr, false, true);
}