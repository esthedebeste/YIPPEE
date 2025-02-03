module;
#include <algorithm>
#include <coroutine>
#include <functional>
#include <generator.hpp>
#include <memory>
#include <optional>
#include <ranges>
#include <span>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>
export module backend.base;
import ast;
import utils;
import type;
import operators;
import operations;
import naming;

namespace {
template<class T>
bool operator==(const std::vector<T> &vector, std::span<T> span) {
	if (vector.size() != span.size())
		return false;
	for (std::size_t i = 0; i < vector.size(); ++i)
		if (vector[i] != span[i])
			return false;
	return true;
}
} // namespace

export namespace backend {
template<class Child, class UnderlyingValue, class FunctionValue>
struct Base {
	Base() {
		root_namespace.children.emplace("C", Namespace{Ambiguity::NoDuplicateName, &root_namespace, "C"});
	}
	virtual ~Base() = default;

	bool type_pass = true;

	struct Value {
		type::Type type;
		UnderlyingValue value;
	};
	struct TopLevelFunction;
	virtual FunctionValue generate_function_value(const TopLevelFunction &tlf, const type::Function &type, std::string mangled_name) = 0;
	struct LocalScope;
	struct Namespace;
	struct NewLocalScopeHere {
		LocalScope old_locals;
		LocalScope *locals_ref;
		NewLocalScopeHere(Namespace *ns, LocalScope *locals) : old_locals(ns), locals_ref{locals} {
			std::swap(*locals_ref, old_locals);
			locals_ref->emplace_back();
		}
		~NewLocalScopeHere() {
			locals_ref->pop_back();
			std::swap(*locals_ref, old_locals);
		}
	};
	struct NewScopeHere {
		LocalScope *local_scope;
		explicit NewScopeHere(LocalScope *locals) : local_scope{locals} {
			local_scope->emplace_back();
		}
		~NewScopeHere() {
			local_scope->pop_back();
		}
	};
	struct TopLevelFunction {
		Namespace *ns;
		const ast::top::Function *ast;
		// returns std::nullopt if invalid
		std::optional<type::Function> type(Base *base, std::span<type::Type> type_parameters) {
			// when generating the template, return
			// back to namespace of definition
			NewLocalScopeHere nsh(ns, &base->locals);
			std::vector<type::Type> type_arguments{};
			for (const auto &[sindex, argument] : std::views::enumerate(ast->type_arguments)) {
				const size_t index = sindex;
				std::optional<type::Type> type;
				if (index < type_parameters.size())
					type = type_parameters[index];
				else if (argument.default_type)
					type = base->visit(*argument.default_type);
				if (!type)
					return std::nullopt;
				type::Type argtype = std::move(*type);
				base->locals.back().types.emplace(argument.name,
												  ConstantType{.name = argument.name, .type = argtype});
				type_arguments.push_back(argtype);
			}
			std::optional<type::Function> result{};
			try {
				std::vector<type::Type> args;
				for (const ast::TypeAst &type : ast->parameters | std::views::values) {
					auto param_type = base->visit(&type);
					if (!param_type.is_ref)
						param_type.is_const = true;
					args.push_back(std::move(param_type));
				}
				const type::Type return_type = base->visit(ast->return_type);
				result = type::Function{args, return_type};
			} catch (std::runtime_error &) {
				result = std::nullopt;
			}
			return result;
		}
		FunctionValue value(Base *base, std::span<type::Type> type_parameters) {
			// when generating the template, return
			// back to namespace of definition
			NewLocalScopeHere nsh(ns, &base->locals);
			std::vector<type::Type> type_arguments{};
			for (const auto &[sindex, argument] : std::views::enumerate(ast->type_arguments)) {
				const size_t index = sindex;
				std::optional<type::Type> type;
				if (index < type_parameters.size())
					type = type_parameters[index];
				else if (argument.default_type)
					type = base->visit(*argument.default_type);
				if (!type) {
					throw std::runtime_error(fmt("Missing type argument for function ", ast->location));
				}
				type::Type argtype = std::move(*type);
				base->locals.back().types.emplace(
						argument.name, ConstantType{.name = argument.name, .type = argtype});
				type_arguments.push_back(argtype);
			}
			std::vector<type::Type> args;
			for (const auto &type : ast->parameters | std::views::values) {
				auto param_type = base->visit(&type);
				if (!param_type.is_ref)
					param_type.is_const = true;
				args.push_back(std::move(param_type));
			}
			const type::Type return_type = base->visit(ast->return_type);
			const type::Function type{args, return_type};
			std::string mangled_name;
			if (ns->name == "C" && ns->parent == &base->root_namespace) {
				// namespace ::C
				mangled_name = ast->name.final.str;
			} else {
				const naming::FullName name{ns->path(), ast->name.final.str};
				mangled_name = name.mangle();
				mangled_name += type.mangle();
			}
			return base->generate_function_value(*this, type, std::move(mangled_name));
		}
	};
	struct GetFunctionResult {
		TopLevelFunction *tlf;
		type::Function type;
		FunctionValue value;
	};
	struct ConstantType {
		std::string_view name;
		type::Type type;
	};
	type::Type get_type(const ConstantType &constant_type, const std::span<type::Type> arguments) {
		if (!arguments.empty())
			throw std::runtime_error(fmt("Type arguments for type '", constant_type.name, "' are not allowed"));
		return constant_type.type;
	}
	struct GenericStruct {
		Namespace *definition_ns;
		const ast::top::Struct &ast;
		naming::FullName name;
	};
	utils::string_map<type::Type> named_struct_types;
	type::Type get_type(const GenericStruct &generic_struct, std::span<type::Type> arguments) {
		// when generating the template, return back to namespace of definition
		NewLocalScopeHere nsh(generic_struct.definition_ns, &locals);
		std::vector<type::Type> type_arguments{};
		for (const auto &[sindex, argument] : std::views::enumerate(generic_struct.ast.type_arguments)) {
			const size_t index = sindex;
			std::optional<type::Type> type;
			if (index < arguments.size())
				type = arguments[index];
			else if (argument.default_type)
				type = visit(*argument.default_type);
			if (!type)
				throw std::runtime_error(
						fmt("Missing type argument for struct ", argument.name, ". ", generic_struct.ast.location));
			type::Type argtype = std::move(*type);
			locals.back().types.emplace(
					argument.name,
					ConstantType{.name = argument.name, .type = argtype});
			type_arguments.push_back(argtype);
		}
		type::NamedStruct init{generic_struct.name, type_arguments, {}};
		std::string name = init.mangle();
		auto [it, added] = named_struct_types.try_emplace(name, std::move(init));
		auto &result = it->second.get<type::NamedStruct>();
		if (added)
			for (const auto &[name, type] : generic_struct.ast.members)
				result.members.emplace_back(name, visit(&type));
		return result;
	}
	using TypeTemplate = utils::variant<ConstantType, GenericStruct>;
	type::Type get_type(const TypeTemplate &type_template, std::span<type::Type> arguments) {
		return type_template.visit([&](const auto &t) { return get_type(t, arguments); });
	}
	enum struct Ambiguity : uint8_t {
		Allowed,
		NoDuplicateSignature,
		NoDuplicateName,
	};
	struct FunctionContainer {
		Ambiguity ambiguity;
		explicit FunctionContainer(Ambiguity ambiguity)
			: ambiguity{ambiguity} {}
		std::optional<GetFunctionResult> get_function(Base *base, std::string_view name, std::span<type::Type> type_parameters, std::span<type::Type> parameters) {
			auto it = functions.find(name);
			if (it == functions.end())
				return std::nullopt;
			std::optional<GetFunctionResult> result;
			for (auto &func : it->second) {
				auto type = func.type(base, type_parameters);
				if (!type || !std::ranges::equal(type->parameters, parameters))
					continue;
				auto value = func.value(base, type_parameters);
				if (result && ambiguity != Ambiguity::Allowed)
					throw std::runtime_error(fmt("Ambiguous function call for function '", name, "'; ", func.ast->location, " and ", result->tlf->ast->location, " both valid for this call"));
				result = GetFunctionResult{&func, *type, value};
			}
			return result;
		}
		TopLevelFunction &emplace_function(Base *base, std::string_view string,
										   Namespace *ns,
										   const ast::top::Function *ast) {
			TopLevelFunction tlf{ns, ast};
			if (ambiguity == Ambiguity::NoDuplicateSignature && ast->type_arguments.empty()) {
				std::vector<type::Type> parameters;
				for (const auto &param : ast->parameters | std::views::values)
					parameters.push_back(base->visit(&param));
				if (auto func = get_function(base, string, {}, parameters))
					throw std::runtime_error(fmt("Function already exists with the same signature - ", ast->location, " and ", func->tlf->ast->location));
			}
			if (auto it = functions.find(string); it != functions.end()) {
				if (ambiguity == Ambiguity::NoDuplicateName && it->second.size() != 0)
					throw std::runtime_error(fmt("Function already exists with the same name - ", ast->location, " and ", it->second[0].ast->location));
				return it->second.emplace_back(tlf);
			}
			return functions
					.emplace(string, std::vector<TopLevelFunction>{})
					.first->second.emplace_back(tlf);
		}
		TopLevelFunction &emplace_function(Base *base, TopLevelFunction tlf) {
			const auto &string = tlf.ast->name.final.str;
			if (ambiguity == Ambiguity::NoDuplicateSignature && tlf.ast->type_arguments.empty()) {
				std::vector<type::Type> parameters;
				for (const auto &param : tlf.ast->parameters | std::views::values)
					parameters.push_back(base->visit(&param));
				if (auto func = get_function(base, string, {}, parameters))
					throw std::runtime_error(fmt("Function already exists with the same signature - ", tlf.ast->location, " and ", func->tlf->ast->location));
			}
			if (auto it = functions.find(string); it != functions.end()) {
				if (ambiguity == Ambiguity::NoDuplicateName && it->second.size() != 0)
					throw std::runtime_error(fmt("Function already exists with the same name - ", tlf.ast->location, " and ", it->second[0].ast->location));
				return it->second.emplace_back(tlf);
			}
			return functions
					.emplace(string, std::vector<TopLevelFunction>{})
					.first->second.emplace_back(tlf);
		}
		TopLevelFunction &get_function_by_ast(Base *base, const ast::top::Function &ast) {
			const auto &string = ast.name.final.str;
			auto &tlfs = functions.at(string);
			for (auto &tlf : tlfs)
				if (tlf.ast == &ast)
					return tlf;
			throw std::runtime_error("Failed to get function by ast");
		}

	private:
		std::unordered_map<std::string_view, std::vector<TopLevelFunction>> functions{};
	};

	struct Scope : FunctionContainer {
		using values_t = std::unordered_map<std::string_view, Value>;
		values_t values{};
		using types_t = std::unordered_map<std::string_view, TypeTemplate>;
		types_t types{};
		explicit Scope(Ambiguity ambiguity) : FunctionContainer{ambiguity} {}
		Scope() : FunctionContainer{Ambiguity::NoDuplicateSignature} {};
	};

	struct Namespace : Scope {
		Namespace *parent;
		std::string_view name;
		Namespace(Ambiguity ambiguity, Namespace *parent, std::string_view name) : Scope{ambiguity}, parent{parent}, name{name} {}
		std::unordered_map<std::string_view, Namespace> children;

		std::vector<std::string_view> path() {
			std::vector<std::string_view> result;
			for (Namespace *current = this; current != nullptr;
				 current = current->parent)
				result.push_back(current->name);
			std::ranges::reverse(result);
			return result;
		}
		std::generator<Namespace *> namespaces(const std::string_view name) {
			for (Namespace *current = this; current != nullptr;
				 current = current->parent)
				if (auto got = current->children.find(name);
					got != current->children.end())
					co_yield &got->second;
		}
		Namespace *namespace_(const std::string_view name) {
			auto gen = namespaces(name);
			for (auto ns : gen)
				return ns;
			return nullptr;
		}
		std::generator<Namespace *> namespaces(const ast::Identifier &name,
											   const bool final = true) {
			for (Namespace *current = this; current != nullptr;
				 current = current->parent) {
				Namespace *curr_res = current;
				bool nextresolve = false;
				for (const auto &part : name.parts) {
					auto got = curr_res->children.find(part.str);
					if (got == curr_res->children.end()) {
						nextresolve = true;
						break;
					}
					curr_res = &got->second;
				}
				if (nextresolve)
					continue;
				if (final) {
					auto got = curr_res->children.find(name.final.str);
					if (got == current->children.end())
						continue;
					curr_res = &got->second;
				}
				co_yield curr_res;
			}
		}
		Namespace *namespace_(const ast::Identifier &name, const bool final = true) {
			auto gen = namespaces(name, final);
			for (auto ns : gen)
				return ns;
			return nullptr;
		}
		const Value *value(const std::string_view name) {
			for (Namespace *current = this; current != nullptr;
				 current = current->parent)
				if (auto got = current->values.find(name); got != current->values.end())
					return &got->second;
			return nullptr;
		}
		const Value *value(const ast::Identifier &name) {
			if (name.parts.empty())
				return value(name.final.str);
			for (auto ns : namespaces(name, false))
				if (auto got = ns->values.find(name.final.str); got != ns->values.end())
					return &got->second;
			return nullptr;
		}
		const TypeTemplate *type(const std::string_view name) {
			for (Namespace *current = this; current != nullptr;
				 current = current->parent)
				if (auto got = current->types.find(name); got != current->types.end())
					return &got->second;
			return nullptr;
		}
		const TypeTemplate *type(const ast::Identifier &name) {
			if (name.parts.empty())
				return type(name.final.str);
			for (auto ns : namespaces(name, false))
				if (auto got = ns->types.find(name.final.str); got != ns->types.end())
					return &got->second;
			return nullptr;
		}
		std::optional<GetFunctionResult> function(Base *base, std::string_view name, std::span<type::Type> type_parameters, std::span<type::Type> parameters) {
			for (Namespace *current = this; current != nullptr;
				 current = current->parent)
				if (auto got = current->get_function(base, name, type_parameters, parameters))
					return got;
			return std::nullopt;
		}
		std::optional<GetFunctionResult> function(Base *base, const ast::Identifier &name, std::span<type::Type> type_parameters, std::span<type::Type> parameters) {
			if (name.parts.empty())
				return function(base, name.final.str, type_parameters, parameters);
			for (auto ns : namespaces(name, false))
				if (auto got = ns->get_function(base, name.final.str, type_parameters, parameters))
					return got;
			return std::nullopt;
		}
	};

	struct LocalScope : std::vector<Scope> {
		Namespace *ns;
		LocalScope(Namespace *ns) : ns{ns} {}
		const Value *value(const std::string_view name) {
			for (const auto &scope : *this | std::views::reverse)
				if (auto got = scope.values.find(name); got != scope.values.end())
					return &got->second;
			return ns->value(name);
		}
		const Value *value(const ast::Identifier &name) {
			if (name.parts.empty())
				return value(name.final.str);
			return ns->value(name);
		}
		const TypeTemplate *type(const std::string_view name) {
			for (const auto &scope : *this | std::views::reverse)
				if (auto got = scope.types.find(name); got != scope.types.end())
					return &got->second;
			return ns->type(name);
		}
		const TypeTemplate *type(const ast::Identifier &name) {
			if (name.parts.empty())
				return type(name.final.str);
			return ns->type(name);
		}
	};

	Namespace root_namespace{Ambiguity::NoDuplicateSignature, nullptr, "YIPPEE"};
	LocalScope locals{&root_namespace};
	FunctionContainer all_function_container{Ambiguity::Allowed};

	type::Function function_type(const ast::top::Function &function) {
		std::vector<type::Type> args;
		for (const auto &type : function.parameters | std::views::values)
			args.push_back(visit(&type));
		return type::Function{args, visit(function.return_type)};
	}

	void visit(const ast::Program &program) {
		NewScopeHere nsh{&locals};
		for (auto &top : program.tops)
			visit(&top);
	}

	void visit(const ast::top::Struct &ast) {
		const auto ns_ = locals.ns->namespace_(ast.name, false);
		naming::FullName name{ns_->path(), ast.name.final.str};

		if (type_pass) {
			if (ns_->types.contains(ast.name.final.str))
				throw std::runtime_error(fmt("Struct '", name, "' already defined"));

			ns_->types.emplace(
					name.final,
					GenericStruct{
							.definition_ns = ns_,
							.ast = ast,
							.name = name,
					});
		} else {
			auto res = ns_->types.at(name.final);
			if (ast.type_arguments.empty()) {
				// if it's not templated, check if the struct works out pre-reference by calling it,
				// and re-assign it to a cached version.
				const auto ns_ = locals.ns->namespace_(ast.name, false);
				naming::FullName name{ns_->path(), ast.name.final.str};

				auto &res = ns_->types.at(name.final);
				res = ConstantType{
						.name = name.final,
						.type = get_type(res, {})};
			}
		}
	}

	type::Type visit(const ast::TypePtr &type) { return visit(type.get()); }
	type::Type visit(const ast::TypeAst *type) {
		return type->visit(
				[](const std::monostate) -> type::Type {
					throw std::runtime_error("unexpected empty AST node");
				},
				[&](const auto &type) { return visit(type); });
	}

	type::Type visit(const ast::type::Primitive &primitive) {
		return primitive.prim;
	}
	type::Type visit(const ast::type::Named &named) {
		const auto template_ = locals.type(named.name);
		if (!template_)
			throw std::runtime_error(
					fmt("Unknown type '", named.name, "' ", named.location));
		std::vector<type::Type> arguments;
		for (const auto &param : named.arguments)
			arguments.push_back(visit(&param));
		return get_type(*template_, arguments);
	}
	type::Type visit(const ast::type::Pointer &pointer) {
		auto type = visit(pointer.pointed);
		return type::Pointer{std::make_unique<type::Type>(type)};
	}
	type::Type visit(const ast::type::Array &array) {
		auto member = visit(array.member);
		return type::Array{std::make_unique<type::Type>(member), array.size};
	}
	type::Type visit(const ast::type::Constant &constant) {
		auto constanted = visit(constant.constanted);
		constanted.is_const = true;
		return constanted;
	}
	[[noreturn]] void visit(const std::monostate) {
		throw std::runtime_error("unexpected empty AST node");
	}
	void visit(const ast::TopLevelPtr &top) { visit(top.get()); }
	void visit(const ast::TopLevelAst *top) {
		top->visit([&](const auto &top) { visit(top); });
	}
	void visit(const ast::top::Function &function) {
		auto ns = locals.ns->namespace_(function.name, false);
		if (type_pass) {
			auto &tlf = ns->emplace_function(this, function.name.final.str, ns, &function);
			all_function_container.emplace_function(this, tlf);
		} else {
			auto &tlf = ns->get_function_by_ast(this, function);
			if (function.type_arguments.empty())
				tlf.value(this, {}); // always compile non-templated functions
		}
	}
	void visit(const ast::top::Namespace &ast) {
		Namespace *ns = locals.ns;
		// make every part of the namespace
		for (auto &part : ast.name.parts) {
			Namespace new_ns{Ambiguity::NoDuplicateSignature, ns, part.str};
			ns = &ns->children.try_emplace(new_ns.name, std::move(new_ns))
						  .first->second;
		}
		Namespace _new_ns{Ambiguity::NoDuplicateSignature, ns, ast.name.final.str};
		ns = &ns->children.try_emplace(_new_ns.name, std::move(_new_ns))
					  .first->second;
		NewLocalScopeHere nlsh(ns, &locals);
		for (auto &top : ast.tops)
			visit(&top);
	}
	void visit(const ast::StatementPtr &statement) { visit(statement.get()); }
	void visit(const ast::StatementAst *statement) {
		statement->visit(
				[&](const auto &statement) { visit(statement); });
	}
	void visit(const ast::stmt::Block &statement) {
		NewScopeHere nsh{&locals};
		for (const auto &stmt : statement.statements)
			visit(&stmt);
	}
	virtual void impl_visit(const ast::stmt::If &statement) = 0;
	void visit(const ast::stmt::If &statement) {
		return impl_visit(statement);
	}
	virtual void impl_visit(const ast::stmt::For &statement) = 0;
	void visit(const ast::stmt::For &statement) {
		NewScopeHere nsh{&locals}; // init variable is local
		impl_visit(statement);
	}
	virtual void impl_visit(const ast::stmt::While &statement) = 0;
	void visit(const ast::stmt::While &statement) {
		impl_visit(statement);
	}
	virtual void impl_return(Value value) = 0;
	void visit(const ast::stmt::Return &statement) {
		const auto value = deref(visit(statement.expr));
		impl_return(value);
	}
	virtual void impl_expr_stmt(Value value, const ast::stmt::Expr &statement) = 0;
	void visit(const ast::stmt::Expr &statement) {
		auto value = visit(statement.expr);
		impl_expr_stmt(value, statement);
	}
	void visit(const ast::stmt::Variable &statement) {
		auto &values = locals.back().values;
		if (values.contains(statement.name))
			throw std::runtime_error(fmt("Variable '", statement.name,
										 "' already defined ", statement.location));
		auto value = mkref(deref(visit(statement.expr)));
		value.type = statement.type ? visit(*statement.type) : value.type;
		value.type.is_ref = true;
		value.type.is_const = statement.is_const;
		values.emplace(statement.name, value);
	}

	Value visit(const ast::ExprPtr &expr) { return visit(expr.get()); }
	Value visit(const ast::ExprAst *expr) {
		return expr->visit(
				[&](const auto &expr) -> Value { return visit(expr); });
	}

	/// Load the reference value `ref`, such that it is of type `when_loaded`
	virtual UnderlyingValue impl_deref(type::Type when_loaded, UnderlyingValue ref) = 0;
	/// If value is NOT a reference, returns it.
	/// Otherwise, load it and returns the value.
	Value deref(Value value) {
		if (!value.type.is_ref)
			return value;
		auto loaded_type = value.type; // copy
		loaded_type.is_const = false;
		loaded_type.is_ref = false;
		return Value{loaded_type, impl_deref(loaded_type, value.value)};
	}
	/// Store the value on the stack, return a pointer to that value.
	virtual UnderlyingValue impl_mkref(Value value) = 0;
	/// If value is a reference, returns it.
	/// Otherwise, `alloca`s and returns a const T&.
	Value mkref(Value value) {
		if (value.type.is_ref)
			return value;
		auto ref_type = value.type; // copy
		ref_type.is_const = true;
		ref_type.is_ref = true;
		return Value{ref_type, impl_mkref(value)};
	}

	using unary_generator = Value (*)(Child &, const Value &);
	std::vector<std::pair<operation::Unary, unary_generator>> unary_operations{
			std::move(builtin_unary_operations())};
	template<const type::Type &op_t, const operators::unary op, UnderlyingValue (Child::*Lambda)(UnderlyingValue operand), const type::Type &result = op_t>
	std::pair<operation::Unary, unary_generator> static uop() {
		return std::make_pair<operation::Unary, unary_generator>(
				operation::Unary{op_t, op},
				static_cast<unary_generator>(+[](Child &visitor, const Value &operand) -> Value {
					return Value{result, (visitor.*Lambda)(visitor.deref(operand).value)};
				}));
	}

	virtual UnderlyingValue unary_b_not_u8(UnderlyingValue operand) = 0;
	virtual UnderlyingValue unary_b_not_u16(UnderlyingValue operand) = 0;
	virtual UnderlyingValue unary_b_not_u32(UnderlyingValue operand) = 0;
	virtual UnderlyingValue unary_b_not_u64(UnderlyingValue operand) = 0;
	virtual UnderlyingValue unary_b_not_u128(UnderlyingValue operand) = 0;

	virtual UnderlyingValue unary_neg_u8(UnderlyingValue operand) = 0;
	virtual UnderlyingValue unary_neg_u16(UnderlyingValue operand) = 0;
	virtual UnderlyingValue unary_neg_u32(UnderlyingValue operand) = 0;
	virtual UnderlyingValue unary_neg_u64(UnderlyingValue operand) = 0;
	virtual UnderlyingValue unary_neg_u128(UnderlyingValue operand) = 0;

	virtual UnderlyingValue unary_neg_i8(UnderlyingValue operand) = 0;
	virtual UnderlyingValue unary_neg_i16(UnderlyingValue operand) = 0;
	virtual UnderlyingValue unary_neg_i32(UnderlyingValue operand) = 0;
	virtual UnderlyingValue unary_neg_i64(UnderlyingValue operand) = 0;
	virtual UnderlyingValue unary_neg_i128(UnderlyingValue operand) = 0;

	virtual UnderlyingValue unary_neg_half(UnderlyingValue operand) = 0;
	virtual UnderlyingValue unary_neg_float(UnderlyingValue operand) = 0;
	virtual UnderlyingValue unary_neg_double(UnderlyingValue operand) = 0;

	virtual UnderlyingValue unary_l_not_boolean(UnderlyingValue operand) = 0;

	std::vector<std::pair<operation::Unary, unary_generator>>
	builtin_unary_operations() {
		std::vector<std::pair<operation::Unary, unary_generator>> ops{};
		utils::integer_range_loop<0, type::numeric.size()>([&]<size_t Index>() {
			constexpr auto &num = type::numeric[Index];
			ops.emplace_back(operation::Unary{num, operators::pos},
							 static_cast<unary_generator>(+[](Child &visitor, const Value &operand) -> Value {
								 return operand;
							 }));
		});
		ops.push_back(uop<type::t_uint8, operators::b_not, &Child::unary_b_not_u8>());
		ops.push_back(uop<type::t_uint16, operators::b_not, &Child::unary_b_not_u16>());
		ops.push_back(uop<type::t_uint32, operators::b_not, &Child::unary_b_not_u32>());
		ops.push_back(uop<type::t_uint64, operators::b_not, &Child::unary_b_not_u64>());
		ops.push_back(uop<type::t_uint128, operators::b_not, &Child::unary_b_not_u128>());

		ops.push_back(uop<type::t_uint8, operators::neg, &Child::unary_neg_u8, type::t_int8>());
		ops.push_back(uop<type::t_uint16, operators::neg, &Child::unary_neg_u16, type::t_int16>());
		ops.push_back(uop<type::t_uint32, operators::neg, &Child::unary_neg_u32, type::t_int32>());
		ops.push_back(uop<type::t_uint64, operators::neg, &Child::unary_neg_u64, type::t_int64>());
		ops.push_back(uop<type::t_uint128, operators::neg, &Child::unary_neg_u128, type::t_int128>());

		ops.push_back(uop<type::t_int8, operators::neg, &Child::unary_neg_i8>());
		ops.push_back(uop<type::t_int16, operators::neg, &Child::unary_neg_i16>());
		ops.push_back(uop<type::t_int32, operators::neg, &Child::unary_neg_i32>());
		ops.push_back(uop<type::t_int64, operators::neg, &Child::unary_neg_i64>());
		ops.push_back(uop<type::t_int128, operators::neg, &Child::unary_neg_i128>());

		ops.push_back(uop<type::t_half, operators::neg, &Child::unary_neg_half>());
		ops.push_back(uop<type::t_float, operators::neg, &Child::unary_neg_float>());
		ops.push_back(uop<type::t_double, operators::neg, &Child::unary_neg_double>());

		ops.push_back(uop<type::t_boolean, operators::l_not, &Child::unary_l_not_boolean>());
		return ops;
	}
	using bop_generator = Value (*)(Child &, const Value &, const Value &);
	std::vector<std::pair<operation::Binary, bop_generator>> binary_operations{
			std::move(builtin_binary_operations())};
	template<const type::Type &left, const operators::binary op, UnderlyingValue (Child::*Lambda)(UnderlyingValue lhs, UnderlyingValue rhs), const type::Type &right = left, const type::Type &result = left>
	static std::pair<operation::Binary, bop_generator>
	bop() {
		return std::make_pair<operation::Binary, bop_generator>(
				operation::Binary{left, right, op},
				static_cast<bop_generator>(+[](Child &visitor, const Value &lhs, const Value &rhs) -> Value {
					return Value{result, (visitor.*Lambda)(visitor.deref(lhs).value, visitor.deref(rhs).value)};
				}));
	}

	// Section: unsigned-unsigned built-in binary ops

	virtual UnderlyingValue binary_b_and_u8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_and_u16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_and_u32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_and_u64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_and_u128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue binary_b_or_u8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_or_u16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_or_u32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_or_u64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_or_u128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue binary_b_xor_u8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_xor_u16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_xor_u32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_xor_u64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_xor_u128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue binary_b_shl_u8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_shl_u16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_shl_u32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_shl_u64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_shl_u128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue binary_b_shr_u8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_shr_u16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_shr_u32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_shr_u64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_shr_u128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;


	virtual UnderlyingValue binary_add_u8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_add_u16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_add_u32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_add_u64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_add_u128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue binary_sub_u8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_sub_u16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_sub_u32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_sub_u64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_sub_u128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue binary_mul_u8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mul_u16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mul_u32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mul_u64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mul_u128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue binary_div_u8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_div_u16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_div_u32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_div_u64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_div_u128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue binary_mod_u8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mod_u16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mod_u32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mod_u64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mod_u128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	// Section: signed-signed built-in binary ops

	virtual UnderlyingValue binary_b_shr_i8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_shr_i16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_shr_i32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_shr_i64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_b_shr_i128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue binary_add_i8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_add_i16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_add_i32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_add_i64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_add_i128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue binary_sub_i8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_sub_i16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_sub_i32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_sub_i64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_sub_i128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue binary_mul_i8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mul_i16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mul_i32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mul_i64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mul_i128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue binary_div_i8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_div_i16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_div_i32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_div_i64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_div_i128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue binary_mod_i8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mod_i16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mod_i32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mod_i64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mod_i128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	// Section: float-float built-in binary ops

	virtual UnderlyingValue binary_add_half(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_add_float(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_add_double(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue binary_sub_half(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_sub_float(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_sub_double(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue binary_mul_half(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mul_float(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mul_double(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue binary_div_half(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_div_float(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_div_double(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue binary_mod_half(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mod_float(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_mod_double(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue binary_pow_float(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue binary_pow_double(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	static std::vector<std::pair<operation::Binary, bop_generator>>
	builtin_binary_operations() {
		std::vector<std::pair<operation::Binary, bop_generator>> bops{};
		bops.push_back(bop<type::t_uint8, operators::b_and, &Child::binary_b_and_u8>());
		bops.push_back(bop<type::t_uint16, operators::b_and, &Child::binary_b_and_u16>());
		bops.push_back(bop<type::t_uint32, operators::b_and, &Child::binary_b_and_u32>());
		bops.push_back(bop<type::t_uint64, operators::b_and, &Child::binary_b_and_u64>());
		bops.push_back(bop<type::t_uint128, operators::b_and, &Child::binary_b_and_u128>());

		bops.push_back(bop<type::t_uint8, operators::b_or, &Child::binary_b_or_u8>());
		bops.push_back(bop<type::t_uint16, operators::b_or, &Child::binary_b_or_u16>());
		bops.push_back(bop<type::t_uint32, operators::b_or, &Child::binary_b_or_u32>());
		bops.push_back(bop<type::t_uint64, operators::b_or, &Child::binary_b_or_u64>());
		bops.push_back(bop<type::t_uint128, operators::b_or, &Child::binary_b_or_u128>());

		bops.push_back(bop<type::t_uint8, operators::b_xor, &Child::binary_b_xor_u8>());
		bops.push_back(bop<type::t_uint16, operators::b_xor, &Child::binary_b_xor_u16>());
		bops.push_back(bop<type::t_uint32, operators::b_xor, &Child::binary_b_xor_u32>());
		bops.push_back(bop<type::t_uint64, operators::b_xor, &Child::binary_b_xor_u64>());
		bops.push_back(bop<type::t_uint128, operators::b_xor, &Child::binary_b_xor_u128>());

		bops.push_back(bop<type::t_uint8, operators::b_shl, &Child::binary_b_shl_u8>());
		bops.push_back(bop<type::t_uint16, operators::b_shl, &Child::binary_b_shl_u16>());
		bops.push_back(bop<type::t_uint32, operators::b_shl, &Child::binary_b_shl_u32>());
		bops.push_back(bop<type::t_uint64, operators::b_shl, &Child::binary_b_shl_u64>());
		bops.push_back(bop<type::t_uint128, operators::b_shl, &Child::binary_b_shl_u128>());

		bops.push_back(bop<type::t_uint8, operators::b_shr, &Child::binary_b_shr_u8>());
		bops.push_back(bop<type::t_uint16, operators::b_shr, &Child::binary_b_shr_u16>());
		bops.push_back(bop<type::t_uint32, operators::b_shr, &Child::binary_b_shr_u32>());
		bops.push_back(bop<type::t_uint64, operators::b_shr, &Child::binary_b_shr_u64>());
		bops.push_back(bop<type::t_uint128, operators::b_shr, &Child::binary_b_shr_u128>());
		bops.push_back(bop<type::t_int8, operators::b_shr, &Child::binary_b_shr_i8>());
		bops.push_back(bop<type::t_int16, operators::b_shr, &Child::binary_b_shr_i16>());
		bops.push_back(bop<type::t_int32, operators::b_shr, &Child::binary_b_shr_i32>());
		bops.push_back(bop<type::t_int64, operators::b_shr, &Child::binary_b_shr_i64>());
		bops.push_back(bop<type::t_int128, operators::b_shr, &Child::binary_b_shr_i128>());

		bops.push_back(bop<type::t_uint8, operators::add, &Child::binary_add_u8>());
		bops.push_back(bop<type::t_uint16, operators::add, &Child::binary_add_u16>());
		bops.push_back(bop<type::t_uint32, operators::add, &Child::binary_add_u32>());
		bops.push_back(bop<type::t_uint64, operators::add, &Child::binary_add_u64>());
		bops.push_back(bop<type::t_uint128, operators::add, &Child::binary_add_u128>());
		bops.push_back(bop<type::t_int8, operators::add, &Child::binary_add_i8>());
		bops.push_back(bop<type::t_int16, operators::add, &Child::binary_add_i16>());
		bops.push_back(bop<type::t_int32, operators::add, &Child::binary_add_i32>());
		bops.push_back(bop<type::t_int64, operators::add, &Child::binary_add_i64>());
		bops.push_back(bop<type::t_int128, operators::add, &Child::binary_add_i128>());

		bops.push_back(bop<type::t_uint8, operators::sub, &Child::binary_sub_u8>());
		bops.push_back(bop<type::t_uint16, operators::sub, &Child::binary_sub_u16>());
		bops.push_back(bop<type::t_uint32, operators::sub, &Child::binary_sub_u32>());
		bops.push_back(bop<type::t_uint64, operators::sub, &Child::binary_sub_u64>());
		bops.push_back(bop<type::t_uint128, operators::sub, &Child::binary_sub_u128>());
		bops.push_back(bop<type::t_int8, operators::sub, &Child::binary_sub_i8>());
		bops.push_back(bop<type::t_int16, operators::sub, &Child::binary_sub_i16>());
		bops.push_back(bop<type::t_int32, operators::sub, &Child::binary_sub_i32>());
		bops.push_back(bop<type::t_int64, operators::sub, &Child::binary_sub_i64>());
		bops.push_back(bop<type::t_int128, operators::sub, &Child::binary_sub_i128>());

		bops.push_back(bop<type::t_uint8, operators::mul, &Child::binary_mul_u8>());
		bops.push_back(bop<type::t_uint16, operators::mul, &Child::binary_mul_u16>());
		bops.push_back(bop<type::t_uint32, operators::mul, &Child::binary_mul_u32>());
		bops.push_back(bop<type::t_uint64, operators::mul, &Child::binary_mul_u64>());
		bops.push_back(bop<type::t_uint128, operators::mul, &Child::binary_mul_u128>());
		bops.push_back(bop<type::t_int8, operators::mul, &Child::binary_mul_i8>());
		bops.push_back(bop<type::t_int16, operators::mul, &Child::binary_mul_i16>());
		bops.push_back(bop<type::t_int32, operators::mul, &Child::binary_mul_i32>());
		bops.push_back(bop<type::t_int64, operators::mul, &Child::binary_mul_i64>());
		bops.push_back(bop<type::t_int128, operators::mul, &Child::binary_mul_i128>());

		bops.push_back(bop<type::t_uint8, operators::div, &Child::binary_div_u8>());
		bops.push_back(bop<type::t_uint16, operators::div, &Child::binary_div_u16>());
		bops.push_back(bop<type::t_uint32, operators::div, &Child::binary_div_u32>());
		bops.push_back(bop<type::t_uint64, operators::div, &Child::binary_div_u64>());
		bops.push_back(bop<type::t_uint128, operators::div, &Child::binary_div_u128>());
		bops.push_back(bop<type::t_int8, operators::div, &Child::binary_div_i8>());
		bops.push_back(bop<type::t_int16, operators::div, &Child::binary_div_i16>());
		bops.push_back(bop<type::t_int32, operators::div, &Child::binary_div_i32>());
		bops.push_back(bop<type::t_int64, operators::div, &Child::binary_div_i64>());
		bops.push_back(bop<type::t_int128, operators::div, &Child::binary_div_i128>());

		bops.push_back(bop<type::t_uint8, operators::mod, &Child::binary_mod_u8>());
		bops.push_back(bop<type::t_uint16, operators::mod, &Child::binary_mod_u16>());
		bops.push_back(bop<type::t_uint32, operators::mod, &Child::binary_mod_u32>());
		bops.push_back(bop<type::t_uint64, operators::mod, &Child::binary_mod_u64>());
		bops.push_back(bop<type::t_uint128, operators::mod, &Child::binary_mod_u128>());
		bops.push_back(bop<type::t_int8, operators::mod, &Child::binary_mod_i8>());
		bops.push_back(bop<type::t_int16, operators::mod, &Child::binary_mod_i16>());
		bops.push_back(bop<type::t_int32, operators::mod, &Child::binary_mod_i32>());
		bops.push_back(bop<type::t_int64, operators::mod, &Child::binary_mod_i64>());
		bops.push_back(bop<type::t_int128, operators::mod, &Child::binary_mod_i128>());

		bops.push_back(bop<type::t_half, operators::add, &Child::binary_add_half>());
		bops.push_back(bop<type::t_float, operators::add, &Child::binary_add_float>());
		bops.push_back(bop<type::t_double, operators::add, &Child::binary_add_double>());

		bops.push_back(bop<type::t_half, operators::sub, &Child::binary_sub_half>());
		bops.push_back(bop<type::t_float, operators::sub, &Child::binary_sub_float>());
		bops.push_back(bop<type::t_double, operators::sub, &Child::binary_sub_double>());

		bops.push_back(bop<type::t_half, operators::mul, &Child::binary_mul_half>());
		bops.push_back(bop<type::t_float, operators::mul, &Child::binary_mul_float>());
		bops.push_back(bop<type::t_double, operators::mul, &Child::binary_mul_double>());

		bops.push_back(bop<type::t_half, operators::div, &Child::binary_div_half>());
		bops.push_back(bop<type::t_float, operators::div, &Child::binary_div_float>());
		bops.push_back(bop<type::t_double, operators::div, &Child::binary_div_double>());

		bops.push_back(bop<type::t_half, operators::mod, &Child::binary_mod_half>());
		bops.push_back(bop<type::t_float, operators::mod, &Child::binary_mod_float>());
		bops.push_back(bop<type::t_double, operators::mod, &Child::binary_mod_double>());

		bops.push_back(bop<type::t_float, operators::pow, &Child::binary_pow_float>());
		bops.push_back(bop<type::t_double, operators::pow, &Child::binary_pow_double>());
		return bops;
	}
	using comparison_generator = bop_generator;
	std::vector<std::pair<operation::Comparison, comparison_generator>>
			comparison_operations{std::move(builtin_comparison_operations())};
	template<const type::Type &left, const operators::comparison op, UnderlyingValue (Child::*Lambda)(UnderlyingValue lhs, UnderlyingValue rhs), const type::Type &right = left, const type::Type &result = left>
	static std::pair<operation::Comparison, comparison_generator>
	cop() {
		return std::make_pair<operation::Comparison, comparison_generator>(
				operation::Comparison{left, right, op},
				static_cast<comparison_generator>(+[](Child &visitor, const Value &lhs, const Value &rhs) -> Value {
					return Value{result,
								 (visitor.*Lambda)(visitor.deref(lhs).value, visitor.deref(rhs).value)};
				}));
	}

	// Section: unsigned-unsigned built-in comparison ops

	virtual UnderlyingValue comparison_less_u8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_u16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_u32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_u64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_u128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue comparison_less_eq_u8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_eq_u16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_eq_u32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_eq_u64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_eq_u128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue comparison_greater_u8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_u16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_u32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_u64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_u128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue comparison_greater_eq_u8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_eq_u16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_eq_u32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_eq_u64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_eq_u128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue comparison_eq_eq_u8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_eq_eq_u16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_eq_eq_u32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_eq_eq_u64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_eq_eq_u128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue comparison_not_equal_u8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_not_equal_u16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_not_equal_u32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_not_equal_u64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_not_equal_u128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	// Section: signed-signed built-in comparison ops

	virtual UnderlyingValue comparison_less_i8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_i16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_i32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_i64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_i128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue comparison_less_eq_i8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_eq_i16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_eq_i32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_eq_i64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_eq_i128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue comparison_greater_i8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_i16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_i32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_i64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_i128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue comparison_greater_eq_i8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_eq_i16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_eq_i32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_eq_i64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_eq_i128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue comparison_eq_eq_i8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_eq_eq_i16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_eq_eq_i32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_eq_eq_i64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_eq_eq_i128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue comparison_not_equal_i8(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_not_equal_i16(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_not_equal_i32(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_not_equal_i64(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_not_equal_i128(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	// Section: float-float built-in comparison ops
	virtual UnderlyingValue comparison_less_half(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_float(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_double(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue comparison_less_eq_half(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_eq_float(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_less_eq_double(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue comparison_greater_half(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_float(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_double(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue comparison_greater_eq_half(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_eq_float(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_greater_eq_double(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue comparison_eq_eq_half(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_eq_eq_float(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_eq_eq_double(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	virtual UnderlyingValue comparison_not_equal_half(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_not_equal_float(UnderlyingValue lhs, UnderlyingValue rhs) = 0;
	virtual UnderlyingValue comparison_not_equal_double(UnderlyingValue lhs, UnderlyingValue rhs) = 0;

	std::vector<std::pair<operation::Comparison, comparison_generator>>
	builtin_comparison_operations() {
		std::vector<std::pair<operation::Comparison, comparison_generator>> cops{};

		cops.push_back(cop<type::t_uint8, operators::less, &Child::comparison_less_u8>());
		cops.push_back(cop<type::t_uint16, operators::less, &Child::comparison_less_u16>());
		cops.push_back(cop<type::t_uint32, operators::less, &Child::comparison_less_u32>());
		cops.push_back(cop<type::t_uint64, operators::less, &Child::comparison_less_u64>());
		cops.push_back(cop<type::t_uint128, operators::less, &Child::comparison_less_u128>());
		cops.push_back(cop<type::t_int8, operators::less, &Child::comparison_less_i8>());
		cops.push_back(cop<type::t_int16, operators::less, &Child::comparison_less_i16>());
		cops.push_back(cop<type::t_int32, operators::less, &Child::comparison_less_i32>());
		cops.push_back(cop<type::t_int64, operators::less, &Child::comparison_less_i64>());
		cops.push_back(cop<type::t_int128, operators::less, &Child::comparison_less_i128>());
		cops.push_back(cop<type::t_half, operators::less, &Child::comparison_less_half>());
		cops.push_back(cop<type::t_float, operators::less, &Child::comparison_less_float>());
		cops.push_back(cop<type::t_double, operators::less, &Child::comparison_less_double>());

		cops.push_back(cop<type::t_uint8, operators::less_eq, &Child::comparison_less_eq_u8>());
		cops.push_back(cop<type::t_uint16, operators::less_eq, &Child::comparison_less_eq_u16>());
		cops.push_back(cop<type::t_uint32, operators::less_eq, &Child::comparison_less_eq_u32>());
		cops.push_back(cop<type::t_uint64, operators::less_eq, &Child::comparison_less_eq_u64>());
		cops.push_back(cop<type::t_uint128, operators::less_eq, &Child::comparison_less_eq_u128>());
		cops.push_back(cop<type::t_int8, operators::less_eq, &Child::comparison_less_eq_i8>());
		cops.push_back(cop<type::t_int16, operators::less_eq, &Child::comparison_less_eq_i16>());
		cops.push_back(cop<type::t_int32, operators::less_eq, &Child::comparison_less_eq_i32>());
		cops.push_back(cop<type::t_int64, operators::less_eq, &Child::comparison_less_eq_i64>());
		cops.push_back(cop<type::t_int128, operators::less_eq, &Child::comparison_less_eq_i128>());
		cops.push_back(cop<type::t_half, operators::less_eq, &Child::comparison_less_eq_half>());
		cops.push_back(cop<type::t_float, operators::less_eq, &Child::comparison_less_eq_float>());
		cops.push_back(cop<type::t_double, operators::less_eq, &Child::comparison_less_eq_double>());

		cops.push_back(cop<type::t_uint8, operators::greater, &Child::comparison_greater_u8>());
		cops.push_back(cop<type::t_uint16, operators::greater, &Child::comparison_greater_u16>());
		cops.push_back(cop<type::t_uint32, operators::greater, &Child::comparison_greater_u32>());
		cops.push_back(cop<type::t_uint64, operators::greater, &Child::comparison_greater_u64>());
		cops.push_back(cop<type::t_uint128, operators::greater, &Child::comparison_greater_u128>());
		cops.push_back(cop<type::t_int8, operators::greater, &Child::comparison_greater_i8>());
		cops.push_back(cop<type::t_int16, operators::greater, &Child::comparison_greater_i16>());
		cops.push_back(cop<type::t_int32, operators::greater, &Child::comparison_greater_i32>());
		cops.push_back(cop<type::t_int64, operators::greater, &Child::comparison_greater_i64>());
		cops.push_back(cop<type::t_int128, operators::greater, &Child::comparison_greater_i128>());
		cops.push_back(cop<type::t_half, operators::greater, &Child::comparison_greater_half>());
		cops.push_back(cop<type::t_float, operators::greater, &Child::comparison_greater_float>());
		cops.push_back(cop<type::t_double, operators::greater, &Child::comparison_greater_double>());

		cops.push_back(cop<type::t_uint8, operators::greater_eq, &Child::comparison_greater_eq_u8>());
		cops.push_back(cop<type::t_uint16, operators::greater_eq, &Child::comparison_greater_eq_u16>());
		cops.push_back(cop<type::t_uint32, operators::greater_eq, &Child::comparison_greater_eq_u32>());
		cops.push_back(cop<type::t_uint64, operators::greater_eq, &Child::comparison_greater_eq_u64>());
		cops.push_back(cop<type::t_uint128, operators::greater_eq, &Child::comparison_greater_eq_u128>());
		cops.push_back(cop<type::t_int8, operators::greater_eq, &Child::comparison_greater_eq_i8>());
		cops.push_back(cop<type::t_int16, operators::greater_eq, &Child::comparison_greater_eq_i16>());
		cops.push_back(cop<type::t_int32, operators::greater_eq, &Child::comparison_greater_eq_i32>());
		cops.push_back(cop<type::t_int64, operators::greater_eq, &Child::comparison_greater_eq_i64>());
		cops.push_back(cop<type::t_int128, operators::greater_eq, &Child::comparison_greater_eq_i128>());
		cops.push_back(cop<type::t_half, operators::greater_eq, &Child::comparison_greater_eq_half>());
		cops.push_back(cop<type::t_float, operators::greater_eq, &Child::comparison_greater_eq_float>());
		cops.push_back(cop<type::t_double, operators::greater_eq, &Child::comparison_greater_eq_double>());

		cops.push_back(cop<type::t_uint8, operators::eq_eq, &Child::comparison_eq_eq_u8>());
		cops.push_back(cop<type::t_uint16, operators::eq_eq, &Child::comparison_eq_eq_u16>());
		cops.push_back(cop<type::t_uint32, operators::eq_eq, &Child::comparison_eq_eq_u32>());
		cops.push_back(cop<type::t_uint64, operators::eq_eq, &Child::comparison_eq_eq_u64>());
		cops.push_back(cop<type::t_uint128, operators::eq_eq, &Child::comparison_eq_eq_u128>());
		cops.push_back(cop<type::t_int8, operators::eq_eq, &Child::comparison_eq_eq_i8>());
		cops.push_back(cop<type::t_int16, operators::eq_eq, &Child::comparison_eq_eq_i16>());
		cops.push_back(cop<type::t_int32, operators::eq_eq, &Child::comparison_eq_eq_i32>());
		cops.push_back(cop<type::t_int64, operators::eq_eq, &Child::comparison_eq_eq_i64>());
		cops.push_back(cop<type::t_int128, operators::eq_eq, &Child::comparison_eq_eq_i128>());
		cops.push_back(cop<type::t_half, operators::eq_eq, &Child::comparison_eq_eq_half>());
		cops.push_back(cop<type::t_float, operators::eq_eq, &Child::comparison_eq_eq_float>());
		cops.push_back(cop<type::t_double, operators::eq_eq, &Child::comparison_eq_eq_double>());

		cops.push_back(cop<type::t_uint8, operators::not_equal, &Child::comparison_not_equal_u8>());
		cops.push_back(cop<type::t_uint16, operators::not_equal, &Child::comparison_not_equal_u16>());
		cops.push_back(cop<type::t_uint32, operators::not_equal, &Child::comparison_not_equal_u32>());
		cops.push_back(cop<type::t_uint64, operators::not_equal, &Child::comparison_not_equal_u64>());
		cops.push_back(cop<type::t_uint128, operators::not_equal, &Child::comparison_not_equal_u128>());
		cops.push_back(cop<type::t_int8, operators::not_equal, &Child::comparison_not_equal_i8>());
		cops.push_back(cop<type::t_int16, operators::not_equal, &Child::comparison_not_equal_i16>());
		cops.push_back(cop<type::t_int32, operators::not_equal, &Child::comparison_not_equal_i32>());
		cops.push_back(cop<type::t_int64, operators::not_equal, &Child::comparison_not_equal_i64>());
		cops.push_back(cop<type::t_int128, operators::not_equal, &Child::comparison_not_equal_i128>());
		cops.push_back(cop<type::t_half, operators::not_equal, &Child::comparison_not_equal_half>());
		cops.push_back(cop<type::t_float, operators::not_equal, &Child::comparison_not_equal_float>());
		cops.push_back(cop<type::t_double, operators::not_equal, &Child::comparison_not_equal_double>());
		return cops;
	}

	/// Store rvalue into the reference lvalue
	virtual void impl_store(Value lvalue, Value rvalue) = 0;
	Value assignment(const ast::expr::Binop &expr) {
		const auto lvalue = visit(expr.left);
		const auto rvalue = deref(visit(expr.right));
		if (!lvalue.type.is_ref)
			throw std::runtime_error(
					fmt("Left side of assignment is not an lvalue ", expr.location));
		if (lvalue.type.is_const)
			throw std::runtime_error(
					fmt("Left side of assignment is constant ", expr.location));
		impl_store(lvalue, rvalue);
		return lvalue;
	}
	virtual Value impl_logical(const ast::expr::Binop &expr) = 0;
	Value logical(const ast::expr::Binop &expr) {
		return impl_logical(expr);
	}

	virtual UnderlyingValue impl_call(type::Function type, FunctionValue fn, std::span<UnderlyingValue> values) = 0;
	std::optional<Value> try_binop(const ast::expr::Binop &expr, const Value &lvalue, const Value &rvalue) {
		Child &child_ref = *dynamic_cast<Child *>(this);
		for (auto &[op, gen] : binary_operations) {
			if (op.op == expr.op && op.left == lvalue.type &&
				op.right == rvalue.type) {
				return gen(child_ref, lvalue, rvalue);
			}
		}
		std::string_view op_str = string(expr.op);
		auto types = std::array{lvalue.type, rvalue.type};
		if (!types[0].is_ref)
			types[0].is_const = true;
		if (!types[1].is_ref)
			types[1].is_const = true;
		if (auto function = all_function_container.get_function(this, op_str, {}, types)) {
			std::array<UnderlyingValue, 2> args{lvalue.value, rvalue.value};
			auto call = impl_call(function->type, function->value, args);
			return Value{
					*function->type.return_type,
					call};
		}
		return std::nullopt;
	}

	Value visit(const ast::expr::Binop &expr) {
		if (expr.op == operators::assign)
			return assignment(expr);
		if (expr.op == operators::l_and || expr.op == operators::l_or)
			return logical(expr);
		auto lvalue = visit(expr.left);
		auto rvalue = visit(expr.right);
		if (auto value = try_binop(expr, lvalue, rvalue))
			return *value;
		lvalue = deref(lvalue);
		rvalue = deref(rvalue);
		if (auto value = try_binop(expr, lvalue, rvalue))
			return *value;
		throw std::runtime_error(fmt("Couldn't find a corresponding operation (function) for operator '", expr.op, "' :( ", expr.location));
	}

	/// todo refactor this with some kind of backend-independent "phi" & block system
	virtual Value impl_comparison(const ast::expr::Comparison &expr) = 0;
	Value visit(const ast::expr::Comparison &expr) {
		return impl_comparison(expr);
	}

	Value visit(const ast::expr::Unary &expr) {
		Child &child_ref = *dynamic_cast<Child *>(this);
		auto operand = visit(expr.expr);
		for (auto &[op, gen] : unary_operations) {
			if (op.op == expr.op && op.operand == operand.type) {
				return gen(child_ref, operand);
			}
		}
		throw std::runtime_error(
				fmt("No matching unary operation ", expr.location));
	}

	virtual Value impl_conditional(const ast::expr::Conditional &expr) = 0;
	Value visit(const ast::expr::Conditional &expr) {
		return impl_conditional(expr);
	}

	virtual UnderlyingValue impl_value_call(type::Function type, UnderlyingValue fn, std::span<UnderlyingValue> values) = 0;
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
		std::vector<UnderlyingValue> args;
		for (const auto &[param, arg] :
			 std::views::zip(funtype.parameters, expr.arguments)) {
			auto value = visit(&arg);
			if (value.type != param)
				throw std::runtime_error(fmt("Argument type mismatch ", expr.location));
			args.push_back(value.value);
		}
		auto call = impl_call(funtype, function.value, args);
		return Value{
				*funtype.return_type,
				call};
	}

	Value name_call(const ast::expr::Call &expr) {
		const auto id = expr.callee->get<ast::expr::Identifier>();
		std::vector<Value> args;
		for (const auto &arg : expr.arguments) {
			auto value = visit(&arg);
			if (!value.type.is_ref)
				value.type.is_const = true;
			args.push_back(std::move(value));
		}
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
		std::vector<UnderlyingValue> underlying_args;
		for (const auto &arg : args)
			underlying_args.push_back(arg.value);
		auto call = impl_call(function->type, function->value, underlying_args);
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
		auto callee_value = visit(expr.callee);
		if (!callee_value.type.is_ref)
			callee_value.type.is_const = true;
		std::vector args{std::move(callee_value)};
		for (const auto &arg : expr.arguments) {
			auto value = visit(&arg);
			if (!value.type.is_ref)
				value.type.is_const = true;
			args.push_back(std::move(value));
		}
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
		std::vector<UnderlyingValue> underlying_args;
		for (const auto &arg : args)
			underlying_args.push_back(arg.value);
		auto call = impl_call(function->type, function->value, underlying_args);
		return Value{
				*function->type.return_type,
				call};
	}

	virtual UnderlyingValue impl_create(const type::NamedStruct &type, std::span<UnderlyingValue> members) = 0;
	Value visit(const ast::expr::Create &expr) {
		auto type = visit(expr.type);
		if (!type.is<type::NamedStruct>())
			throw std::runtime_error(
					fmt("Cannot create non-struct types (", expr.location, ")"));
		const auto &named = type.get<type::NamedStruct>();
		if (expr.args.size() != named.members.size())
			throw std::runtime_error(
					fmt("Wrong number of members for struct ", expr.location));
		std::vector<UnderlyingValue> members;
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
			members.push_back(value.value);
		}
		auto alloca = impl_create(named, members);
		type.is_const = true;
		type.is_ref = true;
		return Value{type, alloca};
	}


	virtual UnderlyingValue impl_subscript_ptr(type::Type pointed_type, UnderlyingValue pointer, UnderlyingValue index) = 0;
	virtual UnderlyingValue impl_subscript_arr_ref(type::Array array_type, UnderlyingValue arr_ref, UnderlyingValue index) = 0;
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
			auto gep = impl_subscript_ptr(member_type, value.value, index.value);
			member_type.is_ref = true;
			return Value{member_type, gep};
		}
		if (value.type.is<type::Array>()) {
			value = mkref(value);
			const auto &array_type = value.type.get<type::Array>();
			type::Type member_type = *array_type.member;
			member_type.is_ref = true;
			member_type.is_const = value.type.is_const;
			auto gep = impl_subscript_arr_ref(array_type, value.value, index.value);
			return Value{member_type, gep};
		}
		throw std::runtime_error(
				fmt("Cannot subscript non-array/pointer types ", expr.location));
	}

	virtual UnderlyingValue impl_array(const type::Array &type, std::span<UnderlyingValue> members) = 0;
	Value visit(const ast::expr::Array &expr) {
		if (expr.values.empty())
			throw std::runtime_error("Empty array");
		auto first = deref(visit(&expr.values.front()));
		auto member_type = first.type;
		type::Array array_type{std::make_unique<type::Type>(member_type),
							   expr.values.size()};
		std::vector<UnderlyingValue> members{first.value};
		for (const auto &[index, value] :
			 std::views::enumerate(expr.values | std::views::drop(1))) {
			auto value_ = deref(visit(&value));
			if (value_.type != member_type)
				throw std::runtime_error(fmt("Array member type mismatch ",
											 expr.location));
			members.push_back(value_.value);
		}
		auto alloca = impl_array(array_type, members);
		return Value{type::Type{array_type, true, true},
					 alloca};
	}

	virtual UnderlyingValue impl_numeric_convert(UnderlyingValue value, type::Primitive from, type::Primitive to) = 0;
	Value visit(const ast::expr::As &expr) {
		auto value = deref(visit(expr.value));
		auto type = visit(expr.type);
		if (!type.is<type::Primitive>() || !value.type.is<type::Primitive>())
			throw std::runtime_error("`as` expression must be between primitive types");
		if (type.is_ref)
			throw std::runtime_error("`as` expression can't convert to a reference type");
		auto from_prim = value.type.get<type::Primitive>();
		auto to_prim = type.get<type::Primitive>();
		if (!from_prim.is_numeric() || !to_prim.is_numeric())
			throw std::runtime_error("`as` expression must be between numeric types");
		return Value{
				type,
				impl_numeric_convert(value.value, from_prim, to_prim)};
	}

	virtual UnderlyingValue impl_member(const type::NamedStruct &type, UnderlyingValue struct_ref, size_t member_index) = 0;
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
				return Value{member_type, impl_member(named, value.value, index)};
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

	virtual UnderlyingValue impl_const_int32(int32_t value) = 0;
	virtual UnderlyingValue impl_const_double(double value) = 0;
	Value visit(const ast::expr::Number &expr) {
		return expr.value.visit(
				[&](const ast::expr::Number::uint_t uinty) -> Value {
					return Value{type::t_int32, impl_const_int32(static_cast<int32_t>(uinty))};
				},
				[&](const ast::expr::Number::float_t floaty) -> Value {
					return Value{type::t_double, impl_const_double(floaty)};
				});
	}
};
} // namespace backend