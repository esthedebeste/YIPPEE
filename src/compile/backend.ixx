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
#include <string_view>
#include <unordered_map>
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
template<class UnderlyingValue, class FunctionValue>
struct Base {
	virtual ~Base() = default;
	struct Value {
		type::Type type;
		UnderlyingValue value;
	};
	struct TopLevelFunction;
	virtual FunctionValue generate_function_value(const TopLevelFunction &tlf, const type::Function &type) = 0;
	struct Namespace;
	struct TopLevelFunction {
		Namespace *ns;
		const ast::top::Function *ast;
		// returns std::nullopt if invalid
		std::optional<type::Function> type(Base *base, std::span<type::Type> type_parameters) {
			LocalScope old_locals(ns);
			// when generating the template, return
			// back to namespace of definition
			std::swap(base->locals, old_locals);
			base->locals.emplace_back();
			std::vector<type::Type> type_arguments{};
			for (const auto &[index, argument] : std::views::enumerate(ast->type_arguments)) {
				std::optional<type::Type> type;
				if (index < type_parameters.size())
					type = type_parameters[index];
				else if (argument.default_type)
					type = base->visit(*argument.default_type);
				if (!type)
					return std::nullopt;
				type::Type argtype = std::move(*type);
				base->locals.back().types.emplace(
						argument.name,
						[argument, argtype](std::span<type::Type> arguments) {
							if (!arguments.empty())
								throw std::runtime_error(fmt("Type arguments for type '",
															 argument.name,
															 "' are not allowed"));
							return argtype;
						});
				type_arguments.push_back(argtype);
			}
			std::optional<type::Function> result{};
			try {
				std::vector<type::Type> args;
				for (const ast::TypeAst &type : ast->parameters | std::views::values)
					args.push_back(std::move(base->visit(&type)));
				type::Type return_type = base->visit(ast->return_type);
				result = type::Function{args, return_type};
			} catch (std::runtime_error &e) {
				result = std::nullopt;
			}
			base->locals.pop_back();
			std::swap(base->locals, old_locals);
			return result;
		}
		// returns std::nullopt if invalid, or if must_succeed is true, rethrows an error
		std::optional<FunctionValue> value(Base *base, std::span<type::Type> type_parameters, bool must_succeed = false) {
			LocalScope old_locals(ns);
			// when generating the template, return
			// back to namespace of definition
			std::swap(base->locals, old_locals);
			base->locals.emplace_back();
			auto cleanup = [&] {
				base->locals.pop_back();
				std::swap(base->locals, old_locals);
			};
			std::vector<type::Type> type_arguments{};
			for (const auto &[index, argument] : std::views::enumerate(ast->type_arguments)) {
				std::optional<type::Type> type;
				if (index < type_parameters.size())
					type = type_parameters[index];
				else if (argument.default_type)
					type = base->visit(*argument.default_type);
				if (!type)
					if (must_succeed) {
						cleanup();
						throw std::runtime_error(fmt("Missing type argument for function ", ast->location));
					} else
						return std::nullopt;
				type::Type argtype = std::move(*type);
				base->locals.back().types.emplace(
						argument.name,
						[argument, argtype](std::span<type::Type> arguments) {
							if (!arguments.empty())
								throw std::runtime_error(fmt("Type arguments for type '",
															 argument.name,
															 "' are not allowed"));
							return argtype;
						});
				type_arguments.push_back(argtype);
			}
			std::optional<FunctionValue> result{};
			try {
				std::vector<type::Type> args;
				for (const auto &type : ast->parameters | std::views::values)
					args.emplace_back(base->visit(&type));
				type::Type return_type = base->visit(ast->return_type);
				type::Function type{args, return_type};
				result = base->generate_function_value(*this, type);
			} catch (...) {
				if (must_succeed) {
					cleanup();
					std::rethrow_exception(std::current_exception());
				}
				result = std::nullopt;
			}
			cleanup();
			return result;
		}
	};
	struct GetFunctionResult {
		TopLevelFunction *tlf;
		type::Function type;
		FunctionValue value;
	};
	using TypeTemplate = std::function<type::Type(std::span<type::Type>)>;
	struct FunctionContainer {
		bool can_contain_ambiguous = false;
		FunctionContainer(bool can_contain_ambiguous = false) : can_contain_ambiguous{can_contain_ambiguous} {}
		std::optional<GetFunctionResult> get_function(Base *base, std::string_view name, std::span<type::Type> type_parameters, std::span<type::Type> parameters) {
			if (auto it = functions.find(name); it != functions.end()) {
				std::optional<GetFunctionResult> result{};
				for (auto &func : it->second)
					if (auto type = func.type(base, type_parameters); type && type->parameters == parameters)
						if (auto value = func.value(base, type_parameters))
							if (result)
								throw std::runtime_error(fmt("Ambiguous function call for function '", name, "'; ", func.ast->location, " and ", result->tlf->ast->location, " both valid for this call"));
							else
								result = GetFunctionResult{&func, *type, *value};
				return result;
			}
			return std::nullopt;
		}
		TopLevelFunction &emplace_function(Base *base, std::string_view string,
										   Namespace *ns,
										   const ast::top::Function *ast) {
			TopLevelFunction tlf{ns, ast};
			if (ast->type_arguments.empty()) {
				std::vector<type::Type> parameters;
				for (const auto &param : ast->parameters | std::views::values)
					parameters.push_back(base->visit(&param));
				if (!can_contain_ambiguous)
					if (auto func = get_function(base, string, {}, parameters))
						throw std::runtime_error(fmt("Function already exists with the same signature - ", ast->location, " and ", func->tlf->ast->location));
			}
			if (auto it = functions.find(string); it != functions.end())
				return it->second.emplace_back(tlf);
			else
				return functions
						.emplace(std::string(string), std::vector<TopLevelFunction>{})
						.first->second.emplace_back(tlf);
		}
		TopLevelFunction &emplace_function(Base *base, TopLevelFunction tlf) {
			const auto &string = tlf.ast->name.final.str;
			if (tlf.ast->type_arguments.empty()) {
				std::vector<type::Type> parameters;
				for (const auto &param : tlf.ast->parameters | std::views::values)
					parameters.push_back(base->visit(&param));
				if (!can_contain_ambiguous)
					if (auto func = get_function(base, string, {}, parameters))
						throw std::runtime_error(fmt("Function already exists with the same signature - ", tlf.ast->location, " and ", func->tlf->ast->location));
			}
			if (auto it = functions.find(string); it != functions.end())
				return it->second.emplace_back(tlf);
			else
				return functions
						.emplace(std::string(string), std::vector<TopLevelFunction>{})
						.first->second.emplace_back(tlf);
		}

	private:
		utils::string_map<std::vector<TopLevelFunction>> functions{};
	};

	struct Scope : FunctionContainer {
		utils::string_map<Value> values;
		utils::string_map<TypeTemplate> types;
	};

	struct Namespace : Scope {
		Namespace *parent;
		std::string name;
		Namespace(Namespace *parent, const std::string &name) : parent{parent}, name{name} {}
		std::unordered_map<std::string, Namespace> children;

		std::vector<std::string> path() {
			std::vector<std::string> result;
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

	Namespace root_namespace{nullptr, "YIPPEE"};
	LocalScope locals{&root_namespace};
	FunctionContainer all_function_container{true};

	type::Function function_type(const ast::top::Function &function) {
		std::vector<type::Type> args;
		for (const auto &type : function.parameters | std::views::values)
			args.push_back(visit(&type));
		return type::Function{args, visit(function.return_type)};
	}
	void visit(const ast::top::Struct &ast) {
		const auto ns_ = locals.ns->namespace_(ast.name, false);
		naming::FullName name{ns_->path(), ast.name.final.str};
		if (ns_->types.contains(ast.name.final.str))
			throw std::runtime_error(fmt("Struct '", name, "' already defined"));

		auto res = ns_->types.emplace(
				name.final, [=](std::span<type::Type> arguments) {
					LocalScope old_locals(ns_);
					// when generating the template, return back to namespace of definition
					std::swap(locals, old_locals);
					locals.emplace_back();
					std::vector<type::Type> type_arguments{};
					for (const auto &[index, argument] : std::views::enumerate(ast.type_arguments)) {
						std::optional<type::Type> type;
						if (index < arguments.size())
							type = arguments[index];
						else if (argument.default_type)
							type = visit(*argument.default_type);
						if (!type)
							throw std::runtime_error(
									fmt("Missing type argument for struct ", argument.name, ". ", ast.location));
						type::Type argtype = std::move(*type);
						locals.back().types.emplace(
								argument.name,
								[argument, argtype](std::span<type::Type> arguments) {
									if (!arguments.empty())
										throw std::runtime_error(fmt("Type arguments for type '",
																	 argument.name,
																	 "' are not allowed"));
									return argtype;
								});
						type_arguments.push_back(argtype);
					}
					type::NamedStruct result{name, type_arguments, {}};
					for (const auto &[name, type] : ast.members)
						result.members.emplace_back(name, visit(&type));
					locals.pop_back();
					std::swap(locals, old_locals);
					return result;
				});

		if (ast.type_arguments.empty()) {
			// if it's not templated, check if the struct works out pre-reference by calling it
			res.first->second({});
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
		return (*template_)(arguments);
	}
	type::Type visit(const ast::type::Pointer &pointer) {
		auto type = visit(pointer.pointed);
		return type::Pointer{std::make_unique<type::Type>(type)};
	}
	type::Type visit(const ast::type::Array &array) {
		auto member = visit(array.member);
		return type::Array{std::make_unique<type::Type>(member), array.size};
	}
	[[noreturn]] void visit(const std::monostate) {
		throw std::runtime_error("unexpected empty AST node");
	}
};
} // namespace backend