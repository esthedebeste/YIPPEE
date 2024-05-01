module;
#include <coroutine>

#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <utility>
#include <vector>
module ast.top.function;
import ast;
import utils;
namespace ast {
namespace top {
Function::Function(const Location &location, Identifier name,
				   std::vector<TypeArgument> type_arguments,
				   std::vector<Parameter> parameters, TypePtr return_type,
				   StatementPtr statement)
	: AstBase(location), name{std::move(name)},
	  type_arguments(std::move(type_arguments)),
	  parameters(std::move(parameters)), return_type{std::move(return_type)},
	  statement{std::move(statement)} {}
Function::Function(const Function &other) : AstBase(other), name{other.name},
											type_arguments(other.type_arguments), parameters(other.parameters),
											return_type{clone(other.return_type)}, statement{clone(other.statement)} {}
Function::Function(Function &&other) noexcept = default;
Function &Function::operator=(const Function &other) {
	AstBase::operator=(other);
	name = other.name;
	type_arguments = other.type_arguments;
	parameters = other.parameters;
	return_type = clone(other.return_type);
	statement = clone(other.statement);
	return *this;
}
Function &Function::operator=(Function &&other) noexcept = default;
Function::~Function() {}
void Function::children(children_cb cb) const {
	for (auto &type_arg : type_arguments)
		cb(base_ptr(&type_arg));
	for (auto &param : parameters) {
		cb(base_ptr(&param.first));
		cb(base_ptr(&param.second));
	}
	cb(base_ptr(statement.get()));
}
void Function::summarize(std::ostream &os) const {
	os << "Function(" << name << ')';
}
} // namespace top
  // namespace top
} // namespace ast
