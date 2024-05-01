module;
#include <coroutine>

#include <memory>
#include <optional>
#include <ostream>
#include <utility>
module ast.stmt.variable;
import ast;
import utils;
namespace ast {
namespace stmt {
Variable::Variable(const Location &location, std::string name,
				   std::optional<TypePtr> type, ExprPtr expr)
	: AstBase(location), name{std::move(name)}, type{std::move(type)},
	  expr{std::move(expr)} {}
Variable::Variable(const Variable &other)
	: AstBase(other), name{other.name}, expr{clone(other.expr)} {
	type = clone(other.type);
}
Variable::Variable(Variable &&other) noexcept = default;
Variable &Variable::operator=(const Variable &other) {
	AstBase::operator=(other);
	name = other.name;
	expr = clone(other.expr);
	type = clone(other.type);
	return *this;
}
Variable &Variable::operator=(Variable &&other) noexcept = default;
void Variable::summarize(std::ostream &os) const {
	os << "Variable(" << name << ')';
}
void Variable::children(children_cb cb) const {
	if (type)
		cb(base_ptr(type->get()));
	cb(base_ptr(expr.get()));
}
} // namespace stmt
} // namespace ast
