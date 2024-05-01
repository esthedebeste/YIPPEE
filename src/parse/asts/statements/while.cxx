module;
#include <coroutine>

#include <memory>
#include <ostream>
#include <utility>
module ast.stmt.while_;
import ast;
import utils;
namespace ast {
namespace stmt {
While::While(const Location &location, ExprPtr expr, StatementPtr then)
	: AstBase(location), expr{std::move(expr)}, body{std::move(then)} {}
While::While(const While &other)
	: AstBase(other), expr{clone(other.expr)}, body{clone(other.body)} {}
While::While(While &&other) noexcept = default;
While &While::operator=(const While &other) {
	AstBase::operator=(other);
	expr = clone(other.expr);
	body = clone(other.body);
	return *this;
}
While &While::operator=(While &&other) noexcept = default;
void While::children(children_cb cb) const {
	cb(base_ptr(expr.get()));
	cb(base_ptr(body.get()));
}
void While::summarize(std::ostream &os) const { os << "WhileStatement"; }
} // namespace stmt
} // namespace ast
