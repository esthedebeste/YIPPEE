module;
#include <coroutine>

#include <memory>
#include <ostream>
#include <utility>
module ast.stmt.expr;
import ast;
import utils;
namespace ast {
namespace stmt {
Expr::Expr(const Location &location, ExprPtr expr)
	: AstBase(location), expr{std::move(expr)} {}
Expr::Expr(const Expr &other)
	: AstBase(other), expr{clone(other.expr)} {}
Expr::Expr(Expr &&other) noexcept = default;
Expr &Expr::operator=(const Expr &other) {
	AstBase::operator=(other);
	expr = clone(other.expr);
	return *this;
}
Expr &Expr::operator=(Expr &&other) noexcept = default;
void Expr::children(children_cb cb) const { cb(base_ptr(expr.get())); }
void Expr::summarize(std::ostream &os) const { os << "ExprStatement"; }
} // namespace stmt
} // namespace ast
