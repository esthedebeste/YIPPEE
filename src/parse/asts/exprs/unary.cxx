module;

#include <memory>
#include <string_view>
#include <utility>
module ast.exprs.unary;
import ast;
import operators;
import utils;

namespace ast {
namespace expr {
Unary::Unary(const Location &location, const operators::unary op, ExprPtr expr)
	: AstBase(location), op{op}, expr{std::move(expr)} {}
Unary::Unary(const Unary &other)
	: AstBase(other), op{other.op}, expr{clone(other.expr)} {}
Unary::Unary(Unary &&other) = default;
Unary &Unary::operator=(const Unary &other) {
	AstBase::operator=(other);
	op = other.op;
	expr = clone(other.expr);
	return *this;
}
Unary &Unary::operator=(Unary &&other) noexcept = default;
void Unary::children(children_cb cb) const { cb(base_ptr(expr.get())); }
void Unary::summarize(std::ostream &os) const { os << "Unary(" << op << ")"; }
} // namespace expr
} // namespace ast
