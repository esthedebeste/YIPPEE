module;
#include <memory>
#include <utility>
module ast.exprs.conditional;
import ast;
import utils;

namespace ast {
namespace expr {
Conditional::Conditional(const Location &location, ExprPtr condition, ExprPtr thenExpr,
						 ExprPtr elseExpr)
	: AstBase(location), condition{std::move(condition)},
	  thenExpr{std::move(thenExpr)}, elseExpr{std::move(elseExpr)} {}
Conditional::Conditional(const Conditional &other)
	: AstBase(other.location), condition{clone(other.condition)},
	  thenExpr{clone(other.thenExpr)},
	  elseExpr{clone(other.elseExpr)} {}
Conditional::Conditional(Conditional &&other) = default;
Conditional &Conditional::operator=(const Conditional &other) {
	AstBase::operator=(other);
	condition = clone(other.condition);
	thenExpr = clone(other.thenExpr);
	elseExpr = clone(other.elseExpr);
	return *this;
}
Conditional &Conditional::operator=(Conditional &&other) noexcept = default;
void Conditional::children(children_cb cb) const {
	cb(base_ptr(condition.get()));
	cb(base_ptr(thenExpr.get()));
	cb(base_ptr(elseExpr.get()));
}
void Conditional::summarize(std::ostream &os) const { os << "Conditional"; }
} // namespace expr
} // namespace ast
