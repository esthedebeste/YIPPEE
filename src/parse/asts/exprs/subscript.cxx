module;

#include <memory>
#include <string_view>
#include <utility>
module ast.exprs.subscript;
import ast;
import operators;
import utils;

namespace ast {
namespace expr {
Subscript::Subscript(const Location &location, ExprPtr expr, ExprPtr index)
	: AstBase(location), expr{std::move(expr)}, index{std::move(index)} {}
Subscript::Subscript(const Subscript &other)
	: AstBase(other), expr{clone(other.expr)},
	  index{clone(other.index)} {}
Subscript::Subscript(Subscript &&other) = default;
Subscript &Subscript::operator=(const Subscript &other) {
	AstBase::operator=(other);
	expr = clone(other.expr);
	index = clone(other.index);
	return *this;
}
Subscript &Subscript::operator=(Subscript &&other) noexcept = default;
void Subscript::children(children_cb cb) const {
	cb(base_ptr(expr.get()));
	cb(base_ptr(index.get()));
}
void Subscript::summarize(std::ostream &os) const { os << "Subscript"; }
} // namespace expr
} // namespace ast
