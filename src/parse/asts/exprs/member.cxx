module;
#include <memory>
#include <string_view>
#include <utility>
module ast.exprs.member;
import ast;
import operators;
import utils;

namespace ast {
namespace expr {
Member::Member(const Location &location, ExprPtr expr, std::string name)
	: AstBase(location), expr{std::move(expr)}, name{std::move(name)} {}
Member::Member(const Member &other)
	: AstBase(other), expr{utils::clone(other.expr)}, name{other.name} {}
Member::Member(Member &&other) = default;
Member &Member::operator=(const Member &other) {
	AstBase::operator=(other);
	expr = utils::clone(other.expr);
	name = other.name;
	return *this;
}
Member &Member::operator=(Member &&other) noexcept = default;
void Member::children(children_cb cb) const { cb(base_ptr(expr.get())); }
void Member::summarize(std::ostream &os) const {
	os << "Member(" << name << ")";
}
} // namespace expr
} // namespace ast
