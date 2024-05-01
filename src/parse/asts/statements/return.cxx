module;
#include <coroutine>
#include <memory>
#include <ostream>
#include <utility>
module ast.stmt.ret;
import ast;
import utils;
namespace ast {
namespace stmt {
Return::Return(const Location &location, ExprPtr expr)
	: AstBase(location), expr{std::move(expr)} {}
Return::Return(const Return &other)
	: AstBase(other), expr{clone(other.expr)} {}
Return::Return(Return &&other) noexcept = default;
Return &Return::operator=(const Return &other) {
	AstBase::operator=(other);
	expr = clone(other.expr);
	return *this;
}
Return &Return::operator=(Return &&other) noexcept = default;
void Return::children(children_cb cb) const { cb(base_ptr(expr.get())); }
void Return::summarize(std::ostream &os) const { os << "Return"; }
} // namespace stmt
} // namespace ast
