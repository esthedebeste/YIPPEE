module;
#include <coroutine>
#include <memory>
#include <string_view>
#include <utility>
module ast.exprs.binop;
import ast;
import utils;

namespace ast {
namespace expr {
Binop::Binop(const Location &location, ExprPtr left, const operators::binary op,
			 ExprPtr right)
	: AstBase(location), op{op}, left{std::move(left)},
	  right{std::move(right)} {}
Binop::Binop(const Binop &other)
	: AstBase(other), op{other.op}, left{clone(other.left)},
	  right{clone(other.right)} {}
Binop::Binop(Binop &&other) = default;
Binop &Binop::operator=(const Binop &other) {
	AstBase::operator=(other);
	op = other.op;
	left = clone(other.left);
	right = clone(other.right);
	return *this;
}
Binop &Binop::operator=(Binop &&other) noexcept = default;
void Binop::children(children_cb cb) const {
	cb(base_ptr(left.get()));
	cb(base_ptr(right.get()));
}

void Binop::summarize(std::ostream &os) const {
	if (op == operators::binary::assign)
		os << "Assignment";
	else
		os << "Binop(" << op << ")";
}
} // namespace expr
} // namespace ast
