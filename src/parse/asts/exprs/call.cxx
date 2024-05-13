module;
#include <memory>
#include <string_view>
#include <utility>
module ast.exprs.call;
import ast;
import operators;
import utils;

namespace ast {
namespace expr {
Call::Call(const Location &location, ExprPtr callee, std::vector<ExprAst> arguments)
	: AstBase(location), callee{std::move(callee)},
	  arguments{std::move(arguments)} {}
Call::Call(const Call &other)
	: AstBase(other), callee{clone(other.callee)},
	  arguments{other.arguments} {}
Call::Call(Call &&other) noexcept = default;
Call &Call::operator=(const Call &other) {
	AstBase::operator=(other);
	callee = clone(other.callee);
	arguments = other.arguments;
	return *this;
}
Call &Call::operator=(Call &&other) noexcept = default;
void Call::children(children_cb cb) const {
	cb(base_ptr(callee.get()));
	for (const auto &arg : arguments) {
		cb(base_ptr(&arg));
	}
}
void Call::summarize(std::ostream &os) const { os << "Call"; }
} // namespace expr
} // namespace ast
