module;
#include <coroutine>

#include <memory>
#include <ostream>
#include <utility>
module ast.stmt.if_;
import ast;
import utils;
namespace ast {
namespace stmt {
If::If(const Location &location, ExprPtr condition, StatementPtr then,
	   std::optional<StatementPtr> otherwise)
	: AstBase(location), condition{std::move(condition)}, then{std::move(then)},
	  otherwise{std::move(otherwise)} {}
If::If(const If &other)
	: AstBase(other), condition{clone(other.condition)}, then{clone(other.then)},
	  otherwise{clone(other.otherwise)} {}
If::If(If &&other) noexcept = default;
If &If::operator=(const If &other) {
	AstBase::operator=(other);
	condition = clone(other.condition);
	then = clone(other.then);
	otherwise = clone(other.otherwise);
	return *this;
}
If &If::operator=(If &&other) noexcept = default;
void If::children(children_cb cb) const {
	cb(base_ptr(condition.get()));
	cb(base_ptr(then.get()));
	if (otherwise)
		cb(base_ptr(otherwise->get()));
}
void If::summarize(std::ostream &os) const { os << "IfStatement"; }
} // namespace stmt
} // namespace ast
