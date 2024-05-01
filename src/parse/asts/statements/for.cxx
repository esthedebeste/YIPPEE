module;
#include <coroutine>

#include <memory>
#include <ostream>
#include <utility>
module ast.stmt.for_;
import ast;
import utils;
namespace ast {
namespace stmt {
For::For(const Location &location, std::optional<StatementPtr> init,
		 std::optional<ExprPtr> cond, std::optional<ExprPtr> incr,
		 StatementPtr body)
	: AstBase(location), init{std::move(init)}, cond{std::move(cond)},
	  incr{std::move(incr)}, body{std::move(body)} {}
For::For(const For &other)
	: AstBase(other), init{clone(other.init)}, cond{clone(other.cond)},
	  incr{clone(other.incr)}, body{clone(other.body)} {}
For::For(For &&other) noexcept = default;
For &For::operator=(const For &other) {
	AstBase::operator=(other);
	init = clone(other.init);
	cond = clone(other.cond);
	incr = clone(other.incr);
	body = clone(other.body);
	return *this;
}
For &For::operator=(For &&other) noexcept = default;
void For::children(children_cb cb) const {
	if (init)
		cb(base_ptr(init->get()));
	if (cond)
		cb(base_ptr(cond->get()));
	if (incr)
		cb(base_ptr(incr->get()));
	cb(base_ptr(body.get()));
}
void For::summarize(std::ostream &os) const { os << "ForStatement"; }
} // namespace stmt
} // namespace ast
