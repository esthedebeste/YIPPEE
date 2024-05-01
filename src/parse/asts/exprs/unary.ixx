module;
#include <coroutine>

#include <memory>
#include <utility>
export module ast.exprs.unary;
import ast.def;
import utils;
import operators;
namespace ast {
namespace expr {
export struct Unary final : AstBase {
	operators::unary op;
	ExprPtr expr;
	Unary(const Location &location, const operators::unary op, ExprPtr expr);
	Unary(const Unary &other);
	Unary(Unary &&other);
	Unary &operator=(const Unary &other);
	Unary &operator=(Unary &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace expr
} // namespace ast
