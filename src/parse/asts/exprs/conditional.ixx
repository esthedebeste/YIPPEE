module;
#include <coroutine>

#include <memory>
#include <utility>
export module ast.exprs.conditional;
import ast.def;
import utils;
namespace ast {
namespace expr {
export struct Conditional final : AstBase {
	ExprPtr condition, thenExpr, elseExpr;
	Conditional(const Location &location, ExprPtr condition, ExprPtr thenExpr,
				ExprPtr elseExpr);
	Conditional(const Conditional &other);
	Conditional(Conditional &&other);
	Conditional &operator=(const Conditional &other);
	Conditional &operator=(Conditional &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace expr
} // namespace ast
