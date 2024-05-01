module;
#include <coroutine>
#include <memory>
#include <ostream>
#include <utility>
export module ast.stmt.expr;
import ast.def;
import utils;
namespace ast {
namespace stmt {
export struct Expr final : AstBase {
	ExprPtr expr;
	Expr(const Location &location, ExprPtr expr);
	Expr(const Expr &other);
	Expr(Expr &&other) noexcept;
	Expr &operator=(const Expr &other);
	Expr &operator=(Expr &&other) noexcept;
	void children(children_cb cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace stmt
} // namespace ast
