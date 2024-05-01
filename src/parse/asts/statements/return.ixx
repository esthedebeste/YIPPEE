module;
#include <coroutine>
#include <memory>
#include <ostream>
#include <utility>
export module ast.stmt.ret;
import ast.def;
import utils;
namespace ast {
namespace stmt {
export struct Return final : AstBase {
	ExprPtr expr;
	Return(const Location &location, ExprPtr expr);
	Return(const Return &other);
	Return(Return &&other) noexcept;
	Return &operator=(const Return &other);
	Return &operator=(Return &&other) noexcept;
	void children(children_cb cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace stmt
} // namespace ast
