module;
#include <coroutine>
#include <memory>
#include <optional>
#include <ostream>
#include <utility>
export module ast.stmt.for_;
import ast.def;
import utils;
namespace ast {
namespace stmt {
export struct For final : AstBase {
	std::optional<StatementPtr> init;
	std::optional<ExprPtr> cond, incr;
	StatementPtr body;
	For(const Location &location, std::optional<StatementPtr> init,
		std::optional<ExprPtr> cond, std::optional<ExprPtr> incr,
		StatementPtr body);
	For(const For &other);
	For(For &&other) noexcept;
	For &operator=(const For &other);
	For &operator=(For &&other) noexcept;
	void children(children_cb cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace stmt
} // namespace ast
