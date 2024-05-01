module;
#include <coroutine>
#include <memory>
#include <optional>
#include <ostream>
#include <utility>
export module ast.stmt.while_;
import ast.def;
import utils;
namespace ast {
namespace stmt {
export struct While final : AstBase {
	ExprPtr expr;
	StatementPtr body;
	While(const Location &location, ExprPtr expr, StatementPtr body);
	While(const While &other);
	While(While &&other) noexcept;
	While &operator=(const While &other);
	While &operator=(While &&other) noexcept;
	void children(children_cb cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace stmt
} // namespace ast
