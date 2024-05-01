module;
#include <memory>
#include <vector>
export module ast.exprs.call;
import ast.def;
import utils;
namespace ast {
namespace expr {
export struct Call final : AstBase {
	ExprPtr callee;
	std::vector<ExprAst> arguments;
	Call(const Location &location, ExprPtr callee, std::vector<ExprAst> arguments);
	Call(const Call &other);
	Call(Call &&other);
	Call &operator=(const Call &other);
	Call &operator=(Call &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace expr
} // namespace ast
