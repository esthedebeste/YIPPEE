module;
#include <memory>
#include <vector>
export module ast.exprs.array;
import ast.def;
import utils;
namespace ast {
namespace expr {
export struct Array final : AstBase {
	std::vector<ExprAst> values;
	Array(const Location &location, std::vector<ExprAst> values);
	Array(const Array &other);
	Array(Array &&other);
	Array &operator=(const Array &other);
	Array &operator=(Array &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace expr
} // namespace ast
