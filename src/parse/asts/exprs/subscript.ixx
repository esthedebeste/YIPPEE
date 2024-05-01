module;
#include <memory>
export module ast.exprs.subscript;
import ast.def;
import utils;
namespace ast {
namespace expr {
export struct Subscript final : AstBase {
	ExprPtr expr, index;
	Subscript(const Location &location, ExprPtr expr, ExprPtr index);
	Subscript(const Subscript &other);
	Subscript(Subscript &&other);
	Subscript &operator=(const Subscript &other);
	Subscript &operator=(Subscript &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace expr
} // namespace ast
