module;
#include <memory>
#include <string>
export module ast.exprs.member;
import ast.def;
import utils;
namespace ast {
namespace expr {
export struct Member final : AstBase {
	ExprPtr expr;
	std::string name;
	Member(const Location &location, ExprPtr expr, std::string name);
	Member(const Member &other);
	Member(Member &&other);
	Member &operator=(const Member &other);
	Member &operator=(Member &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace expr
} // namespace ast
