module;
#include <string>
#include <vector>
export module ast.exprs.create;
import utils;
import ast.def;
namespace ast {
namespace expr {
export struct Create final : AstBase {
	TypePtr type;
	std::vector<std::pair<std::string, ExprAst>> args;
	Create(const Location &location, TypePtr type,
		   std::vector<std::pair<std::string, ExprAst>> args);
	Create(const Create &other);
	Create(Create &&other);
	Create &operator=(const Create &other);
	Create &operator=(Create &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace expr
} // namespace ast