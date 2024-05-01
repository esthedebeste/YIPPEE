module;
#include <string>
#include <vector>
export module ast.exprs.identifier;
import utils;
import ast.def;
import ast.name;
namespace ast {
namespace expr {
export struct Identifier final : AstBase {
	ast::Identifier value;
	std::vector<TypeAst> type_arguments;
	Identifier(const Location &location, ast::Identifier value, std::vector<TypeAst> type_arguments);
	Identifier(const Identifier &other);
	Identifier(Identifier &&other);
	Identifier &operator=(const Identifier &other);
	Identifier &operator=(Identifier &&other) noexcept;
	void summarize(std::ostream &os) const override;
};
} // namespace expr
} // namespace ast