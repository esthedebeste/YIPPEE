module;
#include <ostream>
#include <string>
export module ast.type.named;
import ast.def;
import ast.name;

export namespace ast {
namespace type {
struct Named final : AstBase {
	Identifier name;
	std::vector<TypeAst> arguments;
	Named(const Location &location, Identifier name, std::vector<TypeAst> arguments);
	Named(const Named &);
	Named(Named &&) noexcept;
	Named &operator=(const Named &);
	Named &operator=(Named &&) noexcept;
	void summarize(std::ostream &os) const override;
};
} // namespace type
} // namespace ast