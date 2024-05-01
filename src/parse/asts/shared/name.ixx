module;
#include <ostream>
#include <string>
#include <vector>
export module ast.name;
import ast.def;

export namespace ast {
struct Name final : AstBase {
	std::string str;
	explicit Name(const Location &location, std::string str);
	void summarize(std::ostream &os) const override;
};
std::ostream &operator<<(std::ostream &stream, const Name &name);
struct Identifier final : AstBase {
	std::vector<Name> parts;
	Name final;
	explicit Identifier(const Location &location, std::vector<Name> parts, Name final);
	void summarize(std::ostream &os) const override;
};
std::ostream &operator<<(std::ostream &stream, const Identifier &name);
AnyAstPtr base_ptr(const Name *ptr);
} // namespace ast