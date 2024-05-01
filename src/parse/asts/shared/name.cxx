module;
#include <ostream>
#include <string>
#include <vector>
module ast.name;
import ast;
namespace ast {
Name::Name(const Location &location, std::string str)
	: AstBase{location}, str{std::move(str)} {}
std::ostream &operator<<(std::ostream &stream, const Name &name) {
	return stream << name.str;
}
void Name::summarize(std::ostream &os) const { os << "Name(" << *this << ")"; }
AnyAstPtr base_ptr(const Name *ptr) { return {ptr}; }

Identifier::Identifier(const Location &location, std::vector<Name> parts, Name final)
	: AstBase{location}, parts{std::move(parts)}, final{std::move(final)} {}
std::ostream &operator<<(std::ostream &stream, const Identifier &name) {
	for (const auto &part : name.parts)
		stream << part << "::";
	stream << name.final;
	return stream;
}
void Identifier::summarize(std::ostream &os) const {
	os << "Identifier(" << *this << ")";
}
} // namespace ast