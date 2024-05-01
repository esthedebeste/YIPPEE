module;

#include <memory>
#include <ostream>
module ast.type.named;
import ast;

namespace ast {
namespace type {
Named::Named(const Location &location, Identifier name, std::vector<TypeAst> arguments)
	: AstBase(location), name{std::move(name)},
	  arguments{std::move(arguments)} {}
Named::Named(const Named &named) = default;
Named::Named(Named &&) noexcept = default;
Named &Named::operator=(const Named &) = default;
Named &Named::operator=(Named &&) noexcept = default;
void Named::summarize(std::ostream &os) const {
	os << "NamedType(" << name << ')';
}
} // namespace type
} // namespace ast