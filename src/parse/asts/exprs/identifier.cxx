module;
#include <memory>
#include <ostream>
module ast.exprs.identifier;
import utils;
import ast;

namespace ast::expr {
Identifier::Identifier(const Location &location, ast::Identifier value, std::vector<TypeAst> type_arguments)
	: AstBase(location), value{std::move(value)}, type_arguments{std::move(type_arguments)} {}
Identifier::Identifier(const Identifier &other) = default;
Identifier::Identifier(Identifier &&other) = default;
Identifier &Identifier::operator=(const Identifier &other) = default;
Identifier &Identifier::operator=(Identifier &&other) noexcept = default;

void Identifier::summarize(std::ostream &os) const {
	os << "Identifier(" << value << ")";
}
} // namespace ast::expr