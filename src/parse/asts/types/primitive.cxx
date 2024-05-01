module;

#include <ostream>
module ast.type.primitive;
import type.primitive;
import ast;

namespace ast {
namespace type {
Primitive::Primitive(const Location &location, const ::type::Primitive prim)
	: AstBase(location), prim{prim} {}
Primitive::Primitive(const Primitive &other) = default;
Primitive::Primitive(Primitive &&other) noexcept = default;
Primitive &Primitive::operator=(const Primitive &other) = default;
Primitive &Primitive::operator=(Primitive &&other) noexcept = default;
void Primitive::summarize(std::ostream &os) const {
	os << "Primitive(" << prim.name() << ')';
}
} // namespace type
} // namespace ast