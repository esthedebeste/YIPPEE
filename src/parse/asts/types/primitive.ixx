module;

#include <ostream>
export module ast.type.primitive;
export import type.primitive;
import ast.def;

export namespace ast {
namespace type {
struct Primitive final : AstBase {
	::type::Primitive prim;
	Primitive(const Location &location, ::type::Primitive prim);
	Primitive(const Primitive &other);
	Primitive(Primitive &&other) noexcept;
	Primitive &operator=(const Primitive &other);
	Primitive &operator=(Primitive &&other) noexcept;
	void summarize(std::ostream &os) const override;
};
} // namespace type
} // namespace ast