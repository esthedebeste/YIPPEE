module;

#include <memory>
#include <ostream>
module ast.type.pointer;
import ast;
import utils;

namespace ast {
namespace type {
Pointer::Pointer(const Location &location, TypePtr pointed)
	: AstBase(location), pointed{std::move(pointed)} {}
Pointer::Pointer(const Pointer &other)
	: AstBase(other), pointed{clone(other.pointed)} {}
Pointer::Pointer(Pointer &&other) noexcept = default;
Pointer &Pointer::operator=(const Pointer &other) {
	AstBase::operator=(other);
	pointed = clone(other.pointed);
	return *this;
}
Pointer &Pointer::operator=(Pointer &&other) noexcept = default;
Pointer::~Pointer() {}
void Pointer::children(children_cb cb) const { cb(base_ptr(pointed.get())); }
void Pointer::summarize(std::ostream &os) const { os << "Pointer"; }
} // namespace type
} // namespace ast