module;
#include <memory>
module type.pointer;
import type;
import utils;

namespace type {
Pointer::Pointer(std::unique_ptr<Type> pointed) : pointed{std::move(pointed)} {}
Pointer::Pointer(const Pointer &other) : pointed{clone(other.pointed)} {}
Pointer &Pointer::operator=(const Pointer &other) {
	pointed = clone(other.pointed);
	return *this;
}
std::strong_ordering Pointer::operator<=>(const Pointer &other) const {
	return pointed->operator<=>(*other.pointed);
}
bool Pointer::operator==(const Pointer &other) const {
	return pointed->operator==(*other.pointed);
}
} // namespace type