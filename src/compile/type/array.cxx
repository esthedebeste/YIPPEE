module;
#include <memory>
module type.array;
import type;
import utils;

namespace type {
Array::Array(std::unique_ptr<Type> member, const std::uintmax_t size) : member{std::move(member)}, size{size} {}
Array::Array(const Array &other) : member{clone(other.member)}, size{other.size} {}
Array &Array::operator=(const Array &other) {
	member = clone(other.member);
	size = other.size;
	return *this;
}
std::strong_ordering Array::operator<=>(const Array &other) const {
	if (auto cmp = size <=> other.size; cmp != 0)
		return cmp;
	return member->operator<=>(*other.member);
}
bool Array::operator==(const Array &other) const {
	return size == other.size && member == other.member;
}
} // namespace type