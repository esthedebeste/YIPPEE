module;

#include <memory>
#include <ostream>
module ast.type.array;
import ast;
import utils;

namespace ast {
namespace type {
Array::Array(const Location &location, TypePtr member, std::uintmax_t size)
	: AstBase(location), member{std::move(member)}, size{size} {}
Array::Array(const Array &other)
	: AstBase(other), member{clone(other.member)}, size{other.size} {}
Array::Array(Array &&other) noexcept = default;
Array &Array::operator=(const Array &other) {
	AstBase::operator=(other);
	member = clone(other.member);
	size = other.size;
	return *this;
}
Array &Array::operator=(Array &&other) noexcept = default;
Array::~Array() {}
void Array::children(children_cb cb) const { cb(base_ptr(member.get())); }
void Array::summarize(std::ostream &os) const { os << "Array(" << size << ")"; }
} // namespace type
} // namespace ast