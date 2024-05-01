module;
#include <memory>
#include <unordered_map>
export module type.array;

import utils;
import type.decl;

export namespace type {
struct Array final {
	std::unique_ptr<Type> member;
	std::uintmax_t size;
	Array(std::unique_ptr<Type> member, std::uintmax_t size);
	Array(const Array &);
	Array &operator=(const Array &);
	std::strong_ordering operator<=>(const Array &other) const;
	bool operator==(const Array &other) const;
};
} // namespace type
