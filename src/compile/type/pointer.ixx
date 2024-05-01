module;
#include <memory>
#include <unordered_map>
export module type.pointer;

import utils;
import type.decl;

export namespace type {
struct Pointer final {
	std::unique_ptr<Type> pointed;
	explicit Pointer(std::unique_ptr<Type> pointed);
	Pointer(const Pointer &);
	Pointer &operator=(const Pointer &);
	std::strong_ordering operator<=>(const Pointer &other) const;
	bool operator==(const Pointer &other) const;
};
} // namespace type
