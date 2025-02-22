module;
#include <compare>
export module operations;
import type;
import operators;

export namespace operation {
struct Binary {
	type::Type left, right;
	operators::binary op;
	std::strong_ordering operator<=>(const Binary &) const = default;
};
struct Comparison {
	type::Type left, right;
	operators::comparison op;
	std::strong_ordering operator<=>(const Comparison &) const = default;
};
struct Unary {
	type::Type operand;
	operators::unary op;
	std::strong_ordering operator<=>(const Unary &) const = default;
};
} // namespace operation