module;
#include <ostream>
#include <ranges>
export module operators;
export namespace operators {
enum binary {
	assign = 0,
	l_and = 1,
	l_or = 2,
	b_and = 3,
	b_or = 4,
	b_xor = 5,
	b_shl = 6,
	b_shr = 7,
	add = 8,
	sub = 9,
	pow = 10,
	mul = 11,
	div = 12,
	mod = 13,
	MAX_BINARY,
};
extern std::array<std::string_view, MAX_BINARY> binary_strings;
#define ALLFUNC(funcname, max, enumname)                                              \
	inline constexpr auto funcname() {                                                \
		return std::ranges::transform_view(                                           \
				std::ranges::iota_view<int, int>{0, static_cast<int>(enumname::max)}, \
				[](int i) { return static_cast<enumname>(i); });                      \
	}
ALLFUNC(all_binaries, MAX_BINARY, binary)
inline std::string_view string(const binary op) {
	return binary_strings[static_cast<std::size_t>(op)];
}
std::ostream &operator<<(std::ostream &stream, const binary op) {
	return stream << string(op);
}
enum unary {
	neg = 0,
	pos = 1,
	l_not = 2,
	b_not = 3,
	MAX_UNARY,
};
extern std::array<std::string_view, MAX_UNARY> unary_strings;
ALLFUNC(all_unaries, MAX_UNARY, unary)
inline std::string_view string(const unary op) {
	return unary_strings[static_cast<std::size_t>(op)];
}
std::ostream &operator<<(std::ostream &stream, const unary op) {
	return stream << string(op);
}
enum comparison {
	less_eq = 0,
	greater_eq = 1,
	eq_eq = 2,
	not_equal = 3,
	less = 4,
	greater = 5,
	MAX_COMPARISON,
};
extern std::array<std::string_view, MAX_COMPARISON> comparison_strings;
ALLFUNC(all_comparisons, MAX_COMPARISON, comparison)
inline std::string_view string(const comparison op) {
	return comparison_strings[static_cast<std::size_t>(op)];
}
std::ostream &operator<<(std::ostream &stream, const comparison op) {
	return stream << string(op);
}
extern std::array<std::string_view, MAX_BINARY + MAX_UNARY + MAX_COMPARISON> all_strings;
} // namespace operators

module :private;
namespace operators {
std::array<std::string_view, MAX_BINARY> binary_strings{
		"=", "&&", "||", "&", "|", "^", "<<", ">>", "+", "-", "**", "*", "/", "%"};
std::array<std::string_view, MAX_UNARY> unary_strings{"-", "+", "!", "~"};
std::array<std::string_view, MAX_COMPARISON> comparison_strings{
		"<=", ">=", "==", "!=", "<", ">"};
std::array<std::string_view, MAX_BINARY + MAX_UNARY + MAX_COMPARISON> all_strings{
		"&&", "||", "<<", ">>", "**", "<=", ">=", "==", "!=",
		"=", "&", "|", "^", "+", "-", "*", "/", "%", "-", "+", "!", "~", "<", ">"};
} // namespace operators