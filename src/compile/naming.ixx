module;
#include <compare>
#include <ostream>
#include <string>
#include <string_view>
#include <vector>
export module naming;

export namespace naming {
struct FullName {
	std::vector<std::string> namespaces;
	std::string final;
	constexpr std::strong_ordering operator<=>(const FullName &other) const {
		if (const auto cmp = namespaces <=> other.namespaces; cmp != 0)
			return cmp;
		return final <=> other.final;
	}
	constexpr bool operator==(const FullName &other) const {
		return namespaces == other.namespaces && final == other.final;
	}

	[[nodiscard]] std::string mangle() const;
};
std::ostream &operator<<(std::ostream &, const FullName &);
} // namespace naming