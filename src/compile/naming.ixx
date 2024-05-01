module;
#include <compare>
#include <ostream>
#include <string>
#include <string_view>
#include <vector>
export module naming;
import type.decl;

export namespace naming {
struct FullName {
	std::vector<std::string> namespaces;
	std::string final;
	std::strong_ordering operator<=>(const FullName &other) const {
		if (auto cmp = namespaces <=> other.namespaces; cmp != 0)
			return cmp;
		return final <=> other.final;
	}
	bool operator==(const FullName &other) const {
		return namespaces == other.namespaces && final == other.final;
	}
};
std::ostream &operator<<(std::ostream &, const FullName &);
std::string mangle(const FullName &name);
std::string mangle(const type::Array &named_struct);
std::string mangle(const type::NamedStruct &named_struct);
std::string mangle(const type::Function &function);
std::string mangle(const type::Pointer &pointer);
std::string mangle(const type::Primitive &primitive);
std::string mangle(const type::Type &type);
std::string mangle(const FullName &full_name, const type::Type &type);
} // namespace naming