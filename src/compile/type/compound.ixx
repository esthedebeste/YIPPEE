module;
#include <compare>
#include <string>
#include <vector>
export module type.compound;
import utils;
import type.decl;
import naming;

export namespace type {
struct NamedStruct {
	naming::FullName name;
	std::vector<Type> template_args;
	std::vector<std::pair<std::string, Type>> members;
	explicit NamedStruct(naming::FullName name, std::vector<Type> template_args,
						 std::vector<std::pair<std::string, Type>> members);
	std::strong_ordering operator<=>(const NamedStruct &other) const;
	bool operator==(const NamedStruct &other) const;
};
} // namespace type