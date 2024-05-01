module;
#include <string>
#include <vector>
module type.compound;
import type;

namespace {
std::strong_ordering
operator<=>(const std::pair<std::string, type::Type> &lhs,
			const std::pair<std::string, type::Type> &rhs) {
	if (auto cmp = lhs.first <=> rhs.first; cmp != 0)
		return cmp;
	return lhs.second <=> rhs.second;
}
} // namespace

namespace type {
NamedStruct::NamedStruct(
		naming::FullName name, std::vector<Type> template_args,
		std::vector<std::pair<std::string, Type>> members)
	: name{std::move(name)}, template_args(std::move(template_args)),
	  members{std::move(members)} {}
std::strong_ordering NamedStruct::operator<=>(const NamedStruct &other) const {
	if (auto cmp = name <=> other.name; cmp != 0)
		return cmp;
	if (auto cmp = template_args <=> other.template_args; cmp != 0)
		return cmp;
	return members <=> other.members;
}
bool NamedStruct::operator==(const NamedStruct &other) const {
	return name == other.name && template_args == other.template_args &&
		   members == other.members;
}
} // namespace type