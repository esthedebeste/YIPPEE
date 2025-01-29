module;
#include <memory>
#include <vector>
#include <string>
module type;

import utils;
import naming;

namespace {
std::strong_ordering
operator<=>(const std::pair<std::string, type::Type> &lhs,
			const std::pair<std::string, type::Type> &rhs) {
	if (const auto cmp = lhs.first <=> rhs.first; cmp != 0)
		return cmp;
	return lhs.second <=> rhs.second;
}
} // namespace
namespace type {

Array::Array(std::unique_ptr<Type> member, const std::uintmax_t size) : member{std::move(member)}, size{size} {}
Array::Array(const Array &other) : member{clone(other.member)}, size{other.size} {}
Array &Array::operator=(const Array &other) {
	member = clone(other.member);
	size = other.size;
	return *this;
}
std::strong_ordering Array::operator<=>(const Array &other) const {
	if (const auto cmp = size <=> other.size; cmp != 0)
		return cmp;
	return member->operator<=>(*other.member);
}
bool Array::operator==(const Array &other) const {
	return size == other.size && member == other.member;
}
std::string Array::mangle() const {
	std::string str{"a"};
	str += member->mangle();
	str += "_";
	str += std::to_string(size);
	str += "_";
	return str;
}
NamedStruct::NamedStruct(
		naming::FullName name, std::vector<Type> template_args,
		std::vector<std::pair<std::string, Type>> members)
	: name{std::move(name)}, template_args(std::move(template_args)),
	  members{std::move(members)} {}
std::strong_ordering NamedStruct::operator<=>(const NamedStruct &other) const {
	if (const auto cmp = name <=> other.name; cmp != 0)
		return cmp;
	if (const auto cmp = template_args <=> other.template_args; cmp != 0)
		return cmp;
	return members <=> other.members;
}
bool NamedStruct::operator==(const NamedStruct &other) const {
	return name == other.name && template_args == other.template_args &&
		   members == other.members;
}
std::string NamedStruct::mangle() const {
	std::string str{"n"};
	str += name.mangle();
	for (auto &template_arg : template_args) {
		str += "_";
		str += template_arg.mangle();
	}
	str += "__";
	return str;
}
Function::Function(std::vector<Type> parameters,
				   std::unique_ptr<Type> return_type)
	: parameters(std::move(parameters)), return_type{std::move(return_type)} {}
Function::Function(std::vector<Type> parameters, Type return_type)
	: parameters(std::move(parameters)),
	  return_type{std::make_unique<Type>(return_type)} {}
Function::Function(const Function &other) : parameters(other.parameters), return_type(clone(other.return_type)) {}
Function &Function::operator=(const Function &other) {
	parameters = other.parameters;
	return_type = clone(other.return_type);
	return *this;
}
std::strong_ordering Function::operator<=>(const Function &other) const {
	const auto params = parameters <=> other.parameters;
	if (params != 0)
		return params;
	return *return_type <=> *other.return_type;
}
bool Function::operator==(const Function &other) const {
	return parameters == other.parameters && *return_type == *other.return_type;
}
std::string Function::mangle() const {
	std::string str{"f"};
	str += return_type->mangle();
	for (auto &parameter : parameters) {
		str += "_";
		str += parameter.mangle();
	}
	str += "__";
	return str;
}
Pointer::Pointer(std::unique_ptr<Type> pointed) : pointed{std::move(pointed)} {}
Pointer::Pointer(const Pointer &other) : pointed{clone(other.pointed)} {}
Pointer &Pointer::operator=(const Pointer &other) {
	pointed = clone(other.pointed);
	return *this;
}
std::strong_ordering Pointer::operator<=>(const Pointer &other) const {
	return pointed->operator<=>(*other.pointed);
}
bool Pointer::operator==(const Pointer &other) const {
	return pointed->operator==(*other.pointed);
}
std::string Pointer::mangle() const {
	return 'p' + pointed->mangle();
}
std::string Primitive::mangle() const {
	return 'P' + std::string(mangle_name());
}
std::string Type::mangle() const {
	std::string prefix{};
	if (is_const)
		prefix += 'C';
	if (is_ref)
		prefix += 'R';
	return prefix + visit([](const auto &t) { return t.mangle(); });
}
} // namespace type