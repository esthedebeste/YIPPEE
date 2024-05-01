module;
#include <string>
#include <string_view>
#include <vector>
module naming;
import type;

namespace naming {
std::ostream &operator<<(std::ostream &stream, const FullName &name) {
	for (auto &ns : name.namespaces)
		stream << ns << "::";
	stream << name.final;
	return stream;
}
std::string mangle(const FullName &name) {
	std::string str{};
	for (auto &ns : name.namespaces)
		str += ns + "__";
	str += name.final;
	str += "_";
	return str;
}
std::string mangle(const type::Array &array) {
	std::string str{"a"};
	str += mangle(*array.member);
	str += "_";
	str += std::to_string(array.size);
	str += "_";
	return str;
}
std::string mangle(const type::NamedStruct &named_struct) {
	std::string str{"n"};
	str += mangle(named_struct.name);
	for (auto &template_arg : named_struct.template_args) {
		str += "_";
		str += mangle(template_arg);
	}
	str += "__";
	return str;
}
std::string mangle(const type::Function &function) {
	std::string str{"f"};
	str += mangle(*function.return_type);
	for (auto &parameter : function.parameters) {
		str += "_";
		str += mangle(parameter);
	}
	str += "__";
	return str;
}
std::string mangle(const type::Pointer &pointer) {
	return 'p' + mangle(*pointer.pointed);
}
std::string mangle(const type::Primitive &primitive) {
	return 'P' + std::string(primitive.mangle_name());
}
std::string mangle(const type::Type &type) {
	std::string prefix{};
	if (type.is_const)
		prefix += 'C';
	if (type.is_ref)
		prefix += 'R';
	return type.visit([](const auto &t) { return mangle(t); });
}
std::string mangle(const FullName &full_name, const type::Type &type) {
	return mangle(full_name) + mangle(type);
}
} // namespace naming