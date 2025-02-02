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
std::string FullName::mangle() const {
	std::string str{};
	for (auto &ns : namespaces) {
		str += ns;
		str += "__";
	}
	str += final;
	str += "_";
	return str;
}
} // namespace naming