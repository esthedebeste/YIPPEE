module;
#include <coroutine>
#include <optional>
#include <ostream>
#include <string>
#include <utility>
#include <vector>
module ast.top.structure;
import ast;
import utils;
namespace ast {
namespace top {
Struct::Struct(const Location &location, Identifier name,
			   std::vector<TypeArgument> type_arguments,
			   std::vector<Field> members)
	: AstBase(location), name{std::move(name)},
	  type_arguments(std::move(type_arguments)), members(std::move(members)) {}

Struct::Struct(const Struct &other) = default;
Struct::Struct(Struct &&other) = default;
Struct &Struct::operator=(const Struct &other) = default;
Struct &Struct::operator=(Struct &&other) noexcept = default;
Struct::~Struct() {}
void Struct::children(children_cb cb) const {
	for (auto &argument : type_arguments)
		if (argument.default_type)
			cb(base_ptr(argument.default_type->get()));
	for (auto &member : members)
		cb(base_ptr(&member.second));
}
void Struct::summarize(std::ostream &os) const {
	os << "Struct(" << name;
	size_t i = 0;
	os << '<';
	for (auto &argument : type_arguments)
		os << (i++ == 0 ? "" : ", ") << argument.name;
	i = 0;
	os << ">{";
	for (auto &member : members)
		os << (i++ == 0 ? "" : ", ") << member.first;
	os << "})";
}
} // namespace top
} // namespace ast
