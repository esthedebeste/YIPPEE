module;
#include <ostream>
#include <string>
#include <vector>
module ast.type_argument;
import utils;
import ast;
namespace ast {
TypeArgument::TypeArgument(const Location &location, std::string name, std::optional<TypePtr> default_type)
	: AstBase(location), name(std::move(name)), default_type(std::move(default_type)) {}
TypeArgument::TypeArgument(const TypeArgument &other) : AstBase(other), name(other.name), default_type(clone(other.default_type)) {}
TypeArgument::TypeArgument(TypeArgument &&other) = default;
TypeArgument &TypeArgument::operator=(const TypeArgument &other) {
	AstBase::operator=(other);
	name = other.name;
	default_type = clone(other.default_type);
	return *this;
}
TypeArgument &TypeArgument::operator=(TypeArgument &&other) noexcept = default;
void TypeArgument::summarize(std::ostream &os) const { os << "TypeArgument(" << name << ")"; }
void TypeArgument::children(children_cb cb) const {
	if (default_type)
		cb(base_ptr(default_type->get()));
}
AnyAstPtr base_ptr(const TypeArgument *ptr) { return {ptr}; }
} // namespace ast