module;
#include <memory>
#include <vector>
module type.function;
import type;
import utils;

namespace type {
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
} // namespace type