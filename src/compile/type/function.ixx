module;
#include <memory>
#include <vector>
export module type.function;
import utils;
import type.decl;

export namespace type {
struct Function {
	std::vector<Type> parameters;
	std::unique_ptr<Type> return_type;
	explicit Function(std::vector<Type> parameters,
					  std::unique_ptr<Type> return_type);
	explicit Function(std::vector<Type> parameters, Type return_type);
	Function(const Function &);
	Function &operator=(const Function &);
	std::strong_ordering operator<=>(const Function &) const;
	bool operator==(const Function &other) const;
};
} // namespace type