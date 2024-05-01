module;
#include <optional>
#include <ostream>
#include <string>
export module ast.type_argument;
import ast.def;

export namespace ast {
struct TypeArgument final : AstBase {
	std::string name;
	std::optional<TypePtr> default_type;
	TypeArgument(const Location &location, std::string name, std::optional<TypePtr> default_type);
	TypeArgument(const TypeArgument &other);
	TypeArgument(TypeArgument &&other);
	TypeArgument &operator=(const TypeArgument &other);
	TypeArgument &operator=(TypeArgument &&other) noexcept;
	void summarize(std::ostream &os) const override;
	void children(children_cb) const override;
};
AnyAstPtr base_ptr(const TypeArgument *ptr);
} // namespace ast