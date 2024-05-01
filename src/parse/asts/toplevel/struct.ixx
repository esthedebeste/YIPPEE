module;
#include <coroutine>

#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <utility>
#include <vector>
export module ast.top.structure;
import ast.def;
import ast.name;
import ast.type_argument;
import utils;
namespace ast {
namespace top {
export struct Struct final : AstBase {
	Identifier name;
	std::vector<TypeArgument> type_arguments;
	using Field = std::pair<std::string, TypeAst>;
	std::vector<Field> members;
	Struct(const Location &location, Identifier name,
		   std::vector<TypeArgument> type_arguments, std::vector<Field> members);
	Struct(const Struct &other);
	Struct(Struct &&other);
	Struct &operator=(const Struct &other);
	Struct &operator=(Struct &&other) noexcept;
	~Struct() override;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace top
} // namespace ast
