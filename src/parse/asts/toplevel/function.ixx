
module;
#include <coroutine>

#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <utility>
#include <vector>
export module ast.top.function;
import ast.def;
import ast.name;
import ast.stmt.block;
import ast.type_argument;
namespace ast {
namespace top {
export struct Function final : AstBase {
	Identifier name;
	std::vector<TypeArgument> type_arguments;
	using Parameter = std::pair<Name, TypeAst>;
	std::vector<Parameter> parameters;
	TypePtr return_type;
	StatementPtr statement;
	Function(const Location &location, Identifier name,
			 std::vector<TypeArgument> type_arguments,
			 std::vector<Parameter> parameters, TypePtr return_type,
			 StatementPtr statement);
	Function(const Function &other);
	Function(Function &&other) noexcept;
	Function &operator=(const Function &other);
	Function &operator=(Function &&other) noexcept;
	~Function() override;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace top
} // namespace ast
