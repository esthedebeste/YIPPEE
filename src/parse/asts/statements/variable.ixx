module;
#include <coroutine>

#include <memory>
#include <optional>
#include <ostream>
#include <utility>
export module ast.stmt.variable;
import ast.def;
import utils;
namespace ast {
namespace stmt {
export struct Variable final : AstBase {
	std::string name;
	std::optional<TypePtr> type;
	ExprPtr expr;
	Variable(const Location &location, std::string name, std::optional<TypePtr> type,
			 ExprPtr expr);
	Variable(const Variable &other);
	Variable(Variable &&other) noexcept;
	Variable &operator=(const Variable &other);
	Variable &operator=(Variable &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace stmt
} // namespace ast
