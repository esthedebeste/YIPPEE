module;
#include <memory>
#include <optional>
#include <ostream>
export module ast.stmt.if_;
import ast.def;
import utils;
namespace ast {
namespace stmt {
export struct If final : AstBase {
	ExprPtr condition;
	StatementPtr then;
	std::optional<StatementPtr> otherwise;
	If(const Location &location, ExprPtr condition, StatementPtr then,
	   std::optional<StatementPtr> otherwise);
	If(const If &other);
	If(If &&other) noexcept;
	If &operator=(const If &other);
	If &operator=(If &&other) noexcept;
	void children(children_cb cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace stmt
} // namespace ast
