module;
#include <memory>
#include <ostream>
export module ast.exprs.binop;
import ast.def;
import utils;
import operators;
namespace ast {
namespace expr {
export struct Binop final : AstBase {
	operators::binary op;
	ExprPtr left, right;
	Binop(const Location &location, ExprPtr left, operators::binary op, ExprPtr right);
	Binop(const Binop &other);
	Binop(Binop &&other);
	Binop &operator=(const Binop &other);
	Binop &operator=(Binop &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace expr
} // namespace ast
