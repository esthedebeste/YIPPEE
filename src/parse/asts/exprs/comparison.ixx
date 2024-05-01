module;
#include <coroutine>

#include <memory>
#include <utility>
#include <vector>
export module ast.exprs.comparison;
import ast.def;
import utils;
import operators;
namespace ast {
namespace expr {
export struct Comparison final : AstBase {
	std::vector<operators::comparison> ops;
	std::vector<ExprAst> operands;
	Comparison(const Location &location, std::vector<operators::comparison> ops,
			   std::vector<ExprAst> operands);
	Comparison(const Comparison &other);
	Comparison(Comparison &&other);
	Comparison &operator=(const Comparison &other);
	Comparison &operator=(Comparison &&other) noexcept;
	~Comparison() override;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace expr
} // namespace ast
