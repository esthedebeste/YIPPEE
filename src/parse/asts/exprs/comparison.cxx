module;

#include <memory>
#include <string_view>
#include <utility>
module ast.exprs.comparison;
import ast;


namespace ast::expr {
Comparison::Comparison(const Location &location,
					   std::vector<operators::comparison> ops,
					   std::vector<ExprAst> operands)
	: AstBase(location), ops{std::move(ops)}, operands{std::move(operands)} {}
Comparison::Comparison(const Comparison &other) = default;
Comparison::Comparison(Comparison &&other) = default;
Comparison &Comparison::operator=(const Comparison &other) = default;
Comparison &Comparison::operator=(Comparison &&other) noexcept = default;
Comparison::~Comparison() {}
void Comparison::children(children_cb cb) const {
	for (auto &expr : operands)
		cb(base_ptr(&expr));
}
void Comparison::summarize(std::ostream &os) const {
	os << "Comparison(";
	for (size_t i = 0; i < ops.size(); ++i) {
		os << ops[i];
		if (i != ops.size() - 1) {
			os << ", ";
		}
	}
	os << ")";
}
} // namespace ast::expr
