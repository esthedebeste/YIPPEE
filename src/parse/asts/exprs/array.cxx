module;
#include <memory>
#include <string_view>
#include <utility>
module ast.exprs.array;
import ast;
import operators;
import utils;

namespace ast {
namespace expr {
Array::Array(const Location &location, std::vector<ExprAst> values)
	: AstBase(location), values{std::move(values)} {}
Array::Array(const Array &other) = default;
Array::Array(Array &&other) = default;
Array &Array::operator=(const Array &other) = default;
Array &Array::operator=(Array &&other) noexcept = default;
void Array::children(children_cb cb) const {
	for (const auto &value : values)
		cb(base_ptr(&value));
}
void Array::summarize(std::ostream &os) const { os << "Array(" << values.size() << ")"; }
} // namespace expr
} // namespace ast
