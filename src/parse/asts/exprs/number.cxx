module;
#include <memory>
module ast.exprs.number;
import utils;
import ast;

namespace ast::expr {
Number::Number(const Location &location, uint_t integer)
	: AstBase(location), value{integer} {}
Number::Number(const Location &location, float_t fp) : AstBase(location), value{fp} {}
Number::Number(const Number &other) = default;
Number::Number(Number &&other) = default;
Number &Number::operator=(const Number &other) = default;
Number &Number::operator=(Number &&other) noexcept = default;
void Number::summarize(std::ostream &os) const {
	os << "Number(";
	value.visit([&](auto &&v) { os << v; });
	os << ")";
}
} // namespace ast::expr