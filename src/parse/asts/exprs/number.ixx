module;
#include <memory>
export module ast.exprs.number;
import utils;
import ast.def;
namespace ast {
namespace expr {
export struct Number final : AstBase {
	using uint_t = std::uintmax_t;
	using float_t = long double;
	utils::variant<uint_t, float_t> value;
	Number(const Location &location, uint_t integer);
	Number(const Location &location, float_t fp);
	Number(const Number &other);
	Number(Number &&other);
	Number &operator=(const Number &other);
	Number &operator=(Number &&other) noexcept;
	void summarize(std::ostream &os) const override;
};
} // namespace expr
} // namespace ast