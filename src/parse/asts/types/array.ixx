module;

#include <memory>
#include <ostream>
export module ast.type.array;
import ast.def;

export namespace ast {
namespace type {
struct Array final : AstBase {
	TypePtr member;
	std::uintmax_t size;
	Array(const Location &location, TypePtr member, std::uintmax_t size);
	Array(const Array &other);
	Array(Array &&other) noexcept;
	Array &operator=(const Array &other);
	Array &operator=(Array &&other) noexcept;
	~Array() override;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace type
} // namespace ast