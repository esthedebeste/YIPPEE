module;

#include <memory>
#include <ostream>
export module ast.type.pointer;
import ast.def;

export namespace ast {
namespace type {
struct Pointer final : AstBase {
	TypePtr pointed;
	Pointer(const Location &location, TypePtr pointed);
	Pointer(const Pointer &other);
	Pointer(Pointer &&other) noexcept;
	Pointer &operator=(const Pointer &other);
	Pointer &operator=(Pointer &&other) noexcept;
	~Pointer() override;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace type
} // namespace ast