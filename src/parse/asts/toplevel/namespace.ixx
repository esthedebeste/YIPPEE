module;
#include <coroutine>

#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <utility>
#include <vector>
export module ast.top.space;
import ast.def;
import ast.name;
import utils;
namespace ast {
namespace top {
export struct Namespace final : AstBase {
	Identifier name;
	std::vector<TopLevelAst> tops;
	Namespace(const Location &location, const Identifier &name, const std::vector<TopLevelAst> &tops);
	Namespace(const Namespace &other);
	Namespace(Namespace &&other);
	Namespace &operator=(const Namespace &other);
	Namespace &operator=(Namespace &&other) noexcept;
	~Namespace() override;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace top
} // namespace ast
