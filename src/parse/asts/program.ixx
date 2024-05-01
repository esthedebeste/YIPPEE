module;
#include <coroutine>

#include <memory>
#include <ostream>
#include <vector>
export module ast.program;
import ast.def;

export namespace ast {
struct Program final : AstBase {
	std::vector<TopLevelAst> tops;
	Program(const Location &location, std::vector<TopLevelAst> tops);
	~Program() override;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace ast