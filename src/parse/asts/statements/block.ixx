module;
#include <coroutine>

#include <memory>
#include <ostream>
#include <utility>
#include <vector>
export module ast.stmt.block;
import ast.def;
import utils;
namespace ast {
namespace stmt {
export struct Block final : AstBase {
	std::vector<StatementAst> statements;
	Block(const Location &location, std::vector<StatementAst> statements);
	Block(const Block &other);
	Block(Block &&other) noexcept;
	Block &operator=(const Block &other);
	Block &operator=(Block &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace stmt
} // namespace ast
