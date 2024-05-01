module;
#include <coroutine>

#include <ostream>
#include <utility>
#include <vector>
module ast.stmt.block;
import ast;

namespace ast {
namespace stmt {
Block::Block(const Location &location, std::vector<StatementAst> statements)
	: AstBase(location), statements(std::move(statements)) {}
Block::Block(const Block &other) = default;
Block::Block(Block &&other) noexcept = default;
Block &Block::operator=(const Block &other) = default;
Block &Block::operator=(Block &&other) noexcept = default;
void Block::children(children_cb cb) const {
	for (auto &stmt : statements)
		cb(base_ptr(&stmt));
}
void Block::summarize(std::ostream &os) const { os << "Block"; }
} // namespace stmt
} // namespace ast