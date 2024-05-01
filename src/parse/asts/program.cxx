module;
#include <coroutine>

#include <ostream>
#include <vector>
module ast.program;
import ast;

namespace ast {
Program::Program(const Location &location, std::vector<TopLevelAst> tops)
	: AstBase(location), tops{std::move(tops)} {}
Program::~Program() {}
void Program::children(children_cb cb) const {
	for (auto &top : tops) {
		cb(base_ptr(&top));
	}
}
void Program::summarize(std::ostream &os) const { os << "Program"; }
} // namespace ast