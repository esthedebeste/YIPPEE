module;
#include <functional>
#include <ostream>
#include <span>
export module ast.graph;
import ast;

export namespace ast {
void graph(std::span<const ast::AstBase *>, std::ostream &);
}

module :private;
namespace {
void child_graph(const ast::AstBase *parent, std::ostream &stream) {
	stream << "  \"" << parent << "\" [label=\"";
	parent->summarize(stream);
	stream << " (" << parent->location.line << ":" << parent->location.column << ")";
	stream << "\"]\n";
	std::function<void(const ast::AstBase *)> callback{[&](const ast::AstBase *child) {
		stream << "  \"" << parent << "\" -> \"" << child << "\"\n";
		child_graph(child, stream);
	}};
	parent->children(callback);
}
} // namespace

void ast::graph(std::span<const ast::AstBase *> asts, std::ostream &stream) {
	stream << "digraph ParseTree {\n";
	for (const auto &ast : asts)
		child_graph(ast, stream);
	stream << "}\n";
}