module;
#include <functional>
#include <ostream>
export module ast.graph;
import ast;

export namespace ast {
void graph(const ast::AstBase*, std::ostream &);
}

module :private;
namespace {
void child_graph(const ast::AstBase* parent, std::ostream &stream) {
	stream << "  \"" << parent << "\" [label=\"";
	parent->summarize(stream);
	stream << " (" << parent->location.line << ":" << parent->location.column << ")";
	stream << "\"]\n";
	std::function<void(const ast::AstBase *)> callback{[&](const ast::AstBase* child) {
		stream << "  \"" << parent << "\" -> \"" << child << "\"\n";
		child_graph(child, stream);
	}};
	parent->children(callback);
}
} // namespace

void ast::graph(const AstBase *ast, std::ostream &stream) {
	stream << "digraph ParseTree {\n";
	child_graph(ast, stream);
	stream << "}\n";
}