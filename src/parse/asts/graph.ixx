module;
#include <ostream>
export module ast.graph;
import ast;

export namespace ast {
void graph(const AnyAstPtr &, std::ostream &);
}

module :private;

namespace {
void child_graph(const ast::AnyAstPtr &parent, std::ostream &stream) {
	stream << "  \"" << parent.ptr() << "\" [label=\"";
	parent.visit([&](auto &ptr) {
		ptr->summarize(stream);
		stream << " (" << ptr->location.line << ":" << ptr->location.column << ")";
	});
	stream << "\"]\n";
	parent.visit([&](auto ptr) {
		std::function<void(ast::AnyAstPtr)> callback{[&](const ast::AnyAstPtr child) {
			stream << "  \"" << parent.ptr() << "\" -> \"" << child.ptr() << "\"\n";
			child_graph(child, stream);
		}};
		ptr->children(callback);
	});
}
} // namespace

void ast::graph(const AnyAstPtr &ast, std::ostream &stream) {
	stream << "digraph ParseTree {\n";
	child_graph(ast, stream);
	stream << "}\n";
}