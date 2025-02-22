module;
#include <functional>
#include <ostream>
#include <span>
#include <cmath>
export module ast.graph;
import ast;

export namespace ast {
void graph(std::span<const ast::AstBase *>, std::ostream &);
}

module :private;
namespace {
void child_graph(const ast::AstBase *parent, std::ostream &stream, const int depth = 0) {
	stream << "  \"" << parent << "\" [label=\"";
	parent->summarize(stream);
	stream << " " << parent->location;
	const double h = std::fmod(static_cast<double>(depth) / 10, 1.0);
	stream << "\", color=\"" << h << ", 1, 0.7\"]\n";
	std::function callback{[&](const ast::AstBase *child) -> void {
		stream << "  \"" << parent << "\" -> \"" << child << "\" [color=\"" << h << ", 1, 0.7\"]s\n";
		child_graph(child, stream, depth + 1);
	}};
	parent->children(callback);
}
} // namespace

void ast::graph(std::span<const ast::AstBase *> asts, std::ostream &stream) {
	stream << "digraph ParseTree {\n  layout=sfdp\n  overlap=false\n  beautify=true\n";
	for (const auto &ast : asts)
		child_graph(ast, stream);
	stream << "}\n";
}