module;
#include <functional>
#include <memory>
#include <ostream>
export module ast.def;

export namespace ast {
struct Location {
	std::string_view file;
	std::size_t line, column;
	Location(const std::string_view file, const std::size_t line,
			 const std::size_t column)
		: file{file}, line{line}, column{column} {}
};
std::ostream &operator<<(std::ostream &os, const Location &location) {
	return os << '(' << location.file << ':' << location.line << ':'
			  << location.column << ')';
}
struct AnyAstPtr;
struct Ast;
using AstPtr = std::unique_ptr<Ast>;
struct ExprAst;
using ExprPtr = std::unique_ptr<ExprAst>;
struct StatementAst;
using StatementPtr = std::unique_ptr<StatementAst>;
struct TopLevelAst;
using TopLevelPtr = std::unique_ptr<TopLevelAst>;
struct TypeAst;
using TypePtr = std::unique_ptr<TypeAst>;
struct AstBase {
	Location location;
	explicit AstBase(const Location &location) : location{location} {}
	virtual ~AstBase() {}
	using children_cb = std::function<void(AnyAstPtr)> &;
	virtual void children(children_cb) const {}
	// used by the graph printer to print a summary of the node.
	// don't need to summarize children.
	virtual void summarize(std::ostream &os) const { os << "???"; }
};
AnyAstPtr base_ptr(const Ast *ptr);
AnyAstPtr base_ptr(const ExprAst *ptr);
AnyAstPtr base_ptr(const StatementAst *ptr);
AnyAstPtr base_ptr(const TopLevelAst *ptr);
AnyAstPtr base_ptr(const TypeAst *ptr);
} // namespace ast