module;
#include <coroutine>

#include <memory>
#include <ostream>
export module ast;
export import ast.program;
export import ast.exprs.array;
export import ast.exprs.binop;
export import ast.exprs.call;
export import ast.exprs.comparison;
export import ast.exprs.conditional;
export import ast.exprs.create;
export import ast.exprs.identifier;
export import ast.exprs.member;
export import ast.exprs.number;
export import ast.exprs.subscript;
export import ast.exprs.unary;
export import ast.stmt.block;
export import ast.stmt.expr;
export import ast.stmt.for_;
export import ast.stmt.if_;
export import ast.stmt.ret;
export import ast.stmt.variable;
export import ast.stmt.while_;
export import ast.top.space;
export import ast.top.function;
export import ast.top.structure;
export import ast.type.array;
export import ast.type.named;
export import ast.type.pointer;
export import ast.type.primitive;
export import ast.name;
export import ast.type_argument;
export import ast.def;
import utils;

using ExprVariant =
		utils::variant<ast::expr::Array, ast::expr::Binop, ast::expr::Call,
					   ast::expr::Comparison, ast::expr::Conditional, ast::expr::Create,
					   ast::expr::Identifier, ast::expr::Member, ast::expr::Number,
					   ast::expr::Subscript, ast::expr::Unary>;
using StmtVariant =
		utils::variant<ast::stmt::Block, ast::stmt::Expr, ast::stmt::For,
					   ast::stmt::If, ast::stmt::Return, ast::stmt::Variable,
					   ast::stmt::While>;
using TopLevelVariant =
		utils::variant<ast::top::Namespace, ast::top::Function, ast::top::Struct>;
using TypeVariant =
		utils::variant<ast::type::Array, ast::type::Named, ast::type::Pointer, ast::type::Primitive>;
using AstVariant = utils::unwrap_concat_instantiate<
		utils::variant, ExprVariant, StmtVariant, TopLevelVariant, TypeVariant,
		utils::variant<ast::Program, ast::Name, ast::Identifier, ast::TypeArgument>>;
template<class T>
struct PtrMapper {
	using type = const T *;
};
using AnyAstPtrVariant = utils::unwrap_map<PtrMapper, AstVariant>;
export namespace ast {
// any pointer to any ast
struct AnyAstPtr : AnyAstPtrVariant {
	using base = AnyAstPtrVariant;
	using base::base;
	AnyAstPtr(base &&b) : base{std::move(b)} {}
	const void *ptr() const {
		return visit([](auto &&v) { return static_cast<const void *>(v); });
	}
};
struct Ast : AstVariant {
	using base = AstVariant;
	using base::base;
};
struct ExprAst : ExprVariant {
	using base = ExprVariant;
	using base::base;
	ExprAst(base &&b) : base{std::move(b)} {}
};
struct StatementAst : StmtVariant {
	using base = StmtVariant;
	using base::base;
	StatementAst(base &&b) : base{std::move(b)} {}
};
struct TopLevelAst : TopLevelVariant {
	using base = TopLevelVariant;
	using base::base;
	TopLevelAst(base &&b) : base{std::move(b)} {}
};
struct TypeAst : TypeVariant {
	using base = TypeVariant;
	using base::base;
	TypeAst(base &&b) : base{std::move(b)} {}
};
} // namespace ast