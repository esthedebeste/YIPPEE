module;
#include <coroutine>
#include <functional>
#include <memory>
#include <optional>
#include <ostream>
export module ast;
import utils;
import operators;
import type;
namespace ast {
export struct AnyAstPtr;
export struct Ast;
export using AstPtr = std::unique_ptr<Ast>;
export struct ExprAst;
export using ExprPtr = std::unique_ptr<ExprAst>;
export struct StatementAst;
export using StatementPtr = std::unique_ptr<StatementAst>;
export struct TopLevelAst;
export using TopLevelPtr = std::unique_ptr<TopLevelAst>;
export struct TypeAst;
export using TypePtr = std::unique_ptr<TypeAst>;
export struct Location {
	std::string_view file;
	std::size_t line, column;
	Location(const std::string_view file, const std::size_t line,
			 const std::size_t column)
		: file{file}, line{line}, column{column} {}
};
export std::ostream &operator<<(std::ostream &os, const Location &location) {
	return os << '(' << location.file << ':' << location.line << ':'
			  << location.column << ')';
}
export struct AstBase {
	Location location;
	explicit AstBase(const Location &location) : location{location} {}
	virtual ~AstBase() = default;
	using children_cb = std::function<void(const AstBase *)> &;
	virtual void children(children_cb) const {}
	// used by the graph printer to print a summary of the node.
	// don't need to summarize children.
	virtual void summarize(std::ostream &os) const { os << "???"; }
};
export const AstBase *to_ast_base(const auto *variant) {
	return variant->visit([](const auto &arg) -> const AstBase * { return &arg; });
}
export struct Name final : AstBase {
	std::string str;
	explicit Name(const Location &location, std::string str);
	void summarize(std::ostream &os) const override;
};
export std::ostream &operator<<(std::ostream &stream, const Name &name);
export struct Identifier final : AstBase {
	std::vector<Name> parts;
	Name final;
	explicit Identifier(const Location &location, std::vector<Name> parts, Name final);
	void summarize(std::ostream &os) const override;
};
export std::ostream &operator<<(std::ostream &stream, const Identifier &name);
export struct TypeArgument final : AstBase {
	std::string name;
	std::optional<std::unique_ptr<TypeAst>> default_type;
	TypeArgument(const Location &location, std::string name, std::optional<std::unique_ptr<TypeAst>> default_type);
	TypeArgument(const TypeArgument &other);
	TypeArgument(TypeArgument &&other) noexcept;
	~TypeArgument() override;
	TypeArgument &operator=(const TypeArgument &other);
	TypeArgument &operator=(TypeArgument &&other) noexcept;
	void summarize(std::ostream &os) const override;
	void children(children_cb) const override;
};
export struct Program final : AstBase {
	std::vector<TopLevelAst> tops;
	Program(const Location &location, std::vector<TopLevelAst> tops);
	~Program() override;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
namespace expr {
export struct Array final : AstBase {
	std::vector<ExprAst> values;
	Array(const Location &location, std::vector<ExprAst> values);
	Array(const Array &other);
	Array(Array &&other) noexcept;
	~Array() override;
	Array &operator=(const Array &other);
	Array &operator=(Array &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct Binop final : AstBase {
	operators::binary op;
	std::unique_ptr<ExprAst> left, right;
	Binop(const Location &location, std::unique_ptr<ExprAst> left, operators::binary op, std::unique_ptr<ExprAst> right);
	Binop(const Binop &other);
	Binop(Binop &&other) noexcept;
	~Binop() override;
	Binop &operator=(const Binop &other);
	Binop &operator=(Binop &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct Call final : AstBase {
	std::unique_ptr<ExprAst> callee;
	std::vector<ExprAst> arguments;
	Call(const Location &location, std::unique_ptr<ExprAst> callee, std::vector<ExprAst> arguments);
	Call(const Call &other);
	Call(Call &&other) noexcept;
	~Call() override;
	Call &operator=(const Call &other);
	Call &operator=(Call &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct Comparison final : AstBase {
	std::vector<operators::comparison> ops;
	std::vector<ExprAst> operands;
	Comparison(const Location &location, std::vector<operators::comparison> ops,
			   std::vector<ExprAst> operands);
	Comparison(const Comparison &other);
	Comparison(Comparison &&other) noexcept ;
	~Comparison() override;
	Comparison &operator=(const Comparison &other);
	Comparison &operator=(Comparison &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct Conditional final : AstBase {
	std::unique_ptr<ExprAst> condition, thenExpr, elseExpr;
	Conditional(const Location &location, std::unique_ptr<ExprAst> condition, std::unique_ptr<ExprAst> thenExpr,
				std::unique_ptr<ExprAst> elseExpr);
	Conditional(const Conditional &other);
	Conditional(Conditional &&other) noexcept ;
	~Conditional() override;
	Conditional &operator=(const Conditional &other);
	Conditional &operator=(Conditional &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct Create final : AstBase {
	std::unique_ptr<TypeAst> type;
	std::vector<std::pair<std::string, ExprAst>> args;
	Create(const Location &location, std::unique_ptr<TypeAst> type,
		   std::vector<std::pair<std::string, ExprAst>> args);
	Create(const Create &other);
	Create(Create &&other) noexcept ;
	~Create() override;
	Create &operator=(const Create &other);
	Create &operator=(Create &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct Identifier final : AstBase {
	ast::Identifier value;
	std::vector<TypeAst> type_arguments;
	Identifier(const Location &location, ast::Identifier value, std::vector<TypeAst> type_arguments);
	Identifier(const Identifier &other);
	Identifier(Identifier &&other) noexcept ;
	~Identifier() override;
	Identifier &operator=(const Identifier &other);
	Identifier &operator=(Identifier &&other) noexcept;
	void summarize(std::ostream &os) const override;
};
export struct Member final : AstBase {
	std::unique_ptr<ExprAst> expr;
	std::string name;
	Member(const Location &location, std::unique_ptr<ExprAst> expr, std::string name);
	Member(const Member &other);
	Member(Member &&other) noexcept ;
	~Member() override;
	Member &operator=(const Member &other);
	Member &operator=(Member &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct MemberCall final : AstBase {
	std::unique_ptr<ExprAst> callee;
	std::string name;
	std::vector<TypeAst> type_arguments;
	std::vector<ExprAst> arguments;
	MemberCall(const Location &location, std::unique_ptr<ExprAst> callee, std::string name, std::vector<TypeAst> type_arguments, std::vector<ExprAst> arguments);
	MemberCall(const MemberCall &other);
	MemberCall(MemberCall &&other) noexcept;
	~MemberCall() override;
	MemberCall &operator=(const MemberCall &other);
	MemberCall &operator=(MemberCall &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct Number final : AstBase {
	using uint_t = std::uintmax_t;
	using float_t = long double;
	utils::variant<uint_t, float_t> value;
	Number(const Location &location, uint_t integer);
	Number(const Location &location, float_t fp);
	Number(const Number &other);
	Number(Number &&other) noexcept ;
	Number &operator=(const Number &other);
	Number &operator=(Number &&other) noexcept;
	void summarize(std::ostream &os) const override;
};
export struct Subscript final : AstBase {
	std::unique_ptr<ExprAst> expr, index;
	Subscript(const Location &location, std::unique_ptr<ExprAst> expr, std::unique_ptr<ExprAst> index);
	Subscript(const Subscript &other);
	Subscript(Subscript &&other) noexcept ;
	~Subscript() override;
	Subscript &operator=(const Subscript &other);
	Subscript &operator=(Subscript &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct Unary final : AstBase {
	operators::unary op;
	std::unique_ptr<ExprAst> expr;
	Unary(const Location &location, operators::unary op, std::unique_ptr<ExprAst> expr);
	Unary(const Unary &other);
	Unary(Unary &&other) noexcept ;
	~Unary() override;
	Unary &operator=(const Unary &other);
	Unary &operator=(Unary &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace expr
namespace stmt {
export struct Block final : AstBase {
	std::vector<StatementAst> statements;
	Block(const Location &location, std::vector<StatementAst> statements);
	Block(const Block &other);
	Block(Block &&other) noexcept;
	~Block() override;
	Block &operator=(const Block &other);
	Block &operator=(Block &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct Expr final : AstBase {
	std::unique_ptr<ExprAst> expr;
	Expr(const Location &location, std::unique_ptr<ExprAst> expr);
	Expr(const Expr &other);
	Expr(Expr &&other) noexcept;
	~Expr() override;
	Expr &operator=(const Expr &other);
	Expr &operator=(Expr &&other) noexcept;
	void children(children_cb cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct For final : AstBase {
	std::optional<std::unique_ptr<StatementAst>> init;
	std::optional<std::unique_ptr<ExprAst>> cond, incr;
	std::unique_ptr<StatementAst> body;
	For(const Location &location, std::optional<std::unique_ptr<StatementAst>> init,
		std::optional<std::unique_ptr<ExprAst>> cond, std::optional<std::unique_ptr<ExprAst>> incr,
		std::unique_ptr<StatementAst> body);
	For(const For &other);
	For(For &&other) noexcept;
	~For() override;
	For &operator=(const For &other);
	For &operator=(For &&other) noexcept;
	void children(children_cb cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct If final : AstBase {
	std::unique_ptr<ExprAst> condition;
	std::unique_ptr<StatementAst> then;
	std::optional<std::unique_ptr<StatementAst>> otherwise;
	If(const Location &location, std::unique_ptr<ExprAst> condition, std::unique_ptr<StatementAst> then,
	   std::optional<std::unique_ptr<StatementAst>> otherwise);
	If(const If &other);
	If(If &&other) noexcept;
	~If() override;
	If &operator=(const If &other);
	If &operator=(If &&other) noexcept;
	void children(children_cb cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct Return final : AstBase {
	std::unique_ptr<ExprAst> expr;
	Return(const Location &location, std::unique_ptr<ExprAst> expr);
	Return(const Return &other);
	Return(Return &&other) noexcept;
	~Return() override;
	Return &operator=(const Return &other);
	Return &operator=(Return &&other) noexcept;
	void children(children_cb cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct Variable final : AstBase {
	std::string name;
	std::optional<std::unique_ptr<TypeAst>> type;
	std::unique_ptr<ExprAst> expr;
	Variable(const Location &location, std::string name, std::optional<std::unique_ptr<TypeAst>> type,
			 std::unique_ptr<ExprAst> expr);
	Variable(const Variable &other);
	Variable(Variable &&other) noexcept;
	~Variable() override;
	Variable &operator=(const Variable &other);
	Variable &operator=(Variable &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct While final : AstBase {
	std::unique_ptr<ExprAst> expr;
	std::unique_ptr<StatementAst> body;
	While(const Location &location, std::unique_ptr<ExprAst> expr, std::unique_ptr<StatementAst> body);
	While(const While &other);
	While(While &&other) noexcept;
	~While() override;
	While &operator=(const While &other);
	While &operator=(While &&other) noexcept;
	void children(children_cb cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace stmt
namespace top {
export struct Function final : AstBase {
	Identifier name;
	std::vector<TypeArgument> type_arguments;
	using Parameter = std::pair<Name, TypeAst>;
	std::vector<Parameter> parameters;
	std::unique_ptr<TypeAst> return_type;
	std::unique_ptr<StatementAst> statement;
	Function(const Location &location, Identifier name,
			 std::vector<TypeArgument> type_arguments,
			 std::vector<Parameter> parameters, std::unique_ptr<TypeAst> return_type,
			 std::unique_ptr<StatementAst> statement);
	Function(const Function &other);
	Function(Function &&other) noexcept;
	Function &operator=(const Function &other);
	Function &operator=(Function &&other) noexcept;
	~Function() override;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct Namespace final : AstBase {
	Identifier name;
	std::vector<TopLevelAst> tops;
	Namespace(const Location &location, const Identifier &name, const std::vector<TopLevelAst> &tops);
	Namespace(const Namespace &other);
	Namespace(Namespace &&other) noexcept ;
	Namespace &operator=(const Namespace &other);
	Namespace &operator=(Namespace &&other) noexcept;
	~Namespace() override;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct Struct final : AstBase {
	Identifier name;
	std::vector<TypeArgument> type_arguments;
	using Field = std::pair<std::string, TypeAst>;
	std::vector<Field> members;
	Struct(const Location &location, Identifier name,
		   std::vector<TypeArgument> type_arguments, std::vector<Field> members);
	Struct(const Struct &other);
	Struct(Struct &&other) noexcept ;
	Struct &operator=(const Struct &other);
	Struct &operator=(Struct &&other) noexcept;
	~Struct() override;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace top
namespace type {
export struct Array final : AstBase {
	std::unique_ptr<TypeAst> member;
	std::uintmax_t size;
	Array(const Location &location, std::unique_ptr<TypeAst> member, std::uintmax_t size);
	Array(const Array &other);
	Array(Array &&other) noexcept;
	Array &operator=(const Array &other);
	Array &operator=(Array &&other) noexcept;
	~Array() override;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct Named final : AstBase {
	Identifier name;
	std::vector<TypeAst> arguments;
	Named(const Location &location, Identifier name, std::vector<TypeAst> arguments);
	Named(const Named &);
	Named(Named &&) noexcept;
	~Named() override;
	Named &operator=(const Named &);
	Named &operator=(Named &&) noexcept;
	void summarize(std::ostream &os) const override;
};
export struct Pointer final : AstBase {
	std::unique_ptr<TypeAst> pointed;
	Pointer(const Location &location, std::unique_ptr<TypeAst> pointed);
	Pointer(const Pointer &other);
	Pointer(Pointer &&other) noexcept;
	Pointer &operator=(const Pointer &other);
	Pointer &operator=(Pointer &&other) noexcept;
	~Pointer() override;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
export struct Primitive final : AstBase {
	::type::Primitive prim;
	Primitive(const Location &location, ::type::Primitive prim);
	Primitive(const Primitive &other);
	Primitive(Primitive &&other) noexcept;
	Primitive &operator=(const Primitive &other);
	Primitive &operator=(Primitive &&other) noexcept;
	void summarize(std::ostream &os) const override;
};
} // namespace type
} // namespace ast

using ExprVariant =
		utils::variant<ast::expr::Array, ast::expr::Binop, ast::expr::Call,
					   ast::expr::Comparison, ast::expr::Conditional, ast::expr::Create,
					   ast::expr::Identifier, ast::expr::Member, ast::expr::MemberCall,
					   ast::expr::Number, ast::expr::Subscript, ast::expr::Unary>;
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
	const void *ptr() {
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
};
struct StatementAst : StmtVariant {
	using base = StmtVariant;
	using base::base;
};
struct TopLevelAst : TopLevelVariant {
	using base = TopLevelVariant;
	using base::base;
};
struct TypeAst : TypeVariant {
	using base = TypeVariant;
	using base::base;
};
} // namespace ast