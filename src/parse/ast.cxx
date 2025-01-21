module;
#include <coroutine>
#include <memory>
#include <ostream>
#include <ranges>
#include <vector>
module ast;
namespace ast {
namespace expr {
Array::Array(const Location &location, std::vector<ExprAst> values)
	: AstBase(location), values{std::move(values)} {}
Array::Array(const Array &other) = default;
Array::Array(Array &&other) noexcept = default;
Array::~Array() = default;

Array &Array::operator=(const Array &other) = default;
Array &Array::operator=(Array &&other) noexcept = default;
void Array::children(children_cb cb) const {
	for (const auto &value : values)
		cb(to_ast_base(&value));
}
void Array::summarize(std::ostream &os) const { os << "Array(" << values.size() << ")"; }
Binop::Binop(const Location &location, ExprPtr left, const operators::binary op,
			 ExprPtr right)
	: AstBase(location), op{op}, left{std::move(left)},
	  right{std::move(right)} {}
Binop::Binop(const Binop &other)
	: AstBase(other), op{other.op}, left{clone(other.left)},
	  right{clone(other.right)} {}
Binop::Binop(Binop &&other) noexcept = default;
Binop::~Binop() = default;
Binop &Binop::operator=(const Binop &other) {
	AstBase::operator=(other);
	op = other.op;
	left = clone(other.left);
	right = clone(other.right);
	return *this;
}
Binop &Binop::operator=(Binop &&other) noexcept = default;
void Binop::children(children_cb cb) const {
	cb(to_ast_base(left.get()));
	cb(to_ast_base(right.get()));
}

void Binop::summarize(std::ostream &os) const {
	if (op == operators::binary::assign)
		os << "Assignment";
	else
		os << "Binop(" << op << ")";
}
Call::Call(const Location &location, ExprPtr callee, std::vector<ExprAst> arguments)
	: AstBase(location), callee{std::move(callee)},
	  arguments{std::move(arguments)} {}
Call::Call(const Call &other)
	: AstBase(other), callee{clone(other.callee)},
	  arguments{other.arguments} {}
Call::Call(Call &&other) noexcept = default;
Call::~Call() = default;
Call &Call::operator=(const Call &other) {
	AstBase::operator=(other);
	callee = clone(other.callee);
	arguments = other.arguments;
	return *this;
}
Call &Call::operator=(Call &&other) noexcept = default;
void Call::children(children_cb cb) const {
	cb(to_ast_base(callee.get()));
	for (const auto &arg : arguments) {
		cb(to_ast_base(&arg));
	}
}
void Call::summarize(std::ostream &os) const { os << "Call"; }
Comparison::Comparison(const Location &location,
					   std::vector<operators::comparison> ops,
					   std::vector<ExprAst> operands)
	: AstBase(location), ops{std::move(ops)}, operands{std::move(operands)} {}
Comparison::Comparison(const Comparison &other) = default;
Comparison::Comparison(Comparison &&other) noexcept = default;
Comparison::~Comparison() = default;
Comparison &Comparison::operator=(const Comparison &other) = default;
Comparison &Comparison::operator=(Comparison &&other) noexcept = default;
void Comparison::children(children_cb cb) const {
	for (auto &expr : operands)
		cb(to_ast_base(&expr));
}
void Comparison::summarize(std::ostream &os) const {
	os << "Comparison(";
	for (size_t i = 0; i < ops.size(); ++i) {
		os << ops[i];
		if (i != ops.size() - 1) {
			os << ", ";
		}
	}
	os << ")";
}
Conditional::Conditional(const Location &location, ExprPtr condition, ExprPtr thenExpr,
						 ExprPtr elseExpr)
	: AstBase(location), condition{std::move(condition)},
	  thenExpr{std::move(thenExpr)}, elseExpr{std::move(elseExpr)} {}
Conditional::Conditional(const Conditional &other)
	: AstBase(other.location), condition{clone(other.condition)},
	  thenExpr{clone(other.thenExpr)},
	  elseExpr{clone(other.elseExpr)} {}
Conditional::Conditional(Conditional &&other) noexcept = default;
Conditional::~Conditional() = default;
Conditional &Conditional::operator=(const Conditional &other) {
	AstBase::operator=(other);
	condition = clone(other.condition);
	thenExpr = clone(other.thenExpr);
	elseExpr = clone(other.elseExpr);
	return *this;
}
Conditional &Conditional::operator=(Conditional &&other) noexcept = default;
void Conditional::children(children_cb cb) const {
	cb(to_ast_base(condition.get()));
	cb(to_ast_base(thenExpr.get()));
	cb(to_ast_base(elseExpr.get()));
}
void Conditional::summarize(std::ostream &os) const { os << "Conditional"; }
Create::Create(const Location &location, TypePtr type,
			   std::vector<std::pair<std::string, ExprAst>> args)
	: AstBase(location), type(std::move(type)), args(std::move(args)) {}
Create::Create(const Create &other)
	: AstBase(other), type{clone(other.type)}, args{other.args} {}
Create::Create(Create &&other) noexcept = default;
Create::~Create() = default;
Create &Create::operator=(const Create &other) {
	AstBase::operator=(other);
	type = clone(other.type);
	args = other.args;
	return *this;
}
Create &Create::operator=(Create &&other) noexcept = default;
void Create::children(children_cb cb) const {
	cb(to_ast_base(type.get()));
	for (const auto &val : args | std::views::values)
		cb(to_ast_base(&val));
}
void Create::summarize(std::ostream &os) const { os << "Create"; }
Identifier::Identifier(const Location &location, ast::Identifier value, std::vector<TypeAst> type_arguments)
	: AstBase(location), value{std::move(value)}, type_arguments{std::move(type_arguments)} {}
Identifier::Identifier(const Identifier &other) = default;
Identifier::Identifier(Identifier &&other) noexcept = default;
Identifier::~Identifier() = default;
Identifier &Identifier::operator=(const Identifier &other) = default;
Identifier &Identifier::operator=(Identifier &&other) noexcept = default;

void Identifier::summarize(std::ostream &os) const {
	os << "Identifier(" << value << ")";
}
Member::Member(const Location &location, ExprPtr expr, std::string name)
	: AstBase(location), expr{std::move(expr)}, name{std::move(name)} {}
Member::Member(const Member &other)
	: AstBase(other), expr{utils::clone(other.expr)}, name{other.name} {}
Member::Member(Member &&other) noexcept = default;
Member::~Member() = default;
Member &Member::operator=(const Member &other) {
	AstBase::operator=(other);
	expr = utils::clone(other.expr);
	name = other.name;
	return *this;
}
Member &Member::operator=(Member &&other) noexcept = default;
void Member::children(children_cb cb) const { cb(to_ast_base(expr.get())); }
void Member::summarize(std::ostream &os) const {
	os << "Member(" << name << ")";
}
MemberCall::MemberCall(const Location &location, ExprPtr callee, std::string name, std::vector<TypeAst> type_arguments, std::vector<ExprAst> arguments)
	: AstBase(location), callee{std::move(callee)}, name{std::move(name)},
	  type_arguments{std::move(type_arguments)}, arguments{std::move(arguments)} {}
MemberCall::MemberCall(const MemberCall &other)
	: AstBase(other), callee{clone(other.callee)}, name{other.name},
	  type_arguments{other.type_arguments}, arguments{other.arguments} {}
MemberCall::MemberCall(MemberCall &&other) noexcept = default;
MemberCall::~MemberCall() = default;
MemberCall &MemberCall::operator=(const MemberCall &other) {
	AstBase::operator=(other);
	callee = clone(other.callee);
	name = other.name;
	type_arguments = other.type_arguments;
	arguments = other.arguments;
	return *this;
}
MemberCall &MemberCall::operator=(MemberCall &&other) noexcept = default;
void MemberCall::children(children_cb cb) const {
	cb(to_ast_base(callee.get()));
	for (const auto &arg : type_arguments)
		cb(to_ast_base(&arg));
	for (const auto &arg : arguments)
		cb(to_ast_base(&arg));
}
void MemberCall::summarize(std::ostream &os) const { os << "MemberCall(" << name << ")"; }
Number::Number(const Location &location, uint_t integer)
	: AstBase(location), value{integer} {}
Number::Number(const Location &location, float_t fp) : AstBase(location), value{fp} {}
Number::Number(const Number &other) = default;
Number::Number(Number &&other) noexcept = default;
Number &Number::operator=(const Number &other) = default;
Number &Number::operator=(Number &&other) noexcept = default;
void Number::summarize(std::ostream &os) const {
	os << "Number(";
	value.visit([&](auto &&v) { os << v; });
	os << ")";
}
Subscript::Subscript(const Location &location, ExprPtr expr, ExprPtr index)
	: AstBase(location), expr{std::move(expr)}, index{std::move(index)} {}
Subscript::Subscript(const Subscript &other)
	: AstBase(other), expr{clone(other.expr)},
	  index{clone(other.index)} {}
Subscript::Subscript(Subscript &&other) noexcept = default;
Subscript::~Subscript() = default;

Subscript &Subscript::operator=(const Subscript &other) {
	AstBase::operator=(other);
	expr = clone(other.expr);
	index = clone(other.index);
	return *this;
}
Subscript &Subscript::operator=(Subscript &&other) noexcept = default;
void Subscript::children(children_cb cb) const {
	cb(to_ast_base(expr.get()));
	cb(to_ast_base(index.get()));
}
void Subscript::summarize(std::ostream &os) const { os << "Subscript"; }
Unary::Unary(const Location &location, const operators::unary op, ExprPtr expr)
	: AstBase(location), op{op}, expr{std::move(expr)} {}
Unary::Unary(const Unary &other)
	: AstBase(other), op{other.op}, expr{clone(other.expr)} {}
Unary::Unary(Unary &&other) noexcept = default;
Unary::~Unary() = default;
Unary &Unary::operator=(const Unary &other) {
	AstBase::operator=(other);
	op = other.op;
	expr = clone(other.expr);
	return *this;
}
Unary &Unary::operator=(Unary &&other) noexcept = default;
void Unary::children(children_cb cb) const { cb(to_ast_base(expr.get())); }
void Unary::summarize(std::ostream &os) const { os << "Unary(" << op << ")"; }
} // namespace expr
Name::Name(const Location &location, std::string str)
	: AstBase{location}, str{std::move(str)} {}
std::ostream &operator<<(std::ostream &stream, const Name &name) {
	return stream << name.str;
}
void Name::summarize(std::ostream &os) const { os << "Name(" << *this << ")"; }

Identifier::Identifier(const Location &location, std::vector<Name> parts, Name final)
	: AstBase{location}, parts{std::move(parts)}, final{std::move(final)} {}
std::ostream &operator<<(std::ostream &stream, const Identifier &name) {
	for (const auto &part : name.parts)
		stream << part << "::";
	stream << name.final;
	return stream;
}
void Identifier::summarize(std::ostream &os) const {
	os << "Identifier(" << *this << ")";
}
TypeArgument::TypeArgument(const Location &location, std::string name, std::optional<TypePtr> default_type)
	: AstBase(location), name(std::move(name)), default_type(std::move(default_type)) {}
TypeArgument::TypeArgument(const TypeArgument &other) : AstBase(other), name(other.name), default_type(clone(other.default_type)) {}
TypeArgument::TypeArgument(TypeArgument &&other) noexcept = default;
TypeArgument::~TypeArgument() = default;

TypeArgument &TypeArgument::operator=(const TypeArgument &other) {
	AstBase::operator=(other);
	name = other.name;
	default_type = clone(other.default_type);
	return *this;
}
TypeArgument &TypeArgument::operator=(TypeArgument &&other) noexcept = default;
void TypeArgument::summarize(std::ostream &os) const { os << "TypeArgument(" << name << ")"; }
void TypeArgument::children(children_cb cb) const {
	if (default_type)
		cb(to_ast_base(default_type->get()));
}
namespace stmt {
Block::Block(const Location &location, std::vector<StatementAst> statements)
	: AstBase(location), statements(std::move(statements)) {}
Block::Block(const Block &other) = default;
Block::Block(Block &&other) noexcept = default;
Block::~Block() = default;
Block &Block::operator=(const Block &other) = default;
Block &Block::operator=(Block &&other) noexcept = default;
void Block::children(children_cb cb) const {
	for (auto &stmt : statements)
		cb(to_ast_base(&stmt));
}
void Block::summarize(std::ostream &os) const { os << "Block"; }

Expr::Expr(const Location &location, ExprPtr expr)
	: AstBase(location), expr{std::move(expr)} {}
Expr::Expr(const Expr &other)
	: AstBase(other), expr{clone(other.expr)} {}
Expr::Expr(Expr &&other) noexcept = default;
Expr::~Expr() = default;
Expr &Expr::operator=(const Expr &other) {
	AstBase::operator=(other);
	expr = clone(other.expr);
	return *this;
}
Expr &Expr::operator=(Expr &&other) noexcept = default;
void Expr::children(children_cb cb) const { cb(to_ast_base(expr.get())); }
void Expr::summarize(std::ostream &os) const { os << "ExprStatement"; }
For::For(const Location &location, std::optional<StatementPtr> init,
		 std::optional<ExprPtr> cond, std::optional<ExprPtr> incr,
		 StatementPtr body)
	: AstBase(location), init{std::move(init)}, cond{std::move(cond)},
	  incr{std::move(incr)}, body{std::move(body)} {}
For::For(const For &other)
	: AstBase(other), init{clone(other.init)}, cond{clone(other.cond)},
	  incr{clone(other.incr)}, body{clone(other.body)} {}
For::For(For &&other) noexcept = default;
For::~For() = default;
For &For::operator=(const For &other) {
	AstBase::operator=(other);
	init = clone(other.init);
	cond = clone(other.cond);
	incr = clone(other.incr);
	body = clone(other.body);
	return *this;
}
For &For::operator=(For &&other) noexcept = default;
void For::children(children_cb cb) const {
	if (init)
		cb(to_ast_base(init->get()));
	if (cond)
		cb(to_ast_base(cond->get()));
	if (incr)
		cb(to_ast_base(incr->get()));
	cb(to_ast_base(body.get()));
}
void For::summarize(std::ostream &os) const { os << "ForStatement"; }
If::If(const Location &location, ExprPtr condition, StatementPtr then,
	   std::optional<StatementPtr> otherwise)
	: AstBase(location), condition{std::move(condition)}, then{std::move(then)},
	  otherwise{std::move(otherwise)} {}
If::If(const If &other)
	: AstBase(other), condition{clone(other.condition)}, then{clone(other.then)},
	  otherwise{clone(other.otherwise)} {}
If::If(If &&other) noexcept = default;
If::~If() = default;
If &If::operator=(const If &other) {
	AstBase::operator=(other);
	condition = clone(other.condition);
	then = clone(other.then);
	otherwise = clone(other.otherwise);
	return *this;
}
If &If::operator=(If &&other) noexcept = default;
void If::children(children_cb cb) const {
	cb(to_ast_base(condition.get()));
	cb(to_ast_base(then.get()));
	if (otherwise)
		cb(to_ast_base(otherwise->get()));
}
void If::summarize(std::ostream &os) const { os << "IfStatement"; }
Return::Return(const Location &location, ExprPtr expr)
	: AstBase(location), expr{std::move(expr)} {}
Return::Return(const Return &other)
	: AstBase(other), expr{clone(other.expr)} {}
Return::Return(Return &&other) noexcept = default;
Return::~Return() = default;

Return &Return::operator=(const Return &other) {
	AstBase::operator=(other);
	expr = clone(other.expr);
	return *this;
}
Return &Return::operator=(Return &&other) noexcept = default;
void Return::children(children_cb cb) const { cb(to_ast_base(expr.get())); }
void Return::summarize(std::ostream &os) const { os << "Return"; }
Variable::Variable(const Location &location, std::string name,
				   std::optional<TypePtr> type, ExprPtr expr)
	: AstBase(location), name{std::move(name)}, type{std::move(type)},
	  expr{std::move(expr)} {}
Variable::Variable(const Variable &other)
	: AstBase(other), name{other.name}, expr{clone(other.expr)} {
	type = clone(other.type);
}
Variable::Variable(Variable &&other) noexcept = default;
Variable::~Variable() = default;
Variable &Variable::operator=(const Variable &other) {
	AstBase::operator=(other);
	name = other.name;
	expr = clone(other.expr);
	type = clone(other.type);
	return *this;
}
Variable &Variable::operator=(Variable &&other) noexcept = default;
void Variable::summarize(std::ostream &os) const {
	os << "Variable(" << name << ')';
}
void Variable::children(children_cb cb) const {
	if (type)
		cb(to_ast_base(type->get()));
	cb(to_ast_base(expr.get()));
}
While::While(const Location &location, ExprPtr expr, StatementPtr then)
	: AstBase(location), expr{std::move(expr)}, body{std::move(then)} {}
While::While(const While &other)
	: AstBase(other), expr{clone(other.expr)}, body{clone(other.body)} {}
While::While(While &&other) noexcept = default;
While::~While() = default;
While &While::operator=(const While &other) {
	AstBase::operator=(other);
	expr = clone(other.expr);
	body = clone(other.body);
	return *this;
}
While &While::operator=(While &&other) noexcept = default;
void While::children(children_cb cb) const {
	cb(to_ast_base(expr.get()));
	cb(to_ast_base(body.get()));
}
void While::summarize(std::ostream &os) const { os << "WhileStatement"; }
}
namespace top {
Function::Function(const Location &location, Identifier name,
				   std::vector<TypeArgument> type_arguments,
				   std::vector<Parameter> parameters, TypePtr return_type,
				   StatementPtr statement)
	: AstBase(location), name{std::move(name)},
	  type_arguments(std::move(type_arguments)),
	  parameters(std::move(parameters)), return_type{std::move(return_type)},
	  statement{std::move(statement)} {}
Function::Function(const Function &other) : AstBase(other), name{other.name},
											type_arguments(other.type_arguments), parameters(other.parameters),
											return_type{clone(other.return_type)}, statement{clone(other.statement)} {}
Function::Function(Function &&other) noexcept = default;
Function &Function::operator=(const Function &other) {
	AstBase::operator=(other);
	name = other.name;
	type_arguments = other.type_arguments;
	parameters = other.parameters;
	return_type = clone(other.return_type);
	statement = clone(other.statement);
	return *this;
}
Function &Function::operator=(Function &&other) noexcept = default;
Function::~Function() {}
void Function::children(children_cb cb) const {
	for (auto &type_arg : type_arguments)
		cb(&type_arg);
	for (auto &param : parameters) {
		cb(&param.first);
		cb(to_ast_base(&param.second));
	}
	cb(to_ast_base(statement.get()));
}
void Function::summarize(std::ostream &os) const {
	os << "Function(" << name << ')';
}
Namespace::Namespace(const Location &location, const Identifier &name,
					 const std::vector<TopLevelAst> &tops)
	: AstBase(location), name{name}, tops(tops) {}
Namespace::Namespace(const Namespace &other) = default;
Namespace::Namespace(Namespace &&other) noexcept = default;
Namespace &Namespace::operator=(const Namespace &other) = default;
Namespace &Namespace::operator=(Namespace &&other) noexcept = default;
Namespace::~Namespace() {}
void Namespace::children(children_cb cb) const {
	for (auto &child : tops)
		cb(to_ast_base(&child));
}
void Namespace::summarize(std::ostream &os) const {
	os << "Namespace(" << name << ')';
}
Struct::Struct(const Location &location, Identifier name,
			   std::vector<TypeArgument> type_arguments,
			   std::vector<Field> members)
	: AstBase(location), name{std::move(name)},
	  type_arguments(std::move(type_arguments)), members(std::move(members)) {}

Struct::Struct(const Struct &other) = default;
Struct::Struct(Struct &&other) noexcept = default;
Struct &Struct::operator=(const Struct &other) = default;
Struct &Struct::operator=(Struct &&other) noexcept = default;
Struct::~Struct() {}
void Struct::children(children_cb cb) const {
	for (auto &argument : type_arguments)
		if (argument.default_type)
			cb(to_ast_base(argument.default_type->get()));
	for (auto &member : members)
		cb(to_ast_base(&member.second));
}
void Struct::summarize(std::ostream &os) const {
	os << "Struct(" << name;
	size_t i = 0;
	os << '<';
	for (auto &argument : type_arguments)
		os << (i++ == 0 ? "" : ", ") << argument.name;
	i = 0;
	os << ">{";
	for (auto &member : members)
		os << (i++ == 0 ? "" : ", ") << member.first;
	os << "})";
}
} // namespace top
namespace type {
Array::Array(const Location &location, TypePtr member, const std::uintmax_t size)
	: AstBase(location), member{std::move(member)}, size{size} {}
Array::Array(const Array &other)
	: AstBase(other), member{clone(other.member)}, size{other.size} {}
Array::Array(Array &&other) noexcept = default;
Array &Array::operator=(const Array &other) {
	AstBase::operator=(other);
	member = clone(other.member);
	size = other.size;
	return *this;
}
Array &Array::operator=(Array &&other) noexcept = default;
Array::~Array() {}
void Array::children(children_cb cb) const { cb(to_ast_base(member.get())); }
void Array::summarize(std::ostream &os) const { os << "Array(" << size << ")"; }
Named::Named(const Location &location, Identifier name, std::vector<TypeAst> arguments)
	: AstBase(location), name{std::move(name)},
	  arguments{std::move(arguments)} {}
Named::Named(const Named &named) = default;
Named::Named(Named &&) noexcept = default;
Named::~Named() = default;
Named &Named::operator=(const Named &) = default;
Named &Named::operator=(Named &&) noexcept = default;
void Named::summarize(std::ostream &os) const {
	os << "NamedType(" << name << ')';
}
Pointer::Pointer(const Location &location, TypePtr pointed)
	: AstBase(location), pointed{std::move(pointed)} {}
Pointer::Pointer(const Pointer &other)
	: AstBase(other), pointed{clone(other.pointed)} {}
Pointer::Pointer(Pointer &&other) noexcept = default;
Pointer &Pointer::operator=(const Pointer &other) {
	AstBase::operator=(other);
	pointed = clone(other.pointed);
	return *this;
}
Pointer &Pointer::operator=(Pointer &&other) noexcept = default;
Pointer::~Pointer() {}
void Pointer::children(children_cb cb) const { cb(to_ast_base(pointed.get())); }
void Pointer::summarize(std::ostream &os) const { os << "Pointer"; }
Primitive::Primitive(const Location &location, const ::type::Primitive prim)
	: AstBase(location), prim{prim} {}
Primitive::Primitive(const Primitive &other) = default;
Primitive::Primitive(Primitive &&other) noexcept = default;
Primitive &Primitive::operator=(const Primitive &other) = default;
Primitive &Primitive::operator=(Primitive &&other) noexcept = default;
void Primitive::summarize(std::ostream &os) const {
	os << "Primitive(" << prim.name() << ')';
}
} // namespace type
Program::Program(const Location &location, std::vector<TopLevelAst> tops)
	: AstBase(location), tops{std::move(tops)} {}
Program::~Program() {}
void Program::children(children_cb cb) const {
	for (auto &top : tops) {
		cb(to_ast_base(&top));
	}
}
void Program::summarize(std::ostream &os) const { os << "Program"; }
} // namespace ast