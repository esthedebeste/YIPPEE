module;
#include <memory>
#include <string_view>
#include <utility>
module ast.exprs.member_call;
import ast;
import operators;
import utils;

namespace ast {
namespace expr {
MemberCall::MemberCall(const Location &location, ExprPtr callee, std::string name, std::vector<TypeAst> type_arguments, std::vector<ExprAst> arguments)
	: AstBase(location), callee{std::move(callee)}, name{std::move(name)},
	  type_arguments{std::move(type_arguments)}, arguments{std::move(arguments)} {}
MemberCall::MemberCall(const MemberCall &other)
	: AstBase(other), callee{clone(other.callee)}, name{other.name},
	  type_arguments{other.type_arguments}, arguments{other.arguments} {}
MemberCall::MemberCall(MemberCall &&other) noexcept = default;
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
	cb(base_ptr(callee.get()));
	for (const auto &arg : type_arguments)
		cb(base_ptr(&arg));
	for (const auto &arg : arguments)
		cb(base_ptr(&arg));
}
void MemberCall::summarize(std::ostream &os) const { os << "MemberCall(" << name << ")"; }
} // namespace expr
} // namespace ast
