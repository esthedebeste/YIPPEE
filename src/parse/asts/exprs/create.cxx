module;
#include <memory>
#include <ostream>
#include <ranges>
#include <vector>
module ast.exprs.create;
import utils;
import ast;

namespace ast::expr {
Create::Create(const Location &location, TypePtr type,
			   std::vector<std::pair<std::string, ExprAst>> args)
	: AstBase(location), type(std::move(type)), args(std::move(args)) {}
Create::Create(const Create &other)
	: AstBase(other), type{clone(other.type)}, args{other.args} {}
Create::Create(Create &&other) = default;
Create &Create::operator=(const Create &other) {
	AstBase::operator=(other);
	type = clone(other.type);
	args = other.args;
	return *this;
}
Create &Create::operator=(Create &&other) noexcept = default;
void Create::children(children_cb cb) const {
	cb(base_ptr(type.get()));
	for (const auto &val : args | std::views::values)
		cb(base_ptr(&val));
}
void Create::summarize(std::ostream &os) const { os << "Create"; }
} // namespace ast::expr