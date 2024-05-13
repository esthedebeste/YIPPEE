module;
#include <memory>
#include <string>
#include <vector>
export module ast.exprs.member_call;
import ast.def;
import utils;
namespace ast {
namespace expr {
export struct MemberCall final : AstBase {
	ExprPtr callee;
	std::string name;
	std::vector<TypeAst> type_arguments;
	std::vector<ExprAst> arguments;
	MemberCall(const Location &location, ExprPtr callee, std::string name, std::vector<TypeAst> type_arguments, std::vector<ExprAst> arguments);
	MemberCall(const MemberCall &other);
	MemberCall(MemberCall &&other) noexcept;
	MemberCall &operator=(const MemberCall &other);
	MemberCall &operator=(MemberCall &&other) noexcept;
	void children(children_cb) const override;
	void summarize(std::ostream &os) const override;
};
} // namespace expr
} // namespace ast
