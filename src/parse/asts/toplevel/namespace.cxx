module;
#include <coroutine>

#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <utility>
#include <vector>
module ast.top.space;
import ast;
import utils;
namespace ast {
namespace top {
Namespace::Namespace(const Location &location, const Identifier &name,
					 const std::vector<TopLevelAst> &tops)
	: AstBase(location), name{name}, tops(tops) {}
Namespace::Namespace(const Namespace &other) = default;
Namespace::Namespace(Namespace &&other) = default;
Namespace &Namespace::operator=(const Namespace &other) = default;
Namespace &Namespace::operator=(Namespace &&other) noexcept = default;
Namespace::~Namespace() {}
void Namespace::children(children_cb cb) const {
	for (auto &child : tops)
		cb(base_ptr(&child));
}
void Namespace::summarize(std::ostream &os) const {
	os << "Namespace(" << name << ')';
}
} // namespace top
} // namespace ast
