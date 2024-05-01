module;
module ast.def;
import ast;
namespace ast {
AnyAstPtr base_ptr(const Ast *ptr) {
	return ptr->visit([](auto &x) -> AnyAstPtr { return &x; });
}
AnyAstPtr base_ptr(const ExprAst *ptr) {
	return ptr->visit([](auto &x) -> AnyAstPtr { return &x; });
}
AnyAstPtr base_ptr(const StatementAst *ptr) {
	return ptr->visit([](auto &x) -> AnyAstPtr { return &x; });
}
AnyAstPtr base_ptr(const TopLevelAst *ptr) {
	return ptr->visit([](auto &x) -> AnyAstPtr { return &x; });
}
AnyAstPtr base_ptr(const TypeAst *ptr) {
	return ptr->visit([](auto &x) -> AnyAstPtr { return &x; });
}
} // namespace ast