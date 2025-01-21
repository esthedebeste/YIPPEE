module;
#include <span>
#include <string_view>
export module backend.llvm;
import ast;

export namespace backend::llvm {
void generate_object_file(std::span<ast::Program> programs, std::string_view to);
} // namespace backend::llvm
