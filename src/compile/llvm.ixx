module;
#include <string_view>
export module backend.llvm;
import ast;

export namespace backend::llvm {
void generate_object_file(const ast::Program &program, std::string_view to);
} // namespace backend::llvm
