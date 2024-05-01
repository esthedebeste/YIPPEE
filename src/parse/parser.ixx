module;
#include <string_view>
export module parser;
import ast;

export namespace parser {
ast::Program parse_program(std::string_view filename, std::string_view content);
} // namespace parser