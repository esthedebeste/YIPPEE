cmake_minimum_required(VERSION 3.28)

project(yippee CXX)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ON)

add_library(libyippee)
target_sources(libyippee PRIVATE
        "src/parse/asts/program.cxx"
        "src/parse/asts/ast-def.cxx"
        "src/compile/type/array.cxx"
        "src/compile/type/pointer.cxx"
        "src/compile/type/function.cxx"
        "src/compile/type/compound.cxx"
        "src/parse/asts/types/array.cxx"
        "src/parse/asts/types/primitive.cxx"
        "src/parse/asts/types/pointer.cxx"
        "src/parse/asts/types/named.cxx"
        "src/parse/asts/toplevel/struct.cxx"
        "src/parse/asts/toplevel/namespace.cxx"
        "src/parse/asts/toplevel/function.cxx"
        "src/parse/asts/statements/variable.cxx"
        "src/parse/asts/statements/return.cxx"
        "src/parse/asts/statements/expr.cxx"
        "src/parse/asts/statements/block.cxx"
        "src/parse/asts/statements/if.cxx"
        "src/parse/asts/statements/while.cxx"
        "src/parse/asts/statements/for.cxx"
        "src/parse/asts/exprs/array.cxx"
        "src/parse/asts/exprs/unary.cxx"
        "src/parse/asts/exprs/number.cxx"
        "src/parse/asts/exprs/identifier.cxx"
        "src/parse/asts/exprs/conditional.cxx"
        "src/parse/asts/exprs/comparison.cxx"
        "src/parse/asts/exprs/binop.cxx"
        "src/parse/asts/exprs/subscript.cxx"
        "src/parse/asts/exprs/member.cxx"
        "src/parse/asts/exprs/call.cxx"
        "src/parse/asts/exprs/member-call.cxx"
        "src/parse/asts/exprs/create.cxx"
        "src/parse/parser.cxx"
        "src/parse/asts/shared/type-argument.cxx"
        "src/parse/asts/shared/name.cxx"
        "src/compile/naming.cxx"
        "src/compile/llvm.cxx"
        "src/compile/type/type.cxx"
)

target_sources(libyippee PUBLIC FILE_SET modules TYPE CXX_MODULES FILES
        "src/utils/variant.ixx"
        "src/utils/utils.ixx"
        "src/utils/type-utils.ixx"
        "src/utils/type-holder.ixx"
        "src/utils/string-map.ixx"
        "src/utils/fmt.ixx"
        "src/parse/reader.ixx"
        "src/parse/parser.ixx"
        "src/parse/asts/operators.ixx"
        "src/compile/operations.ixx"
        "src/compile/backend.ixx"
        "src/compile/llvm.ixx"
        "src/cli/start.ixx"
        "src/cli/args.ixx"
        "src/parse/asts/program.ixx"
        "src/parse/asts/shared/type-argument.ixx"
        "src/parse/asts/shared/name.ixx"
        "src/parse/asts/graph.ixx"
        "src/parse/asts/ast.ixx"
        "src/parse/asts/ast-def.ixx"
        "src/compile/type/array.ixx"
        "src/compile/type/type.ixx"
        "src/compile/type/type-decl.ixx"
        "src/compile/type/primitive.ixx"
        "src/compile/type/pointer.ixx"
        "src/compile/type/function.ixx"
        "src/compile/type/compound.ixx"
        "src/parse/asts/types/array.ixx"
        "src/parse/asts/types/primitive.ixx"
        "src/parse/asts/types/pointer.ixx"
        "src/parse/asts/types/named.ixx"
        "src/parse/asts/toplevel/struct.ixx"
        "src/parse/asts/toplevel/namespace.ixx"
        "src/parse/asts/toplevel/function.ixx"
        "src/parse/asts/statements/variable.ixx"
        "src/parse/asts/statements/return.ixx"
        "src/parse/asts/statements/expr.ixx"
        "src/parse/asts/statements/block.ixx"
        "src/parse/asts/statements/if.ixx"
        "src/parse/asts/statements/while.ixx"
        "src/parse/asts/statements/for.ixx"
        "src/parse/asts/exprs/array.ixx"
        "src/parse/asts/exprs/unary.ixx"
        "src/parse/asts/exprs/number.ixx"
        "src/parse/asts/exprs/identifier.ixx"
        "src/parse/asts/exprs/conditional.ixx"
        "src/parse/asts/exprs/comparison.ixx"
        "src/parse/asts/exprs/binop.ixx"
        "src/parse/asts/exprs/subscript.ixx"
        "src/parse/asts/exprs/member.ixx"
        "src/parse/asts/exprs/call.ixx"
        "src/parse/asts/exprs/member-call.ixx"
        "src/parse/asts/exprs/create.ixx"
        "src/compile/naming.ixx"
)
target_include_directories(libyippee PUBLIC "include")
set_target_properties(libyippee PROPERTIES CXX_SCAN_FOR_MODULES ON)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

target_include_directories(libyippee PUBLIC ${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
target_compile_definitions(libyippee PUBLIC ${LLVM_DEFINITIONS_LIST})
llvm_map_components_to_libnames(llvm_libs core)
target_link_libraries(libyippee PRIVATE ${llvm_libs})

add_executable(yippee "src/main.cpp")
target_link_libraries(yippee libyippee)

add_executable(yippee_tests "test/tests.cpp")
target_link_libraries(yippee_tests libyippee)

add_test(NAME tests COMMAND yippee_tests)