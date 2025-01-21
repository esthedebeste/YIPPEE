cmake_minimum_required(VERSION 3.28)

project(yippee C CXX)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ON)

function(wall target)
    if(MSVC)
        target_compile_options(${target} PRIVATE /W3)
    else()
        target_compile_options(${target} PRIVATE -Wall)
    endif()
endfunction()

add_library(libyippee)
wall(libyippee)
target_sources(libyippee PRIVATE
        "src/cli/start.cxx"
        "src/parse/parser.cxx"
        "src/parse/ast.cxx"
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
        "src/parse/operators.ixx"
        "src/compile/operations.ixx"
        "src/compile/backend.ixx"
        "src/compile/llvm.ixx"
        "src/cli/start.ixx"
        "src/cli/args.ixx"
        "src/parse/graph.ixx"
        "src/parse/ast.ixx"
        "src/compile/type/type.ixx"
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
wall(yippee)
target_link_libraries(yippee libyippee)

add_executable(yippee_tests "test/tests.cpp")
wall(yippee_tests)
target_link_libraries(yippee_tests libyippee)

add_test(NAME tests COMMAND yippee_tests)