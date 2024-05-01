module;
#include <fstream>
#include <iostream>
#include <string>
export module cli.start;
import cli.args;

export int start(Args args);

module :private;
import parser;
import ast.graph;
import backend.llvm;
int start(Args args) {
	for (std::string path : args.files) {
		std::cerr << "opening " << path << "...\n";
		try {
			std::cerr << "parsing " << path << "...\n";
			std::ifstream in(path);
			std::string input((std::istreambuf_iterator<char>(in)),
							  std::istreambuf_iterator<char>());
			in.close();
			auto program = parser::parse_program(path, input);
			if (!args.parse_tree.empty()) {
				std::cerr << "writing parse tree to " << args.parse_tree << "...\n";
				std::ofstream outfile(args.parse_tree);
				graph(&program, outfile);
			}
			std::cerr << "generating object file to " << args.output << "...\n";
			backend::llvm::generate_object_file(program, args.output);
		} catch (const std::exception &e) {
			std::cerr << e.what() << std::endl;
			return 1;
		}
	}
	return 0;
}
