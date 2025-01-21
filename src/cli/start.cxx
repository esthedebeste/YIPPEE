module;
#include <fstream>
#include <iostream>
#include <string>
#include <vector>
module cli.start;
import parser;
import ast;
import ast.graph;
import backend.llvm;
int start(Args args) {
	std::vector<ast::Program> programs;
	for (std::string path : args.files) {
		std::cerr << "opening " << path << "...\n";
		try {
			std::cerr << "parsing " << path << "...\n";
			std::ifstream in(path);
			std::string input((std::istreambuf_iterator<char>(in)),
							  std::istreambuf_iterator<char>());
			in.close();
			programs.push_back(parser::parse_program(path, input));
		} catch (const std::exception &e) {
			std::cerr << e.what() << std::endl;
			return 1;
		}
	}
	if (!args.parse_tree.empty()) {
		std::cerr << "writing parse tree to " << args.parse_tree << "...\n";
		std::ofstream outfile(args.parse_tree);
		std::vector<const ast::AstBase *> nodes;
		for (ast::Program program : programs)
			nodes.push_back(&program);
		graph(nodes, outfile);
	}
	std::cerr << "generating object file to " << args.output << "...\n";
	backend::llvm::generate_object_file(programs, args.output);
	return 0;
}
