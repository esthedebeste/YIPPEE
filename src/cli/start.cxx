module;
#include <filesystem>
#include <fstream>
#include <iostream>
#include <ranges>
#include <string>
#include <vector>
module cli.start;
import parser;
import ast;
import ast.graph;
import backend.llvm;
int start(Args args) {
	std::vector<std::string> files{};
	for (const std::string &path : args.inputs) {
		if (std::filesystem::is_directory(path))
			files.append_range(std::filesystem::recursive_directory_iterator(path) | std::views::filter([](const auto &entry) { return !entry.is_directory(); }) | std::views::transform([](const auto &entry) { return entry.path().string(); }));
		else
			files.push_back(path);
	}
	std::vector<ast::Program> programs;
	// keep file contents saved so `Range::content`s remain intact
	std::vector<std::string> file_contents{};
	for (const std::string &path : files) {
		std::cerr << "opening " << path << "...\n";
		try {
			std::cerr << "parsing " << path << "...\n";
			std::ifstream in(path);
			if (in.fail() || !in.is_open()) {
				std::cerr << "failed to open " << path << " :(\n";
				return 1;
			}
			const std::string &input = file_contents.emplace_back(std::istreambuf_iterator(in), std::istreambuf_iterator<char>());
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
	try {
		backend::llvm::generate_object_file(programs, args.output);
	} catch (std::runtime_error &error) {
		std::cerr << error.what() << std::endl;
	}
	return 0;
}
