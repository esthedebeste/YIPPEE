module;
#include <iostream>
#include <optional>
#include <sstream>
#include <string>
#include <vector>
export module cli.args;

export class Args {
	bool set_output(std::string out);
	bool set_parse_tree(std::string parse_tree);

public:
	std::string argv0;
	std::vector<std::string> files;
	std::string output;
	std::string parse_tree;
	friend std::optional<Args> parse_args(int argc, char **argv);
};

export std::optional<Args> parse_args(int argc, char **argv);

module :private;
import utils.fmt;

bool Args::set_output(std::string out) {
	if (!output.empty())
		return false;
	output = std::move(out);
	return true;
}

bool Args::set_parse_tree(std::string pt) {
	if (!parse_tree.empty())
		return false;
	parse_tree = std::move(pt);
	return true;
}

constexpr char USAGE[] = R"usage(
Usage:
  yippee [options] <files> -o <output>

Options:
  --parse-tree <parse tree location>
)usage";

std::optional<Args> parse_args(const int argc, char **argv) {
	constexpr auto err = [](auto str) {
		std::cerr << "yippee: error: " << str << '\n'
				  << USAGE;
		return std::nullopt;
	};

	Args args;
	if (argc == 0)
		return err("no argv0");
	args.argv0 = argv[0];
	if (argc == 1)
		return err("no args :( i dont wanna be a noop");
	for (int i = 1; i < argc; i++) {
#define require_more(name, amount) \
	if (i + (amount) >= argc)      \
		return err("expected " #amount " more argument(s) after " name);

		std::string_view arg = argv[i];
		if (arg.starts_with("--")) {
			if (arg == "--help") {
				std::cout << USAGE;
				return std::nullopt;
			} else if (arg == "--output") {
				require_more("--output", 1);
				if (!args.set_output(argv[++i]))
					return err("multiple outputs");
			} else if (arg == "--parse-tree") {
				require_more("--parse-tree", 1);
				if (!args.set_parse_tree(argv[++i]))
					return err("multiple parse tree outputs");
			} else
				return err("unknown flag " + std::string(arg));
		} else if (arg.starts_with('-')) {
			for (const char c : arg.substr(1)) {
				switch (c) {
					case 'o':
						require_more("-o", 1);
						if (!args.set_output(argv[++i]))
							return err("multiple outputs");
						break;
					default:
						return err(fmt("unknown short option ", c));
				}
			}
		} else // arg doesn't start with - or --, interpret as a file
			args.files.push_back(std::string(arg));
	}
	if (args.output.empty())
		return err("no output specified");
	return args;
}