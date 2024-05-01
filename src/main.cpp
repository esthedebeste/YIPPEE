#include <optional>
import cli.args;
import cli.start;

int main(const int argc, char **argv) {
	const std::optional<Args> argsm = parse_args(argc, argv);
	if (!argsm)
		return 1;
	return start(*argsm);
}