// used to be used. not anymore. todo add automated tests again. maybe with an existing testing library?

#include <functional>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
import utils;

auto max(auto a, auto b) { return a > b ? a : b; }
size_t length(const std::string &s) { return s.size(); }
template<std::size_t N>
std::size_t length(const char (&)[N]) { return N - 1; }

size_t maxlen;
size_t level;
int main() {
	std::vector<std::function<bool()>> test_cases;
	std::vector<std::string> test_names;
#define TEST_CASE(name)                       \
	;                                         \
	test_names.push_back(name);               \
	test_cases.resize(test_cases.size() + 1); \
	test_cases[test_cases.size() - 1] = [&]() -> bool

#define NEW_LEVEL(name, lev) \
	maxlen = 0, level = lev; \
	std::cerr << std::string(lev, ' ') << " - " << name << "\n";

#define SECTION(name) NEW_LEVEL(name, 2)
#define SUBSECTION(name) NEW_LEVEL(name, 4)

#define REQUIRENF(name, failstr, ...)                                    \
	maxlen = max(maxlen, length(name));                                  \
	std::cerr << std::string(level + 2, ' ') << " - [" << name << "]..." \
			  << std::flush;                                             \
	if (!(__VA_ARGS__)) {                                                \
		std::cerr << "\b\b\b FAIL! " << failstr << "\n";                 \
		throw std::runtime_error("test failed");                         \
	}                                                                    \
	std::cerr << std::string(maxlen - length(name), ' ') << " :D\n"

#define REQUIREN(name, ...) REQUIRENF(name, "", __VA_ARGS__)
#define REQUIREF(failstr, ...) REQUIRENF(#__VA_ARGS__, failstr, __VA_ARGS__)
#define REQUIRE(...) REQUIRENF(#__VA_ARGS__, "", __VA_ARGS__)

#define TEQUAL(A, B)                                                             \
	REQUIREN(                                                                    \
			fmt(typeid(A).name(), "(" #A ") == ", typeid(B).name(), "(" #B ")"), \
			A == B)
#define EQUAL(A, B) REQUIRE(A == B)
	// TODO: make tests and include them here (or - find a real testing framework)
	;
	for (std::size_t i = 0; i < test_cases.size(); i++) {
		std::cerr << " - " << test_names[i] << " \n"
				  << std::flush;
		test_cases[i](); // if a test fails it throws
	}
	std::cerr << "Success!";
	return 0;
}