module;
#include <sstream>
#include <string>
export module utils.fmt;
export template<typename... T>
std::string fmt(T... args) {
	std::stringstream ss;
	(ss << ... << args);
	return ss.str();
}
