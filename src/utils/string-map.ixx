module;
#include <string>
#include <string_view>
#include <unordered_map>
export module utils.string_map;

export namespace utils {
inline bool operator<(const std::string &str, const std::string_view &view) {
	return std::string_view{str} < view;
}
inline bool operator<(const std::string_view &view, const std::string &str) {
	return view < std::string_view{str};
}

struct string_hash {
	using hash_type = std::hash<std::string_view>;
	using is_transparent = void;

	std::size_t operator()(const char *str) const { return hash_type{}(str); }
	std::size_t operator()(const std::string_view str) const {
		return hash_type{}(str);
	}
	std::size_t operator()(std::string const &str) const {
		return hash_type{}(str);
	}
};

template<class T>
using string_map =
		std::unordered_map<std::string, T, string_hash, std::equal_to<>>;
} // namespace utils