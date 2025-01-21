module;
#include <cmath>
#include <memory>
#include <string>
#include <string_view>
#include <typeinfo>
#include <vector>

#include <functional>
#include <iostream>
#include <optional>
#include <unordered_map>
export module reader;
import utils;

export namespace reader {
struct Reader {
	std::string_view source{};
	std::size_t m_index{}, line{}, column{};
	[[nodiscard]] std::size_t index() const { return m_index; }
	std::size_t &index(const std::size_t new_index) {
		std::size_t old_index = m_index;
		m_index = new_index;
		if (new_index <= old_index)
			return m_index;
		for (; old_index < new_index; ++old_index) {
			if (source[old_index] == '\n') {
				++line;
				column = 1;
			} else {
				++column;
			}
		}
		return m_index;
	}
	explicit Reader(const std::string_view source)
		: source(source), m_index(0), line(1), column(1) {}
	Reader(const std::string_view source, const std::size_t index, const std::size_t line,
		   const std::size_t column)
		: source(source), m_index(index), line(line), column(column) {}

	bool try_consume(const std::string_view str) {
		if (source.substr(index(), str.size()) == str) {
			index(index() + str.size());
			column += str.size();
			return true;
		}
		return false;
	}

	[[nodiscard]] std::size_t remaining() const { return source.size() - index(); }
	[[nodiscard]] bool eof() const { return index() >= source.size(); }
	[[nodiscard]] char operator*() const { return source[index()]; }
	[[nodiscard]] char operator[](const std::size_t i) const { return source[index() + i]; }
	[[nodiscard]] char peek() const { return source[index()]; }
	[[nodiscard]] char peek(const std::size_t i) const { return source[index() + i]; }
	char consume() {
		const char c = source[index()];
		index(index() + 1);
		return c;
	}
	void consume(const std::size_t n) { index(index() + n); }
};
} // namespace reader