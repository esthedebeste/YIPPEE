module;
#include <array>
#include <charconv>
#include <cmath>
#include <functional>
#include <iostream>
#include <memory>
#include <optional>
#include <ranges>
#include <span>
#include <sstream>
#include <string>
#include <string_view>
#include <typeinfo>
#include <unordered_map>
#include <utility>
#include <vector>
module parser;
import reader;
import utils;
import operators;
import type;

namespace {
auto take_first() { return std::nullopt; }
auto take_first(auto lambda, auto... lambdas) {
	auto result = lambda();
	if (result)
		return result;
	return static_cast<decltype(result)>(take_first(lambdas...));
}

const std::unordered_map<std::string_view, type::Primitive::p_type>
		primitive_types{{"boolean", type::Primitive::boolean},
						{"uint8", type::Primitive::uint8},
						{"int8", type::Primitive::int8},
						{"uint16", type::Primitive::uint16},
						{"int16", type::Primitive::int16},
						{"uint32", type::Primitive::uint32},
						{"int32", type::Primitive::int32},
						{"uint64", type::Primitive::uint64},
						{"int64", type::Primitive::int64},
						{"uint128", type::Primitive::uint128},
						{"int128", type::Primitive::int128},
						{"half", type::Primitive::half},
						{"float", type::Primitive::float_},
						{"double", type::Primitive::double_}};

struct Parser : reader::Reader {
	std::string_view filename;
	Parser(const std::string_view filename, const std::string_view content)
		: Reader(content), filename{filename} {}

	template<class Ret>
	using parser_t = std::optional<Ret> (Parser::*)();
	// utils
	template<class Variant, class... Ts>
	std::optional<Variant> try_(parser_t<Ts>... ts) {
		if (eof())
			return std::nullopt;
		return take_first([&]() -> std::optional<Variant> {
			Parser clone = *this;
			if (auto res = (clone.*ts)(); res) {
				*this = clone;
				return Variant(*res);
			}
			return std::nullopt;
		}...);
	}

	// all or nothing, make sure an optional parser either succeeds or fails with
	// no continued state
	template<class T>
	std::optional<T> aon(parser_t<T> t) {
		Parser clone = *this;
		if (auto res = std::invoke(t, clone); res) {
			*this = clone;
			return res;
		}
		return std::nullopt;
	}
	[[nodiscard]] ast::Location get_loc() const {
		return ast::Location(filename, line, column, index());
	}
	[[nodiscard]] ast::Range get_range(const ast::Location &start) const {
		const ast::Location end = get_loc();
		return ast::Range(start, end, source.substr(start.index, end.index - start.index));
	}

	void error(auto &&...args) {
		throw std::runtime_error(fmt(args..., " at ", get_loc()));
	}

	void block_comment_post_start() {
		while (true) {
			if (try_consume("/*"))
				block_comment_post_start();
			else if (try_consume("*/"))
				return;
			else
				consume();
		}
	}

	void ws() {
		while (true) {
			if (eof())
				break;
			if (std::isspace(**this))
				consume();
			else if (try_consume("//"))
				while (true) {
					if (**this == '\n')
						break;
					consume();
				}
			else if (try_consume("/*"))
				block_comment_post_start();
			else
				break;
		}
	}
	void must_ws() {
		if (eof())
			return;
		if (!std::isspace(**this))
			error("Expected whitespace");
		consume();
		ws();
	}
	void expect(std::string_view str) {
		if (!try_consume(str))
			error("Expected ", str);
	}

	static bool is_id_start(const char c) { return std::isalpha(c) || c == '_'; }

	static bool is_id_continue(const char c) {
		return is_id_start(c) || std::isdigit(c);
	}

	std::string_view parse_name_str() {
		const std::size_t start = index();
		if (!is_id_start(**this))
			return {};
		while (true) {
			if (eof() || !is_id_continue(**this)) {
				const std::string_view name{source.data() + start, index() - start};
				if (name == "operator")
					for (const std::string_view str : operators::all_strings)
						if (try_consume(str))
							return str;
				return name;
			}
			consume();
		}
		std::unreachable();
	}

	bool keyword(const std::string_view str) {
		if (str.size() > remaining())
			return false;
		if (std::string_view(source.data() + index(), str.size()) != str)
			return false;
		if (str.size() == remaining()) {
			consume(str.size());
			return true;
		}
		if (is_id_continue(source[index() + str.size()])) // not a keyword, just a
														  // prefix of a name
			return false;
		consume(str.size());
		return true;
	}

	std::optional<ast::Name> parse_name() {
		const auto loc_start = get_loc();
		const auto name = parse_name_str();
		if (name.empty())
			return std::nullopt;
		return std::make_optional(ast::Name(get_range(loc_start), name));
	}

	std::optional<ast::Identifier> parse_identifier() {
		const auto loc_start = get_loc();
		std::vector<ast::Name> names;
		// names can start with a ::
		if (try_consume("::"))
			names.push_back(ast::Name{get_range(loc_start), ""});
		while (true) {
			auto name = parse_name();
			if (!name)
				return std::nullopt; // :: not followed by a name
			if (!try_consume("::"))
				return std::make_optional(
						ast::Identifier(get_range(loc_start), std::move(names), *name));
			names.push_back(std::move(*name));
		}
	}

	// types

	ast::type::Pointer parse_pointer_type(ast::TypeAst type) {
		const auto loc_start = get_loc();
		ws();
		return ast::type::Pointer(get_range(loc_start), std::make_unique<ast::TypeAst>(type));
	}
	ast::TypeAst parse_array_type(ast::TypeAst type) {
		const auto loc_start = get_loc();
		ws();
		auto size = parse_literal_expr();
		if (!size) {
			ws();
			if (!try_consume("]"))
				error("Expected array size (a number)");
			return ast::type::Slice(get_range(loc_start), std::make_unique<ast::TypeAst>(type));
		}
		if (!size->value.is<ast::expr::Number::uint_t>())
			error("Expected array size to be an integer");
		ws();
		expect("]");
		ws();
		return ast::type::Array(get_range(loc_start), std::make_unique<ast::TypeAst>(type),
								size->value.get<ast::expr::Number::uint_t>());
	}
	std::optional<ast::type::Primitive> parse_primitive_type() {
		const auto loc_start = get_loc();
		const auto name = parse_name_str();
		if (name.empty())
			return std::nullopt;
		const auto it = primitive_types.find(name);
		if (it == primitive_types.end())
			return std::nullopt;
		return std::make_optional(ast::type::Primitive(get_range(loc_start), type::Primitive(it->second)));
	}
	std::optional<ast::type::Named> parse_named_type() {
		const auto loc_start = get_loc();
		const auto id = parse_identifier();
		if (!id)
			return std::nullopt;
		std::vector<ast::TypeAst> parameters{};
		if (try_consume("<")) {
			while (true) {
				ws();
				auto type = parse_type();
				if (!type)
					break;
				ws();
				parameters.push_back(std::move(*type));
				try_consume(","); // optional trailing comma
			}
			ws();
			expect(">");
		}
		return std::make_optional(
				ast::type::Named(get_range(loc_start), *id, std::move(parameters)));
	}
	std::optional<ast::TypeAst> parse_type() {
		const auto loc_start = get_loc();
		ws();
		bool is_const = keyword("const");
		ws();
		auto subject = try_<ast::TypeAst>(&Parser::parse_primitive_type,
										  &Parser::parse_named_type);
		if (!subject)
			return std::nullopt;
		ast::TypeAst type = std::move(*subject);
		ws();
		while (true) {
			switch (**this) {
				case '*':
					consume();
					type = parse_pointer_type(std::move(type));
					break;
				case '[':
					consume();
					type = parse_array_type(std::move(type));
					break;
				default:
					if (is_const)
						type = ast::type::Constant(get_range(loc_start), std::make_unique<ast::TypeAst>(std::move(type)));
					return std::make_optional(type);
			}
		}
	}

	std::optional<ast::TypeAst> parse_colon_type() {
		if (!try_consume(":"))
			return std::nullopt;
		ws();
		return parse_type();
	}

	// expressions

	std::optional<operators::unary> parse_unary_op() {
		for (auto op : operators::all_unaries())
			if (op >= operators::unary::START_KEYWORD && op <= operators::unary::END_KEYWORD
						? keyword(string(op))
						: try_consume(string(op)))
				return std::make_optional(op);
		return std::nullopt;
	}
	std::optional<ast::expr::Unary> parse_unary() {
		const auto loc_start = get_loc();
		auto op = parse_unary_op();
		if (!op)
			return std::nullopt;
		ws();
		auto expr = parse_atom();
		if (!expr)
			error("Expected expression after ", *op);
		return std::make_optional(
				ast::expr::Unary(get_range(loc_start), *op, std::make_unique<ast::ExprAst>(*expr)));
	}

	std::optional<ast::expr::Number> parse_literal_expr() {
		const auto loc_start = get_loc();
		const std::size_t start = index();
		if (!std::isdigit(**this))
			return std::nullopt;
		while (true) {
			if (eof() || !std::isdigit(**this))
				break;
			consume();
		}
		bool is_float = false;
		if (peek() == '.' && std::isdigit(peek(1))) {
			consume(); // .
			consume(); // digit
			is_float = true;
			while (true) {
				if (eof() || !std::isdigit(**this))
					break;
				consume();
			}
		}
		if (try_consume("e") || try_consume("E")) {
			is_float = true;
			if (try_consume("+") || try_consume("-")) {
				if (!std::isdigit(**this))
					error("Expected digit after e ");
			}
			while (true) {
				if (eof() || !std::isdigit(**this))
					break;
				consume();
			}
		}
		std::string_view number{source.data() + start, index() - start};

		if (is_float) {
			ast::expr::Number::float_t value;
			auto result = std::from_chars(number.data(), number.data() + number.size(), value);
			if (result.ec == std::errc::invalid_argument)
				error("Invalid float number");
			if (result.ec == std::errc::result_out_of_range)
				error("Float number out of range (compiler limitation at float", sizeof(value) * 8, ")");
			return std::make_optional(ast::expr::Number(get_range(loc_start), value));
		} else {
			ast::expr::Number::uint_t value;
			auto result = std::from_chars(number.data(), number.data() + number.size(), value);
			if (result.ec == std::errc::invalid_argument)
				error("Invalid integer number");
			if (result.ec == std::errc::result_out_of_range)
				error("Integer number out of range (compiler limitation at uint", sizeof(value) * 8, ")");
			return std::make_optional(ast::expr::Number(get_range(loc_start), value));
		}
	}
	std::optional<ast::ExprAst> parse_bracketed_expr() {
		auto loc_start = get_loc();
		if (!try_consume("("))
			return std::nullopt;
		ws();
		auto expr = parse_expr();
		if (!expr)
			error("Expected expression");
		ws();
		expect(")");
		return expr;
	}

	std::optional<ast::expr::Create> parse_create_expr() {
		const auto loc_start = get_loc();
		if (!try_consume("create"))
			return std::nullopt;
		ws();
		auto type = parse_type();
		if (!type)
			error("Expected type after create");
		ws();
		expect("{");
		std::vector<std::pair<std::string_view, ast::ExprAst>> arguments;
		while (true) {
			ws();
			std::string_view name{parse_name_str()};
			if (!name.empty()) {
				ws();
				expect(":");
				ws();
			}
			auto argument = parse_expr();
			if (!argument)
				break;
			arguments.emplace_back(std::move(name), std::move(*argument));
			ws();
			try_consume(",");
			ws();
			if (try_consume("}"))
				break;
		}
		ws();
		return std::make_optional(ast::expr::Create(
				get_range(loc_start), std::make_unique<ast::TypeAst>(*type), std::move(arguments)));
	}
	std::optional<ast::expr::Identifier> parse_identifier_expr() {
		const auto loc_start = get_loc();
		auto id = parse_identifier();
		if (!id)
			return std::nullopt;
		const auto type_arguments = parse_type_arguments();
		return std::make_optional(ast::expr::Identifier(get_range(loc_start), std::move(*id), type_arguments));
	}
	std::optional<ast::expr::Array> parse_array_expr() {
		const auto loc_start = get_loc();
		if (!try_consume("["))
			return std::nullopt;
		ws();
		std::vector<ast::ExprAst> values = parse_exprs(std::nullopt, "]", ",");
		return std::make_optional(ast::expr::Array(get_range(loc_start), std::move(values)));
	}

	std::optional<ast::ExprAst> parse_postfixed() {
		return try_<ast::ExprAst>(
				&Parser::parse_array_expr, &Parser::parse_literal_expr,
				&Parser::parse_bracketed_expr, &Parser::parse_create_expr,
				&Parser::parse_identifier_expr);
	}

	std::vector<ast::ExprAst> parse_exprs(const std::optional<std::string_view> &start, const std::string_view end, const std::string_view split = ",") {
		ws();
		if (start && !try_consume(*start))
			return {};
		ws();
		if (try_consume(end))
			return {};
		std::vector<ast::ExprAst> exprs;
		while (true) {
			ws();
			auto expr = parse_expr();
			if (!expr)
				error("Expected expression");
			exprs.push_back(std::move(*expr));
			ws();
			if (!try_consume(split)) {
				ws();
				expect(end);
				break;
			}
			if (try_consume(end))
				break;
		}
		ws();
		return exprs;
	}

	ast::expr::Call parse_call(ast::ExprAst subject) {
		auto loc_start = get_loc();
		auto arguments = parse_exprs(std::nullopt /* start char already parsed by parse_postfix_expr */, ")");
		return {get_range(loc_start), std::make_unique<ast::ExprAst>(std::move(subject)),
				std::move(arguments)};
	}

	ast::ExprAst parse_subscript(ast::ExprAst subject) {
		auto loc_start = get_loc();
		auto from = parse_expr();
		ws();
		bool exclusive = try_consume("...");
		if (exclusive || try_consume("..") || !from) {
			auto to = parse_expr();
			ws();
			expect("]");
			return ast::expr::Slice{get_range(loc_start), !exclusive,
									std::make_unique<ast::ExprAst>(subject), from.transform([](const auto &x) {
				return std::make_unique<ast::ExprAst>(x);
			}),
									to.transform([](const auto &x) {
				return std::make_unique<ast::ExprAst>(x);
			})};
		}
		expect("]");
		return ast::expr::Subscript{get_range(loc_start), std::make_unique<ast::ExprAst>(std::move(subject)),
									std::make_unique<ast::ExprAst>(std::move(*from))};
	}

	ast::ExprAst parse_member(ast::ExprAst subject) {
		const auto loc_start = get_loc();
		const auto name = parse_name_str();
		if (name.empty())
			error("Expected member name");
		const bool template_args = **this == '<';
		ws();
		if (!template_args && **this != '(')
			return ast::ExprAst(ast::expr::Member{get_range(loc_start), std::make_unique<ast::ExprAst>(std::move(subject)),
												  name});
		std::vector<ast::TypeAst> type_arguments = parse_type_arguments();
		std::vector<ast::ExprAst> arguments = parse_exprs("(", ")");
		return ast::ExprAst(ast::expr::MemberCall{get_range(loc_start), std::make_unique<ast::ExprAst>(std::move(subject)), name,
												  std::move(type_arguments), std::move(arguments)});
	}

	ast::expr::As parse_as(ast::ExprAst subject) {
		const auto loc_start = get_loc();
		ws();
		auto type = parse_type();
		if (!type)
			error("Expected type after `as`");
		return ast::expr::As{
				get_range(loc_start), std::make_unique<ast::ExprAst>(std::move(subject)),
				std::make_unique<ast::TypeAst>(std::move(*type))};
	}

	std::optional<ast::ExprAst> parse_postfix_expr() {
		auto o_subject = parse_postfixed();
		if (!o_subject)
			return std::nullopt;
		auto subject = std::move(*o_subject);
		while (true) {
			ws();
			switch (**this) {
				case '(':
					consume();
					subject = parse_call(std::move(subject));
					break;
				case '[':
					consume();
					subject = parse_subscript(std::move(subject));
					break;
				case '.':
					if (peek(1) == '.')
						return subject; // two consecutive dots means it's a range
					consume();
					subject = parse_member(std::move(subject));
					break;
				case 'a':
					if (keyword("as")) {
						subject = parse_as(std::move(subject));
						break;
					}
				default:
					return subject;
			}
		}
	}

	std::optional<ast::ExprAst> parse_atom() {
		return try_<ast::ExprAst>(&Parser::parse_unary,
								  &Parser::parse_postfix_expr);
	}

	std::optional<operators::binary> parse_binary_op() {
		for (auto op : operators::all_binaries()) {
			if (op == operators::binary::assign && try_consume("=="))
				return std::nullopt;
			const auto str = string(op);
			if (try_consume(str)) {
				return std::make_optional(op);
			}
		}
		return std::nullopt;
	}

	template<std::size_t binary_c>
	static auto opmatch_oneof(std::array<operators::binary, binary_c> binaries) {
		return [binaries](Parser &p) -> std::optional<operators::binary> {
			auto bin = p.parse_binary_op();
			if (!bin)
				return std::nullopt;
			for (auto option : binaries)
				if (*bin == option)
					return bin;
			return std::nullopt;
		};
	}

	std::optional<ast::ExprAst> parse_binop(auto operator_matcher,
											parser_t<ast::ExprAst> children) {
		const auto loc_start = get_loc();
		auto lhs = std::invoke(children, this);
		if (!lhs)
			return std::nullopt;
		while (true) {
			ws();
			Parser p = *this;
			std::optional<operators::binary> op = operator_matcher(*this);
			if (!op) {
				*this = p;
				return lhs;
			}
			ws();
			auto rhs = std::invoke(children, this);
			if (!rhs)
				error("Expected expression after ", *op);
			lhs = std::make_optional(ast::ExprAst(ast::expr::Binop(
					get_range(loc_start), std::make_unique<ast::ExprAst>(std::move(*lhs)), *op,
					std::make_unique<ast::ExprAst>(std::move(*rhs)))));
		}
	}

	std::optional<ast::ExprAst> parse_exponentiation() {
		return parse_binop(opmatch_oneof<1>({operators::binary::pow}),
						   &Parser::parse_atom);
	}

	std::optional<ast::ExprAst> parse_multiplicative() {
		return parse_binop(
				opmatch_oneof<3>({operators::binary::mul, operators::binary::div,
								  operators::binary::mod}),
				&Parser::parse_exponentiation);
	}

	std::optional<ast::ExprAst> parse_additive() {
		return parse_binop(
				opmatch_oneof<2>({operators::binary::add, operators::binary::sub}),
				&Parser::parse_multiplicative);
	}

	std::optional<ast::ExprAst> parse_bitwise_shift() {
		return parse_binop(
				opmatch_oneof<2>({operators::binary::b_shl, operators::binary::b_shr}),
				&Parser::parse_additive);
	}

	std::optional<ast::ExprAst> parse_bitwise_and() {
		return parse_binop(opmatch_oneof<1>({operators::binary::b_and}),
						   &Parser::parse_bitwise_shift);
	}

	std::optional<ast::ExprAst> parse_bitwise_xor() {
		return parse_binop(opmatch_oneof<1>({operators::binary::b_xor}),
						   &Parser::parse_bitwise_and);
	}

	std::optional<ast::ExprAst> parse_bitwise_or() {
		return parse_binop(opmatch_oneof<1>({operators::binary::b_or}),
						   &Parser::parse_bitwise_xor);
	}

	std::optional<operators::comparison> parse_comparison_op() {
		for (auto op : operators::all_comparisons()) {
			auto str = string(op);
			if (try_consume(str))
				return std::make_optional(op);
		}
		return std::nullopt;
	}
	std::optional<ast::ExprAst> parse_comparison() {
		const auto loc_start = get_loc();
		auto first = parse_bitwise_or();
		if (!first)
			return std::nullopt;
		ws();
		auto first_comparison_op = parse_comparison_op();
		if (!first_comparison_op) // early return to avoid vector heap allocations
			return std::make_optional(std::move(*first));
		ws();
		auto second_expr = parse_bitwise_or();
		if (!second_expr)
			error("Expected expression after ", *first_comparison_op);
		std::vector comparisons{*first_comparison_op};
		std::vector exprs{std::move(*first), std::move(*second_expr)};
		while (true) {
			ws();
			auto op = parse_comparison_op();
			if (!op)
				break;
			ws();
			auto second = parse_bitwise_or();
			if (!second)
				error("Expected expression after ", *op);
			comparisons.push_back(*op);
			exprs.push_back(std::move(*second));
		}
		if (exprs.size() == 1)
			return std::make_optional(std::move(exprs[0]));
		return std::make_optional(ast::ExprAst(ast::expr::Comparison(
				get_range(loc_start), std::move(comparisons), std::move(exprs))));
	}
	std::optional<ast::ExprAst> parse_logical_and() {
		return parse_binop(opmatch_oneof<1>({operators::binary::l_and}),
						   &Parser::parse_comparison);
	}
	std::optional<ast::ExprAst> parse_logical_or() {
		return parse_binop(opmatch_oneof<1>({operators::binary::l_or}),
						   &Parser::parse_logical_and);
	}
	std::optional<ast::ExprAst> parse_conditional() {
		const auto loc_start = get_loc();
		auto condition = parse_logical_or();
		if (!condition)
			return std::nullopt;
		ws();
		if (!try_consume("?"))
			return condition;
		ws();
		auto true_ = parse_assignment();
		if (!true_)
			error("Expected true-expr for conditional");
		ws();
		expect(":");
		ws();
		auto false_ = parse_assignment();
		if (!false_)
			error("Expected false-expr for conditional");
		return std::make_optional(ast::ExprAst(ast::expr::Conditional(
				get_range(loc_start), std::make_unique<ast::ExprAst>(std::move(*condition)),
				std::make_unique<ast::ExprAst>(std::move(*true_)),
				std::make_unique<ast::ExprAst>(std::move(*false_)))));
	}
	std::optional<ast::ExprAst> parse_assignment() {
		return parse_binop(
				[](Parser &p) -> std::optional<operators::binary> {
			if (p.eof())
				return std::nullopt;
			Parser peek = p;
			if (peek.try_consume("=")) {
				if (peek.try_consume("="))
					return std::nullopt;
				p = peek;
				return std::make_optional(operators::binary::assign);
			}
			return std::nullopt;
		},
				&Parser::parse_conditional);
	}

	std::optional<ast::ExprAst> parse_expr() { return parse_assignment(); }

	// statements

	std::optional<ast::stmt::Block> parse_block() {
		const auto loc_start = get_loc();
		if (!try_consume("{"))
			return std::nullopt;
		ws();
		std::vector<ast::StatementAst> statements;
		while (true) {
			auto statement = parse_statement();
			if (!statement)
				break;
			statements.push_back(std::move(*statement));
		}
		expect("}");
		return std::make_optional(
				ast::stmt::Block(get_range(loc_start), std::move(statements)));
	}
	std::optional<ast::stmt::Variable> parse_variable_declaration() {
		const auto loc_start = get_loc();
		ws();
		const auto name = parse_name_str();
		if (name.empty())
			return std::nullopt;
		ws();
		const auto type = aon(&Parser::parse_colon_type);
		ws();
		bool is_const = false;
		if (!try_consume(".=")) {
			if (try_consume(":="))
				is_const = true;
			else
				return std::nullopt; // not a variable declaration?
		}
		ws();
		auto expr = parse_expr();
		if (!expr)
			error("Expected expression");
		ws();
		if (!try_consume(";"))
			error("Expected ; after variable declaration");
		return std::make_optional(ast::stmt::Variable(
				get_range(loc_start), is_const, name,
				type ? std::make_optional(
							   std::make_unique<ast::TypeAst>(std::move(*type)))
					 : std::nullopt,
				std::make_unique<ast::ExprAst>(std::move(*expr))));
	}
	std::optional<ast::stmt::If> parse_if() {
		const auto loc_start = get_loc();
		if (!keyword("if"))
			return std::nullopt;
		ws();
		auto expr = parse_expr();
		if (!expr)
			error("Expected condition expression after if");
		ws();
		auto then = parse_statement();
		if (!then)
			error("Expected statement after if");
		std::optional<ast::StatementAst> otherwise{};
		if (keyword("else")) {
			ws();
			auto else_ = parse_statement();
			if (!else_)
				error("Expected statement after else");
			otherwise = std::make_optional(std::move(*else_));
		}
		return std::make_optional(ast::stmt::If(
				get_range(loc_start), std::make_unique<ast::ExprAst>(std::move(*expr)),
				std::make_unique<ast::StatementAst>(std::move(*then)),
				otherwise ? std::make_optional(std::make_unique<ast::StatementAst>(
									std::move(*otherwise)))
						  : std::nullopt));
	}
	std::optional<ast::stmt::While> parse_while() {
		const auto loc_start = get_loc();
		if (!keyword("while"))
			return std::nullopt;
		ws();
		auto expr = parse_expr();
		if (!expr)
			error("Expected condition expression after while");
		auto body = parse_statement();
		if (!body)
			error("Expected statement after while");
		return std::make_optional(ast::stmt::While(
				get_range(loc_start), std::make_unique<ast::ExprAst>(std::move(*expr)),
				std::make_unique<ast::StatementAst>(std::move(*body))));
	}
	std::optional<ast::stmt::For> parse_for() {
		const auto loc_start = get_loc();
		if (!keyword("for"))
			return std::nullopt;
		ws();
		auto init = parse_statement();
		auto cond = parse_expr();
		ws();
		if (!try_consume(";"))
			return std::nullopt;
		ws();
		auto incr = parse_expr();
		ws();
		auto body = parse_statement();
		if (!body)
			error("Expected statement after for");
		return std::make_optional(ast::stmt::For(
				get_range(loc_start),
				init ? std::make_optional(
							   std::make_unique<ast::StatementAst>(std::move(*init)))
					 : std::nullopt,
				cond ? std::make_optional(
							   std::make_unique<ast::ExprAst>(std::move(*cond)))
					 : std::nullopt,
				incr ? std::make_optional(
							   std::make_unique<ast::ExprAst>(std::move(*incr)))
					 : std::nullopt,
				std::make_unique<ast::StatementAst>(std::move(*body))));
	}
	std::optional<ast::stmt::Return> parse_return() {
		const auto loc_start = get_loc();
		if (!keyword("return"))
			return std::nullopt;
		ws();
		auto expr = parse_expr();
		if (!expr)
			error("Expected expression after return");
		ws();
		if (!try_consume(";"))
			error("Expected ; after return");
		return std::make_optional(ast::stmt::Return(
				get_range(loc_start), std::make_unique<ast::ExprAst>(std::move(*expr))));
	}

	std::optional<ast::stmt::Expr> parse_expr_statement() {
		const auto loc_start = get_loc();
		auto expr = parse_expr();
		if (!expr)
			return std::nullopt;
		ws();
		if (!try_consume(";"))
			error("Expected ; after expression statement");
		return std::make_optional(ast::stmt::Expr(
				get_range(loc_start), std::make_unique<ast::ExprAst>(std::move(*expr))));
	}

	std::optional<ast::StatementAst> parse_statement() {
		auto result = try_<ast::StatementAst>(
				&Parser::parse_block, &Parser::parse_if, &Parser::parse_for,
				&Parser::parse_while, &Parser::parse_return, &Parser::parse_use_stmt,
				&Parser::parse_variable_declaration, &Parser::parse_expr_statement);
		if (!result)
			return std::nullopt;
		ws();
		return result;
	}

	std::vector<ast::TypeArgument> parse_type_arguments_declaration() {
		std::vector<ast::TypeArgument> type_arguments{};
		if (try_consume("<")) {
			while (true) {
				auto loc_start = get_loc();
				std::string_view name{parse_name_str()};
				if (name.empty()) {
					ws();
					expect(">");
					break;
				}
				ws();
				if (try_consume("=")) {
					ws();
					auto type = parse_type();
					if (!type)
						error("Expected type after =");
					type_arguments.emplace_back(get_range(loc_start), name, std::make_unique<ast::TypeAst>(*type));
				} else {
					type_arguments.emplace_back(get_range(loc_start), name, std::nullopt);
				}
				try_consume(","); // optional trailing comma
				ws();
				if (try_consume(">"))
					break;
			}
			ws();
		}
		return type_arguments;
	}
	std::vector<ast::TypeAst> parse_type_arguments() {
		std::vector<ast::TypeAst> type_arguments{};
		if (try_consume("<")) {
			while (true) {
				ws();
				auto type = parse_type();
				if (!type)
					break;
				type_arguments.push_back(std::move(*type));
				ws();
				try_consume(","); // optional trailing comma
				ws();
			}
			ws();
			expect(">");
		}
		return type_arguments;
	}

	// toplevels

	std::optional<ast::top::Namespace> parse_namespace() {
		const auto loc_start = get_loc();
		if (!keyword("namespace"))
			return std::nullopt;
		must_ws();
		auto identifier = parse_identifier();
		if (!identifier)
			error("Expected identifier");
		ws();
		bool semicolon = try_consume(";");
		if (!semicolon)
			expect("{");
		ws();
		std::vector<ast::TopLevelAst> top_level_asts;
		while (true) {
			auto top_level_ast = parse_top_level();
			if (!top_level_ast)
				break;
			ws();
			top_level_asts.push_back(std::move(*top_level_ast));
		}
		ws();
		if (!semicolon) {
			expect("}");
			ws();
		}
		return std::make_optional(ast::top::Namespace(
				get_range(loc_start), std::move(*identifier), std::move(top_level_asts)));
	}
	std::optional<ast::top::Function> parse_function() {
		const auto loc_start = get_loc();
		bool is_external = keyword("extern");
		ws();
		if (!keyword("fun"))
			return std::nullopt;
		must_ws();
		auto identifier = parse_identifier();
		if (!identifier)
			error("Expected function name");
		ws();
		std::vector<ast::TypeArgument> type_arguments{};
		if (!is_external)
			type_arguments = parse_type_arguments_declaration();
		ws();
		expect("(");
		std::vector<ast::FunctionParameter> parameters{};
		while (true) {
			const auto fp_loc_start = get_loc();
			auto name = parse_name();
			if (!name)
				break;
			expect(":");
			ws();
			ast::FunctionParameter::Kind kind;
			if (keyword("own"))
				kind = ast::FunctionParameter::Kind::own;
			else if (keyword("out"))
				kind = ast::FunctionParameter::Kind::out;
			else
				kind = ast::FunctionParameter::Kind::in;
			ws();
			auto type = parse_type();
			if (!type)
				error("Expected parameter type");
			ws();
			parameters.emplace_back(get_range(fp_loc_start), *name, std::make_unique<ast::TypeAst>(std::move(*type)), kind);
			if (!try_consume(","))
				break;
			ws();
		}
		expect(")");
		ws();
		expect(":");
		ws();
		auto return_type = parse_type();
		if (!return_type)
			error("Expected return type");
		ws();
		if (is_external) {
			expect(";");
			return std::make_optional(ast::top::Function(
					get_range(loc_start), std::move(*identifier), std::move(type_arguments), std::move(parameters),
					std::make_unique<ast::TypeAst>(std::move(*return_type)),
					std::nullopt));
		}
		auto body = parse_block();
		if (!body)
			error("Expected function body block");
		return std::make_optional(ast::top::Function(
				get_range(loc_start), std::move(*identifier), std::move(type_arguments), std::move(parameters),
				std::make_unique<ast::TypeAst>(std::move(*return_type)),
				std::make_optional(std::make_unique<ast::StatementAst>(std::move(*body)))));
	}
	std::optional<ast::top::Struct> parse_struct() {
		const auto loc_start = get_loc();
		if (!keyword("struct"))
			return std::nullopt;
		must_ws();
		auto identifier = parse_identifier();
		ws();
		auto type_arguments = parse_type_arguments_declaration();
		expect("{");
		ws();
		std::vector<ast::top::Struct::Field> fields{};
		while (true) {
			auto name = parse_name_str();
			if (name.empty())
				break;
			expect(":");
			ws();
			auto type = parse_type();
			if (!type)
				error("Expected field type");
			fields.emplace_back(name, *type);
			ws();
			try_consume(","); // optional trailing comma
			ws();
			if (try_consume("}"))
				break;
		}
		ws();
		return std::make_optional(ast::top::Struct(get_range(loc_start), std::move(*identifier),
												   std::move(type_arguments),
												   std::move(fields)));
	}

	std::optional<ast::top::Use> parse_use() {
		const auto loc_start = get_loc();
		if (!keyword("use"))
			return std::nullopt;
		must_ws();
		auto alias = parse_identifier();
		if (!alias)
			error("Expected identifier after `use`");
		ws();
		if (!try_consume("=")) {
			expect(";");
			const auto as = alias->final;
			return ast::top::Use{get_range(loc_start), std::move(*alias), std::move(as)};
		}
		if (!alias->parts.empty())
			error("No use `as` with parts");
		ws();
		auto identifier = parse_identifier();
		if (!identifier)
			error("Expected identifier after `use` =");
		return ast::top::Use{get_range(loc_start), (std::move(*identifier)), std::move(alias->final)};
	}
	std::optional<ast::stmt::Use> parse_use_stmt() {
		if (auto use = parse_use())
			return ast::stmt::Use{use->location, use->identifier, use->as};
		return std::nullopt;
	}

	std::optional<ast::TopLevelAst> parse_top_level() {
		return try_<ast::TopLevelAst>(&Parser::parse_namespace,
									  &Parser::parse_function,
									  &Parser::parse_struct,
									  &Parser::parse_use);
	}

	// program

	ast::Program parse_program() {
		const auto loc_start = get_loc();
		std::vector<ast::TopLevelAst> top_level_asts;
		while (true) {
			ws();
			auto top_level_ast = parse_top_level();
			if (!top_level_ast)
				break;
			top_level_asts.push_back(std::move(*top_level_ast));
			ws();
		}
		if (!eof())
			error("Expected EOF");
		return ast::Program(get_range(loc_start), std::move(top_level_asts));
	}
};
} // namespace

namespace parser {
ast::Program parse_program(const std::string_view filename,
						   const std::string_view content) {
	Parser parser(filename, content);
	return parser.parse_program();
}
} // namespace parser