module;
#include <array>
#include <compare>
#include <memory>
#include <string>
#include <utility>
#include <vector>
export module type;

import utils;
import naming;

namespace type {
export struct Type;
export struct Array final {
	std::unique_ptr<Type> member;
	std::uintmax_t size;
	Array(std::unique_ptr<Type> member, std::uintmax_t size);
	Array(const Array &);
	Array &operator=(const Array &);
	std::strong_ordering operator<=>(const Array &other) const;
	bool operator==(const Array &other) const;
	[[nodiscard]] std::string mangle() const;
};
export struct Function {
	std::vector<Type> parameters;
	std::unique_ptr<Type> return_type;
	explicit Function(std::vector<Type> parameters,
					  std::unique_ptr<Type> return_type);
	explicit Function(std::vector<Type> parameters, Type return_type);
	Function(const Function &);
	Function &operator=(const Function &);
	std::strong_ordering operator<=>(const Function &) const;
	bool operator==(const Function &other) const;
	[[nodiscard]] std::string mangle() const;
};
export struct NamedStruct {
	naming::FullName name;
	std::vector<Type> template_args;
	std::vector<std::pair<std::string_view, Type>> members;
	explicit NamedStruct(naming::FullName name, std::vector<Type> template_args,
						 std::vector<std::pair<std::string_view, Type>> members);
	std::strong_ordering operator<=>(const NamedStruct &other) const;
	bool operator==(const NamedStruct &other) const;
	[[nodiscard]] std::string mangle() const;
};
export struct Pointer final {
	std::unique_ptr<Type> pointed;
	explicit Pointer(std::unique_ptr<Type> pointed);
	Pointer(const Pointer &);
	Pointer &operator=(const Pointer &);
	std::strong_ordering operator<=>(const Pointer &other) const;
	bool operator==(const Pointer &other) const;
	[[nodiscard]] std::string mangle() const;
};
export struct Primitive final {
	constexpr static uint8_t numeric_bit = 0b1'0'0'000;
	constexpr static uint8_t floating_bit = 0b0'1'0'000;
	constexpr static uint8_t signed_bit = 0b0'0'1'000;
	constexpr static uint8_t byte_size_bits = 0b0'0'0'111;
	enum p_type : uint8_t {
		boolean = 0b0'0'0'000,
		uint8 = 0b1'0'0'000,
		int8 = 0b1'0'1'000,
		uint16 = 0b1'0'0'001,
		int16 = 0b1'0'1'001,
		uint32 = 0b1'0'0'010,
		int32 = 0b1'0'1'010,
		uint64 = 0b1'0'0'011,
		int64 = 0b1'0'1'011,
		uint128 = 0b1'0'0'100,
		int128 = 0b1'0'1'100,
		half = 0b1'1'1'001,
		float_ = 0b1'1'1'010,
		double_ = 0b1'1'1'011,
	};
	[[nodiscard]] constexpr bool is_bool() const { return type == boolean; }
	[[nodiscard]] constexpr bool is_numeric() const { return type & numeric_bit; }
	[[nodiscard]] constexpr bool is_floating() const { return type & floating_bit; }
	[[nodiscard]] constexpr bool is_integral() const { return !is_floating(); }
	[[nodiscard]] constexpr bool is_signed() const { return type & signed_bit; }
	[[nodiscard]] constexpr Primitive to_signed() const {
		return Primitive(static_cast<p_type>(type | signed_bit));
	}
	[[nodiscard]] constexpr bool is_unsigned() const { return !is_signed(); }
	[[nodiscard]] constexpr int size() const { return 1 << (type & byte_size_bits); }
	[[nodiscard]] constexpr int bits() const { return size() * 8; }
	p_type type;
	constexpr explicit Primitive(const p_type type) : type(type) {}
	constexpr std::strong_ordering operator<=>(const Primitive &other) const {
		return type <=> other.type;
	}
	constexpr bool operator==(const Primitive &other) const {
		return type == other.type;
	}
	[[nodiscard]] constexpr std::string_view name() const {
		switch (type) {
			case boolean:
				return "bool";
			case uint8:
				return "uint8";
			case int8:
				return "int8";
			case uint16:
				return "uint16";
			case int16:
				return "int16";
			case uint32:
				return "uint32";
			case int32:
				return "int32";
			case uint64:
				return "uint64";
			case int64:
				return "int64";
			case uint128:
				return "uint128";
			case int128:
				return "int128";
			case half:
				return "half";
			case float_:
				return "float";
			case double_:
				return "double";
		}
		std::unreachable();
	}
	[[nodiscard]] constexpr std::string_view mangle_name() const {
		switch (type) {
			case boolean:
				return "bl";
			case uint8:
				return "u1";
			case int8:
				return "i1";
			case uint16:
				return "u2";
			case int16:
				return "i2";
			case uint32:
				return "u3";
			case int32:
				return "i3";
			case uint64:
				return "u4";
			case int64:
				return "i4";
			case uint128:
				return "u5";
			case int128:
				return "i5";
			case half:
				return "f2";
			case float_:
				return "f3";
			case double_:
				return "f4";
		}
		std::unreachable();
	}
	[[nodiscard]] std::string mangle() const;
};
/**
 * In memory, basically a
 * struct S {
 *	T* start,
 *	T* end
 * };
 */
export struct Slice final {
	std::unique_ptr<Type> sliced;
	explicit Slice(std::unique_ptr<Type> sliced);
	Slice(const Slice &);
	Slice &operator=(const Slice &);
	std::strong_ordering operator<=>(const Slice &other) const;
	bool operator==(const Slice &other) const;
	[[nodiscard]] std::string mangle() const;
};
using type_variant = utils::variant<Array, Function, NamedStruct,
									Pointer, Primitive, Slice>;
export struct Type : type_variant {
	using base = type_variant;
	bool is_const{}, is_ref{};

	template<class Member>
		requires(type_variant::contains<Member>())
	constexpr Type(Member &&member, const bool is_const = false, const bool is_ref = false)
		: base(std::move(member)), is_const{is_const}, is_ref{is_ref} {}
	template<class Member>
		requires(type_variant::contains<Member>())
	constexpr Type(const Member &member, const bool is_const = false,
				   const bool is_ref = false)
		: base(member), is_const{is_const}, is_ref{is_ref} {}
	constexpr Type(const Type &other) = default;
	constexpr Type(Type &&other) = default;
	constexpr Type &operator=(const Type &other) = default;
	constexpr bool operator==(const Type &other) const {
		return is_const == other.is_const && is_ref == other.is_ref &&
			   base::operator==(other);
	}
	constexpr std::strong_ordering operator<=>(const Type &other) const {
		if (const auto cmp = is_const <=> other.is_const; cmp != 0)
			return cmp;
		if (const auto cmp = is_ref <=> other.is_ref; cmp != 0)
			return cmp;
		return base::operator<=>(other);
	}
	[[nodiscard]] std::string mangle() const;
};
export inline constexpr Primitive boolean{Primitive::p_type::boolean};
export inline constexpr Primitive uint8{Primitive::p_type::uint8};
export inline constexpr Primitive int8{Primitive::p_type::int8};
export inline constexpr Primitive uint16{Primitive::p_type::uint16};
export inline constexpr Primitive int16{Primitive::p_type::int16};
export inline constexpr Primitive uint32{Primitive::p_type::uint32};
export inline constexpr Primitive int32{Primitive::p_type::int32};
export inline constexpr Primitive uint64{Primitive::p_type::uint64};
export inline constexpr Primitive int64{Primitive::p_type::int64};
export inline constexpr Primitive uint128{Primitive::p_type::uint128};
export inline constexpr Primitive int128{Primitive::p_type::int128};
export inline constexpr Primitive half{Primitive::p_type::half};
export inline constexpr Primitive float_{Primitive::p_type::float_};
export inline constexpr Primitive double_{Primitive::p_type::double_};
export inline constexpr Type t_boolean{Primitive{Primitive::boolean}};
export inline constexpr Type t_uint8{Primitive{Primitive::uint8}};
export inline constexpr Type t_int8{Primitive{Primitive::int8}};
export inline constexpr Type t_uint16{Primitive{Primitive::uint16}};
export inline constexpr Type t_int16{Primitive{Primitive::int16}};
export inline constexpr Type t_uint32{Primitive{Primitive::uint32}};
export inline constexpr Type t_int32{Primitive{Primitive::int32}};
export inline constexpr Type t_uint64{Primitive{Primitive::uint64}};
export inline constexpr Type t_int64{Primitive{Primitive::int64}};
export inline constexpr Type t_uint128{Primitive{Primitive::uint128}};
export inline constexpr Type t_int128{Primitive{Primitive::int128}};
export inline constexpr Type t_half{Primitive{Primitive::half}};
export inline constexpr Type t_float{Primitive{Primitive::float_}};
export inline constexpr Type t_double{Primitive{Primitive::double_}};
export inline constexpr std::array unsigneds{t_uint8, t_uint16, t_uint32, t_uint64, t_uint128};
export inline constexpr std::array signeds{t_int8, t_int16, t_int32, t_int64, t_int128};
export inline constexpr std::array integers{t_uint8, t_int8, t_uint16, t_int16, t_uint32, t_int32, t_uint64, t_int64, t_uint128, t_int128};
export inline constexpr std::array floats{t_half, t_float, t_double};
export inline constexpr std::array numeric{t_uint8, t_int8, t_uint16, t_int16, t_uint32, t_int32, t_uint64, t_int64, t_uint128, t_int128, t_half, t_float, t_double};
} // namespace type