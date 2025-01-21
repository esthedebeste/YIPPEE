module;
#include <compare>
#include <memory>
#include <utility>
#include <vector>
#include <string>
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
	std::string mangle() const;
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
	std::string mangle() const;
};
export struct NamedStruct {
	naming::FullName name;
	std::vector<Type> template_args;
	std::vector<std::pair<std::string, Type>> members;
	explicit NamedStruct(naming::FullName name, std::vector<Type> template_args,
						 std::vector<std::pair<std::string, Type>> members);
	std::strong_ordering operator<=>(const NamedStruct &other) const;
	bool operator==(const NamedStruct &other) const;
	std::string mangle() const;
};
export struct Pointer final {
	std::unique_ptr<Type> pointed;
	explicit Pointer(std::unique_ptr<Type> pointed);
	Pointer(const Pointer &);
	Pointer &operator=(const Pointer &);
	std::strong_ordering operator<=>(const Pointer &other) const;
	bool operator==(const Pointer &other) const;
	std::string mangle() const;
};
export struct Primitive final {
	constexpr static uint8_t numeric_bit = 0b1'0'0'000;
	constexpr static uint8_t floating_bit = 0b0'1'0'000;
	constexpr static uint8_t signed_bit = 0b0'0'1'000;
	constexpr static uint8_t byte_size_bit = 0b0'0'0'111;
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
	[[nodiscard]] constexpr int size() const { return 1 << (type & byte_size_bit); }
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
	std::string mangle() const;
};
using type_variant = utils::variant<type::Array, type::Function, type::NamedStruct,
									type::Pointer, type::Primitive>;
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
	std::string mangle() const;
};
export extern const Primitive boolean;
export extern const Primitive uint8;
export extern const Primitive int8;
export extern const Primitive uint16;
export extern const Primitive int16;
export extern const Primitive uint32;
export extern const Primitive int32;
export extern const Primitive uint64;
export extern const Primitive int64;
export extern const Primitive uint128;
export extern const Primitive int128;
export extern const Primitive half;
export extern const Primitive float_;
export extern const Primitive double_;
export extern const Type t_boolean;
export extern const Type t_uint8;
export extern const Type t_int8;
export extern const Type t_uint16;
export extern const Type t_int16;
export extern const Type t_uint32;
export extern const Type t_int32;
export extern const Type t_uint64;
export extern const Type t_int64;
export extern const Type t_uint128;
export extern const Type t_int128;
export extern const Type t_half;
export extern const Type t_float;
export extern const Type t_double;
} // namespace type