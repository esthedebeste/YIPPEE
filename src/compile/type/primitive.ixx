module;
#include <memory>
#include <string_view>
export module type.primitive;

import utils;
import type.decl;

export namespace type {
struct Primitive final {
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
};
extern Primitive boolean;
extern Primitive uint8;
extern Primitive int8;
extern Primitive uint16;
extern Primitive int16;
extern Primitive uint32;
extern Primitive int32;
extern Primitive uint64;
extern Primitive int64;
extern Primitive uint128;
extern Primitive int128;
extern Primitive half;
extern Primitive float_;
extern Primitive double_;
} // namespace type

module :private;
namespace type {
Primitive boolean{Primitive::p_type::boolean};
Primitive uint8{Primitive::p_type::uint8};
Primitive int8{Primitive::p_type::int8};
Primitive uint16{Primitive::p_type::uint16};
Primitive int16{Primitive::p_type::int16};
Primitive uint32{Primitive::p_type::uint32};
Primitive int32{Primitive::p_type::int32};
Primitive uint64{Primitive::p_type::uint64};
Primitive int64{Primitive::p_type::int64};
Primitive uint128{Primitive::p_type::uint128};
Primitive int128{Primitive::p_type::int128};
Primitive half{Primitive::p_type::half};
Primitive float_{Primitive::p_type::float_};
Primitive double_{Primitive::p_type::double_};
} // namespace type