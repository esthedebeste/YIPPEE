module;
#include <compare>
#include <memory>
export module type;
export import type.array;
export import type.compound;
export import type.pointer;
export import type.primitive;
export import type.function;
export import type.decl;
import utils;
import naming;

using type_variant = utils::variant<type::Array, type::Function, type::NamedStruct,
									type::Pointer, type::Primitive>;
export namespace type {
struct Type : type_variant {
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
		if (auto cmp = is_const <=> other.is_const; cmp != 0)
			return cmp;
		if (auto cmp = is_ref <=> other.is_ref; cmp != 0)
			return cmp;
		return base::operator<=>(other);
	}
};
extern Type t_boolean;
extern Type t_uint8;
extern Type t_int8;
extern Type t_uint16;
extern Type t_int16;
extern Type t_uint32;
extern Type t_int32;
extern Type t_uint64;
extern Type t_int64;
extern Type t_uint128;
extern Type t_int128;
extern Type t_half;
extern Type t_float;
extern Type t_double;
} // namespace type