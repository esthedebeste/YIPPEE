module;
#include <memory>
#include <optional>
#include <tuple>
#include <vector>
export module utils.type;

namespace {
template<template<class...> class T, class Uts>
struct UnwrapInstantiate;
template<template<class...> class T, template<class...> class U, class... Ts>
struct UnwrapInstantiate<T, U<Ts...>> {
	using type = T<Ts...>;
};

template<template<class> class Mapper, class Mts>
struct UnwrapMap;
template<template<class> class Mapper, template<class...> class U,
		 class... Ts>
struct UnwrapMap<Mapper, U<Ts...>> {
	using type = U<typename Mapper<Ts>::type...>;
};
template<class T, class... Ts>
struct index_of;
template<class T, class... Ts>
struct index_of<T, T, Ts...> : std::integral_constant<std::size_t, 0> {};
template<class T, class U, class... Ts>
struct index_of<T, U, Ts...>
	: std::integral_constant<std::size_t, 1 + index_of<T, Ts...>::value> {};
template<class T, class... Ts>
struct contains;
template<class T>
struct contains<T> : std::false_type {};
template<class T, class... Ts>
struct contains<T, T, Ts...> : std::true_type {};
template<class T, class U, class... Ts>
struct contains<T, U, Ts...> : contains<T, Ts...> {};

template<std::size_t I, class T, class... Ts>
struct get_type_at_index {
	using type = typename get_type_at_index<I - 1, Ts...>::type;
};
template<class T, class... Ts>
struct get_type_at_index<0, T, Ts...> {
	using type = T;
};
} // namespace

export namespace utils {
template<class To, class From>
std::unique_ptr<To> dynamic_pointer_cast(std::unique_ptr<From> &&r) {
	(void) dynamic_cast<To *>(static_cast<From *>(0));
	To *p = dynamic_cast<To *>(r.get());
	if (p)
		r.release();
	return std::unique_ptr<To>(p);
}

template<class T>
T clone(const T &t) { return t; }
template<class T>
std::unique_ptr<T> clone(const std::unique_ptr<T> &p) {
	if (!p)
		return nullptr;
	return std::make_unique<T>(*p);
}

template<class T>
std::optional<T> clone(const std::optional<T> &t) {
	if (!t)
		return std::nullopt;
	return std::make_optional(clone(*t));
}

template<class T>
std::vector<T> clone(const std::vector<T> &t) {
	std::vector<T> res{};
	res.reserve(t.size());
	for (const auto &v : t)
		res.push_back(clone(v));
	return res;
}

template<template<class...> class T, class U>
using unwrap_instantiate = typename UnwrapInstantiate<T, U>::type;

template<template<class...> class T, class... Us>
using unwrap_concat_instantiate =
		typename UnwrapInstantiate<T, decltype(std::tuple_cat(
											  std::declval<typename UnwrapInstantiate<
													  std::tuple, Us>::type>()...))>::type;

template<template<class> class Mapper, class Mts>
using unwrap_map = typename UnwrapMap<Mapper, Mts>::type;

template<class T, class... Ts>
constexpr std::size_t index_in_pack = index_of<T, Ts...>::value;

template<std::size_t I, class... Ts>
using index_pack = typename get_type_at_index<I, Ts...>::type;

template<class T, class... Ts>
constexpr bool pack_contains = contains<T, Ts...>::value;

template<class... Ts>
struct overloaded : Ts... {
	using Ts::operator()...;
};

template<class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

template<size_t From, size_t Until>
void integer_range_loop(auto &&lambda) {
	if constexpr (From < Until) {
		lambda.template operator()<From>();
		integer_range_loop<From + 1, Until>(std::forward<decltype(lambda)>(lambda));
	}
}
} // namespace utils