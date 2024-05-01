module;
#include <type_traits>
#include <utility>
export module utils.type_holder;
import utils.type;

export namespace utils {
template<class... Ts>
struct type_holder {
	std::size_t index;

	template<class T>
	static consteval std::size_t index_of() {
		return index_in_pack<T, Ts...>::value;
	};
	type_holder(const std::size_t index) : index(index) {}
	template<class T>
	type_holder() : index(index_of<T, Ts...>) {}
	template<class T>
	void set() { index = index_of<T, Ts...>; }
	static consteval std::size_t size() { return sizeof...(Ts); }
	template<class T>
	constexpr const T &get() const {
		return std::get<T>(*this);
	}
	template<class T>
	constexpr T &get() { return std::get<T>(*this); }
	template<class T>
	constexpr bool is() const {
		return this->index() == index_of<T>();
	}
	constexpr bool is(std::size_t index) { return this->index() == index; }
	template<class... Fs>
	constexpr auto visit(Fs... fs) {
		auto to_call = overloaded{fs...};
		(
				[&] {
					using T = Ts;
					if (is<T>())
						to_call.template operator()<T>();
				}(),
				...);
	}
	template<class... Fs>
	constexpr auto visit(Fs... fs) const {
		auto to_call = overloaded{fs...};
		(
				[&] {
					using T = Ts;
					if (is<T>)
						to_call.template operator()<T>();
				}(),
				...);
	}
	template<std::size_t Start = 0, std::size_t End = size()>
	static void for_each(auto fn) {
		(
				[&]() {
					constexpr auto index = index_of<Ts>();
					if constexpr (index >= Start && index < End)
						fn.template operator()<Ts>(index_of<Ts>());
				}(),
				...);
	}
	template<class Start, class EndInclusive>
	static void for_each(auto fn) {
		for_each<index_of<Start>(), index_of<EndInclusive>() + 1>(fn);
	}
	static void for_each(auto fn) { (fn.template operator()<Ts>(), ...); }

	template<class... Other>
	bool operator==(const type_holder<Other...> &other) const {
		return visit([&]<class T0>() {
			using T = std::decay_t<T0>;
			return other.template is<T>();
		});
	}

	bool operator<=>(const type_holder &other) const {
		return other.index() <=> this->index();
	}
};
} // namespace utils