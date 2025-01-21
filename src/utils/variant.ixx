module;
#include <compare>
#include <variant>
export module utils.variant;
import utils.type;

export namespace utils {
template<class... Ts>
struct variant : std::variant<Ts...> {
	using Ts0 = index_pack<0, Ts...>;
	using base = std::variant<Ts...>;
	using base::base;
	using base::operator=;
	template<class... Ots>
		requires(sizeof...(Ots) > 0 && pack_contains<std::monostate, Ts...> &&
				 (pack_contains<Ots, Ts...> && ...))
	constexpr variant(variant<Ots...> &&other) noexcept : base{std::monostate{}} {
		other.visit([&]<typename T>(T &&v) {
			this->template emplace<std::decay_t<T>>(
					std::forward<T>(v));
		});
	}
	template<class... Ots>
		requires(sizeof...(Ots) > 0 && pack_contains<std::monostate, Ts...> &&
				 (pack_contains<Ots, Ts...> && ...))
	constexpr variant(const variant<Ots...> &other) noexcept
		: base{std::monostate{}} {
		other.visit([&]<typename T>(T &&v) {
			this->template emplace<std::decay_t<T>>(v);
		});
	}
	constexpr variant(const variant &other) noexcept = default;
	constexpr variant(variant &&other) noexcept = default;
	constexpr variant &operator=(const variant &other) noexcept = default;
	constexpr variant &operator=(variant &&other) noexcept = default;

	~variant() = default;

	static consteval std::size_t size() { return sizeof...(Ts); }
	template<class T>
	static consteval std::size_t index_of() {
		return index_in_pack<T, Ts...>;
	};
	template<class T>
	static consteval bool contains() {
		return pack_contains<T, Ts...>;
	}
	template<class T>
	constexpr const T &get() const {
		return std::get<T>(*this);
	}
	template<class T>
	constexpr T &get() { return std::get<T>(*this); }
	template<class T>
	[[nodiscard]] constexpr bool is() const {
		return std::holds_alternative<T>(*this);
	}
	constexpr bool is(std::size_t index) { return this->index() == index; }
	template<class... Fs>
	constexpr auto visit(Fs... fs) {
		return std::visit(overloaded{fs...}, *this);
	}
	template<class... Fs>
	constexpr auto visit(Fs... fs) const {
		return std::visit(overloaded{fs...}, *this);
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
	bool operator==(const variant<Other...> &other) const {
		return visit([&]<class T0>(T0 &&v) {
			if constexpr (pack_contains<std::decay_t<T0>, Other...>)
				return other.template is<std::decay_t<T0>>() &&
					   other.template get<std::decay_t<T0>>() == v;
			else
				return false;
		});
	}

	std::strong_ordering operator<=>(const variant<Ts...> &other) const {
		return visit([&]<class T0>(T0 &&v) {
			if (other.is<std::decay_t<T0>>())
				return other.get<std::decay_t<T0>>() <=> v;
			return other.index() <=> this->index();
		});
	}

	template<class Member>
		requires(contains<Member>())
	bool operator==(const Member &other) const {
		return this->is<Member>() && this->get<Member>() == other;
	}
};
} // namespace utils