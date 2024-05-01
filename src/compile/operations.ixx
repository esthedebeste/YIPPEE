module;
export module operations;
import type;
import operators;

export namespace operation {
struct Binary {
	type::Type left, right;
	operators::binary op;
};
struct Comparison {
	type::Type left, right;
	operators::comparison op;
};
struct Unary {
	type::Type operand;
	operators::unary op;
};
} // namespace operation