![yippee](./assets/YIPPEE.webp)

programming language!!!! yippee!

> [!NOTE]  
> YIPPEE is in a very early phase and not yet open for contributions.  
> Talk to me if you would like to help the project (contact information on my [website](https://esthe.win/))

TODO (highest to lowest priority):

- [x] (☆) `if`, `while`, `for` statements. (parser, ast, backend)
- [x] (☆) `create` with struct field names (parser, ast, backend)
- [x] (☆) struct default template argument values (ast, backend)
- [x] (☆☆) array types (parser, ast, backend)
	- [x] (☆) subscript operator (backend)
	- [x] (☆) array literals (parser, ast, backend)
- [x] (☆☆☆) generic functions (parser, ast, backend)
	- [x] (☆) weak linkage (backend)
- [ ] (☆) member function call (`fun double(x: int32)` callable as `x.double()`) (parser, ast, backend)
- [ ] (☆☆) operator overloading (parser, ast, backend)
- [ ] (☆☆☆) infer template arguments (backend?)
- [ ] (☆☆☆☆☆) `import` statement loads a file as type declarations. (parser, ast, backend)
	- [ ] (☆☆☆) get started on a standard library (...?, stdlib)
- [ ] (☆☆☆) `possible(...)` expression (parser, ast, backend)
	- [ ] (☆☆☆☆) template argument constraints (parser, ast, backend)
- [ ] (☆☆☆☆) compile directly to exe (backend)
- [ ] (☆☆☆) `const` types (parser, ast, backend)
- [ ] (☆☆☆) some form of reference types (`out`/`in`?) (parser, ast, backend)
- [ ] (☆☆☆☆☆☆☆☆☆☆) RAII, constructors, destructors, and the such (parser, ast, backend)
- [ ] (☆) allow reordering of fields in `create` (C++ doesn't support this either so I'm letting this one sit) (backend)
- [ ] (☆☆☆) infer template arguments in `create` (backend?)

not sure how to design:

- [ ] (☆☆☆☆) os checking (best design would be a `if constexpr` analogue?) (parser?, ast?, backend)
- [ ] (☆) get array length (array.size, std::length(array), ???) (backend? stdlib?)
- [ ] (☆) character literals. C `uint8`, go `rune`, rust max-4-byte UTF-8 `char`, ???  (parser, ast, backend, stdlib?)
- [ ] (☆) string literals. automatically compile to a `std::something`? should YIPPEE be a UTF-8, UTF-16, or UTF-32 language? [C++-style user literals?](https://en.cppreference.com/w/cpp/language/user_literal)
  (parser, ast, backend,
  stdlib?)