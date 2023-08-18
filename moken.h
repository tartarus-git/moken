#pragma once

#include <cstddef>
#include <cstdint>
#include <utility>
#include <tuple>
#include <concepts>
#include <type_traits>
// TODO: Check to make sure we need all these headers

namespace moken {

	inline constexpr char marker_character = '@';

	void report_error(const char *message) noexcept;

	template <typename U, typename V>
	class are_types_same {
	public:
		static constexpr bool value = false;
		consteval operator bool() { return value; }
	};
	template <typename T>
	class are_types_same<T, T> {
	public:
		static constexpr bool value = true;
		consteval operator bool() { return value; }
	};

	template <typename U, typename V>
	inline constexpr bool are_types_same_v = are_types_same<U, V>::value;

	template <typename U, typename V>
	concept same_as_c = (are_types_same_v<U, V>);

	template <typename element_t, size_t array_length>
	class array_container_t;

	template <typename T>
	class is_array_container_t {
	public:
		static constexpr bool value = false;
		consteval operator bool() { return value; }
	};
	template <typename element_t, size_t array_length>
	class is_array_container_t<array_container_t<element_t, array_length>> {
	public:
		static constexpr bool value = true;
		consteval operator bool() { return value; }
	};

	template <typename T>
	inline constexpr bool is_array_container_t_v = is_array_container_t<T>::value;

	template <typename T>
	concept array_container_t_c = is_array_container_t_v<T>;

	template <typename T>
	concept convertible_to_compile_time_array_c = requires(T instance, decltype(*(instance.begin())) element_instance) {
		{ *(instance.end())  } -> same_as_c<decltype(element_instance)>;
		{ element_instance++ } -> same_as_c<T>;
		{ ++element_instance } -> same_as_c<T&>;
		T::length;
		requires (!is_array_container_t_v<T>);
		// NOTE: Converting from source containers is well and good, but if the source container
		// is another array_container_t, then we oughta use the default copy constructor, because I assume that's
		// potentially much faster at compile-time.
	};

	template <convertible_to_compile_time_array_c source_container_t>
	array_container_t(const source_container_t &source_container)
	-> array_container_t<decltype(*(source_container.begin())), source_container_t::length>;

	template <typename element_t, size_t array_length>
	class array_container_t {
	public:
		using type = element_t;
		static constexpr size_t length = array_length;

		element_t data[length];

		consteval array_container_t(const element_t (&source_array)[array_length]) {
			for (size_t i = 0; i < length; i++) { data[i] = source_array[i]; }
		}

		// NOTE: As said above, this doesn't double as the copy constructor, we use the default one.
		template <convertible_to_compile_time_array_c source_container_t>
		consteval array_container_t(const source_container_t &source_container) {
			size_t i = 0;
			for (element_t element : source_container) { data[i++] = element; }
		}

		// NOTE: These two aren't used as far as I know, I would like to use them, but a compiler bug seems to be preventing me from doing so.
		// I use a work-around, but I'm keeping these two functions in case things change or I'm able to use them in a different spot or something.
		consteval element_t& operator[](size_t index) { return data[index]; }
		consteval const element_t& operator[](size_t index) const { return data[index]; }

		// NOTE: We don't use the following, but they're here to round out the class.
		using iterator_t = element_t*;
		using const_iterator_t = const element_t*;
		// TODO: Is this compatible with that iterator data struct in the standard library?

		consteval iterator_t begin() { return &data; }
		consteval const_iterator_t begin() const { return &data; }

		consteval iterator_t end() { return &data + length; }
		consteval const_iterator_t end() const { return &data + length; }
	};

	template <std::unsigned_integral T>
	concept compatible_array_container_element_t_c = (sizeof(T) <= 2);

	template <array_container_t_c array_container_type>
	concept compatible_array_container_t_c = compatible_array_container_element_t_c<typename array_container_type::type>;

	template <std::integral T>
	concept sufficiently_compatible_array_container_element_t_c = (sizeof(T) <= 2);

	template <array_container_t_c array_container_type>
	concept sufficiently_compatible_array_container_t_c = sufficiently_compatible_array_container_element_t_c<typename array_container_type::type>;

	template <auto array_container> requires sufficiently_compatible_array_container_t_c<decltype(array_container)>
	consteval auto convert_sufficiently_compatible_array_container_to_compatible_array_container() {
		array_container_t<std::make_unsigned_t<typename decltype(array_container)::type>, decltype(array_container)::length> result;
		for (size_t i = 0; i < decltype(result)::length; i++) {
			// TODO: If the compiler doesn't spit out a bug here, change the above note somewhere about not using the [] operators.
			result[i] = array_container[i];
		}
		return result;
	}

	enum class spec_type_t : bool {
		EXTRA_NULL,
		NO_EXTRA_NULL
	};

	template <compatible_array_container_t_c specification_container, spec_type_t spec_type = spec_type_t::EXTRA_NULL>
	consteval size_t calculate_max_table_length() {
		using spec_element_t = typename decltype(specification_container)::type;
		// NOTE: The following line of code is a work-around for what seems to be a compiler bug.
		// I can't write specification_container[i], clang keeps complaining that i is non-const and can't be used in a constant expression,
		// even though this should be an exception since we're using it from inside
		// a consteval function. I think the compiler must somehow forget the consteval-ness along the way and I think the error is a bug.
		// Further proof is that with the following line (which just establishes a reference called "specification" to the inner array
		// in specification_container), everything works exactly as intended.
		// TODO: Something's fishy here, bug report this.
		const spec_element_t (&specification)[decltype(specification_container)::length] = specification_container.data;
		constexpr size_t spec_length = (
						spec_type == NO_EXTRA_NULL
						? decltype(specification_container)::length
						: decltype(specification_container)::length - 1
					       );

		// NOTE: The return 0 is necessary here to allow the constexpr if statement to actually works it's magic.
		// Or else everything else will get instantiated anyway I suppose.
		if constexpr (spec_length == 0) { report_error("moken spec syntax error: specification cannot be empty"); return 0; }

		size_t i = 0;
		size_t result = 0;
		bool is_inside_bracket_expression = false;

		// NOTE: I wanted to make this consteval instead of constexpr, but for some reason (which smells strongly of a compiler bug, TODO: REPORT!!!!)
		// consteval doesn't work, even though both versions are executed at compile-time in this case.
		auto func_implementation = [&i, &result, &is_inside_bracket_expression](size_t nesting_depth, const auto& self) constexpr -> void {
			for (; i < spec_length; i++) {
				spec_element_t character = specification[i];
				switch (character) {

				case '|':
					if (is_inside_bracket_expression) { break; }
					if (i == 0) { report_error(R"(moken spec syntax error: alternation ("|") must be preceeded by something)"); }
					if (i == spec_length - 1) { report_error(R"(moken spec syntax error: alternation ("|") must be succeeded by something)"); }
					switch (specification[i - 1]) {
					case '|': report_error(R"(moken spec syntax error: two alternations ("|") cannot be separated by nothing)");
					case '(': report_error(R"(moken spec syntax error: alternation ("|") cannot be preceeded by an opening parenthesis ("("))");
					}
					break;

				case '*':
					if (is_inside_bracket_expression) { break; }
					if (i == 0) { report_error(R"(moken spec syntax error: kleene closure ("*") must be preceeded by something)"); }
					switch (specification[i - 1]) {
					case '|': report_error(R"(moken spec syntax error: kleene closure ("*") cannot be preceeded by an alternation ("|"))");
					case '(': report_error(R"(moken spec syntax error: kleene closure ("*") cannot be preceeded by an opening parenthesis ("("))");
					}
					break;

				case '[':
					if (is_inside_bracket_expression) { report_error(R"(moken spec syntax error: bracket expressions ("[...]") cannot be nested)"); }
					is_inside_bracket_expression = true;
					break;

				case ']':
					if (!is_inside_bracket_expression) { report_error(R"(moken spec syntax error: closing square bracket ("]") is invalid without corresponding opening square bracket ("["))"); }
					if (specification[i - 1] == '[') { report_error(R"(moken spec syntax error: bracket expression ("[...]") cannot be empty)"); }
					is_inside_bracket_expression = false;
					result++;
					break;

				case '(':
					if (is_inside_bracket_expression) { break; }
					if (i == spec_length - 1) { report_error(R"(moken spec syntax error: opening parenthesis ("(") must be succeeded by something)"); }
					i++;
					self(nesting_depth + 1, self);
					break;

				case ')':
					if (is_inside_bracket_expression) { break; }
					if (nesting_depth == 0) { report_error(R"~(moken spec syntax error: closing parenthesis (")") is invalid without corresponding opening parenthesis ("("))~"); }
					if (specification[i - 1] == '(') { report_error(R"~(moken spec syntax error: subexpression ("(...)") cannot be empty)~"); }
					return;

				case marker_character:
					if (is_inside_bracket_expression) { break; }
					if (i == 0) { report_error(R"(moken spec syntax error: marker ("@") cannot be the first character in a specification)"); }
					switch (specification[i - 1]) {
					case '|': report_error(R"(moken spec syntax error: marker ("@") cannot be preceeded by alternation ("|"))");
					case '(': report_error(R"(moken spec syntax error: marker ("@") cannot be preceeded by opening parenthesis ("("))");
					case '@': report_error(R"(moken spec syntax error: marker ("@") cannot be preceeded by marker ("@"))");
					}
					break;

				case '\\':
					if (i == spec_length - 1) { report_error(R"(moken spec syntax error: escape character ("\") must be succeeded by something)"); }
					switch (specification[i + 1]) {
					case '|':	// NOTE: A couple of these don't need an escape character in front of them if you write them in a bracket expression, but we'll allow it anyway for simplicity.
					case '*':
					case '[':
					case ']':
					case '(':
					case ')':
					case '.':
					case marker_character:
					case '-':	// NOTE: This only needs to be escaped inside bracket expressions, but we'll allow it anywhere for simplicity.
					case '\\':
						break;
					default: report_error(R"(moken spec syntax error: escape character ("\") must be succeeded by metacharacter)");
					}
					result++;
					break;

				case '.':
					/* FALLTHROUGH */

				default:
					if (is_inside_bracket_expression) { break; }	// NOTE: Further syntax checking of bracket expressions is done below.
					result++;
					break;
				}
			}

			if (is_inside_bracket_expression) { report_error(R"(moken spec syntax error: invalid bracket expression ("[...]"), no closing square bracket ("]"))"); }
			if (nesting_depth != 0) { report_error(R"~(moken spec syntax error: invalid subexpression ("(...)"), no closing parenthesis (")"))~"); }

		};
		func_implementation(0, func_implementation);

		// NOTE: The following should never evaluate to true since any specification that's passed all the syntax checks up to here
		// should definitely produce at least one row, but in case I've made a mistake somewhere, this'll make sure we at least
		// get a good error message, for debugging.
		if (result == 0) { report_error("moken spec syntax error: specification must produce at least one DFA table row"); }

		return result;
	}

	enum class token_type_t : uint8_t {
		ALTERNATION,
		KLEENE_CLOSURE_BEGIN,
		KLEENE_CLOSURE_END,
		SUBEXPRESSION_BEGIN,
		SUBEXPRESSION_END,
		MARKER
		TABLE_ROW,
	};

	template <uint32_t table_width>
	struct token_t {
		static constexpr uint32_t row_length = table_width;

		token_type_t type;
		union {
			bool table_row[row_length];
			uint16_t new_termination_handler;
		};
	};

	template <compatible_array_container_t_c specification_container, spec_type_t spec_type = spec_type_t::EXTRA_NULL>
	consteval size_t calculate_token_array_length() {
		using spec_element_t = typename decltype(specification_container)::type;
		// NOTE: Same work-around as above.
		const spec_element_t (&specification)[decltype(specification_container)::length] = specification_container.data;
		constexpr size_t spec_length = (
						spec_type == spec_type_t::NO_EXTRA_NULL
						? decltype(specification_container)::length
						: decltype(specification_container)::length - 1
					       );

		size_t result = 0;

		for (size_t i = 0; i < spec_length; i++) {
			spec_element_t character = specification[i];

			switch (character) {
			case '|': result++; break;
			case '*': result++; break;
			case '(': result++; break;
			case ')': result++; break;
			case '[':
				result++;
				i++;
				for (; specification[i] != ']'; i++) { }
				break;
			case marker_character: break;
			case '\\': result++; i++; break;
			case '.':
			/* FALLTHROUGH */
			default: result++; break;
			}
		}

		return result;
	}

	template <typename specification_container_element_t> requires compatible_array_container_element_t_c<specification_container_element_t>;
	consteval uint32_t calculate_table_width() { return (uint32_t)(specification_container_element_t)-1 + 1; }

	template <compatible_array_container_t_c specification_container, spec_type_t spec_type = spec_type_t::EXTRA_NULL>
	consteval size_t parse_bracket_expression(size_t spec_index, bool (&table_row)[calculate_table_width<typename decltype(specification_container)::type>()]) {
		using spec_element_t = typename decltype(specification_container)::type;
		// NOTE: Same work-around as above.
		const spec_element_t (&specification)[decltype(specification_container)::length] = specification_container.data;
		constexpr size_t spec_length = (
						spec_type == spec_type_t::NO_EXTRA_NULL
						? decltype(specification_container)::length
						: decltype(specification_container)::length - 1
					       );

		auto add_character = [](bool (&table_row)[calculate_table_width<typename decltype(specification_container)::type>()], spec_element_t character) consteval {
			bool& row_bool = table_row[character];
			if (row_bool == true) { report_error(R"(moken spec syntax error: a character cannot be directly or indirectly specified more than once in a bracket expression ("[...]"))"); }
			row_bool = true;
		};

		// NOTE: We can be sure there's a corresponding end bracket and at least one character inside of the brackets because of previous syntax checks.
		spec_element_t previous_character = specification[spec_index + 1];
		bool previous_character_available_for_hyphen = true;
		if (previous_character == '-') { report_error(R"(moken spec syntax error: bracket expression ("[...]") cannot start with a hyphen ("-"))"); }
		add_character(table_row, specification[previous_character]);

		size_t i;

		for (i = spec_index + 2; specification[i] != ']'; i++) {
			spec_element_t character = specification[i];
			if (character == '-') {
				if (!previous_character_available_for_hyphen) { report_error(R"(moken spec syntax error: two hyphen expressions ("a-b") cannot intersect in bracket expression ("[...]"))"); }
				i++;
				character = specification[i];
				switch (character) {
				case ']': report_error(R"(moken spec syntax error: bracket expression ("[...]") cannot end with a hyphen ("-"))");
				case '-': report_error(R"(moken spec syntax error: consecutive hyphens ("-") are invalid in bracket expression ("[...]"))");
				case '\\': i++; character = specification[i]; break;
				}
				// NOTE: We just add all the characters whose codes are in between the codes of the two boundary characters,
				// and we also add the boundary characters themselves of course.
				// This is obviously dependant on the character encoding that your compiler is using, but I figure that
				// aspect could be useful in and of itself.
				// I've also never seen a C++ compiler stray from ASCII for char encoding, so anything other than ASCII is probably very unlikely.
				// ASCII takes care of us very well in this situation: [A-Z, a-z, 0-9] all work great and subsets of those obviously
				// work great as well.
				for (spec_element_t j = previous_character + 1; j <= character; j++) { add_character(table_row, specification[j]); }
				previous_character_available_for_hyphen = false;
				continue;
			}
			if (character == '\\') { i++; character = specification[i]; }
			add_character(table_row, character);
			previous_character = character;
			previous_character_available_for_hyphen = true;
		}

		return i;
	}

	template <compatible_array_container_type_c specification_container, spec_type_t spec_type = spec_type_t::EXTRA_NULL>
	consteval auto tokenize_specification() {
		using spec_element_t = typename decltype(specification_container)::type;
		// NOTE: Same work-around as above.
		const spec_element_t (&specification)[decltype(specification_container)::length] = specification_container.data;
		constexpr size_t spec_length = (
						spec_type == spec_type_t::NO_EXTRA_NULL
						? decltype(specification_container)::length
						: decltype(specification_container)::length - 1
					       );

		constexpr uint32_t table_width = calculate_table_width<decltype(specification_container)::type>();

		using instanced_token_t = token_t<table_width>;

		array_container_t<instanced_token_t, calculate_token_array_length<specification_container, spec_type>()> result { };

		size_t token_array_index = 0;
		size_t nesting_depth = 0;
		uint16_t current_termination_handler = 0;

		auto insert_token = [
				     &token_array = result,
				     &token_array_head = std::as_const(token_array_index)
				    ]
				    (
				     size_t token_array_index,
				     const instanced_token_t& token
				    )
				    consteval
		{
			for (size_t i = token_array_head; i > token_array_index; i--) { token_array[i] = token_array[i - 1]; }
			token_array[token_array_index] = token;
		}

		for (size_t i = 0; i < spec_length; i++) {
			spec_element_t element = specification[i];
			switch (element) {

			case '|':
				result[token_array_index].type = token_type_t::ALTERNATION;
				if (nesting_depth == 0) { result[token_array_index].new_termination_handler = ++current_termination_handler; }
				else { result[token_array_index].new_termination_handler = -1; }
				token_array_index++;
				break;

			case '*':
				result[token_array_index++].type = token_type_t::KLEENE_CLOSURE_END;
				size_t token_backtrack_index = token_array_index - 1;
				switch (result[--token_backtrack_index].type) {
				case token_type_t::SUBEXPRESSION_END:
					size_t nesting_depth = 1;
					while (true) {
						switch (result[--token_backtrack_index].type) {
						case token_type_t::SUBEXPRESSION_END: nesting_depth++; break;
						case token_type_t::SUBEXPRESSION_START:
							nesting_depth--;
							if (nesting_depth == 0) { goto finished_and_insert_token; }
							break;
						}
					}
				case token_type_t::TABLE_ROW: break;
				// NOTE: Any other cases cannot happen because we've already filtered those constellations out via syntax checks.
				}
finished_and_insert_token:
				insert_token(token_backtrack_index, { token_type_t::KLEENE_CLOSURE_START, { } });
				break;

			case '[':
				result[token_array_index].type = token_type_t::TABLE_ROW;
				i = parse_bracket_expression<specification_container, spec_type>(i, result[token_array_index].table_row);
				token_array_index++;
				break;

			case '(':
				result[token_array_index++].type = token_type_t::SUBEXPRESSION_START;
				nesting_depth++;
				break;

			case ')':
				result[token_array_index++].type = token_type_t::SUBEXPRESSION_END;
				nesting_depth--;
				break;

			case marker_character: result[token_array_index++].type = token_type_t::MARKER; break;

			case '.':
				result[token_array_index].type = token_type_t::TABLE_ROW;
				result[token_array_index].table_row = { true };
				token_array_index++;
				break;

			case '\\': i++;
			/* FALLTHROUGH */
			default:
				result[token_array_index].type = token_type_t::TABLE_ROW;
				result[token_array_index].table_row[element] = true;
				token_array_index++;
				break;

			}
		}

		return result;
	}

	template <typename T>
	class is_token_t {
	public:
		static constexpr bool value = false;
		consteval operator bool() { return value; }
	};
	template <size_t table_width>
	class is_token_t<token_t<table_width>> {
	public:
		static constexpr bool value = true;
		consteval operator bool() { return value; }
	};

	template <typename T>
	inline constexpr bool is_token_t_v = is_token_t<T>::value;

	template <typename T>
	concept token_t_c = is_token_t_v<T>;

	template <array_container_t_c T>
	concept token_array_container_t_c = token_type_c<T::type>;

	template <token_array_container_t_c token_array_container>
	consteval std::tuple<size_t, size_t, size_t> calculate_nfa_table_metrics() {
		using instanced_token_t = decltype(token_array_container)::type;
		constexpr size_t token_array_length = decltype(token_array_container)::length;
		// NOTE: Same work-around as above.
		const instanced_token_t (&token_array)[token_array_length];

		size_t rows = 0;

		size_t max_next_vector_capacity = 1;

		size_t current_nested_kleene_closures = 0;
		size_t max_nested_kleene_closures = current_nested_kleene_closures;

		size_t token_array_index = 0;

		constexpr auto implementation = [
						 rows,
						 &max_next_vector_capacity,
						 &current_nested_kleene_closures,
						 &max_nested_kleene_closures,
						 &token_array_index
						]
						(
						 const auto &self
						)
						consteval
						-> void
		{
			size_t current_next_vector_capacity = 1;

			for (; token_array_index < token_array_length; token_array_index++) {
				const instanced_token_t &token = token_array[token_array_index];
				switch (token) {

				case token_type_t::ALTERNATION: current_next_vector_capacity++; break;

				case token_type_t::KLEENE_CLOSURE_BEGIN:
					current_nested_kleene_closures++;
					if (current_nested_kleene_closures > max_nested_kleene_closures) {
						max_nested_kleene_closures = current_nested_kleene_closures;
					}
					break;

				case token_type_t::KLEENE_CLOSURE_END: nested_kleene_closures--; break;

				case token_type_t::SUBEXPRESSION_BEGIN:
					token_array_index++;
					self(self);
					break;

				case token_type_t::SUBEXPRESSION_END:
					if (current_next_vector_capacity > max_next_vector_capacity) {
						max_next_vector_capacity = current_next_vector_capacity;
					}
					token_array_index++;
					return;

				case token_type_t::MARKER: break;

				case token_type_t::TABLE_ROW: rows++; break;

				}
			}
		};

		return { rows, max_next_vector_capacity, max_nested_kleene_closures };
	}

	template <size_t highest_reachable_num>
	struct minimum_unsigned_integral_t_impl { };
	template <size_t highest_reachable_num, typename = std::enable_if<highest_reachable_sum < (uint8_t)-1>::type>
	struct minimum_unsigned_integral_t_impl<highest_reachable_num> { using type = uint8_t; };
	template <size_t highest_reachable_num, typename = std::enable_if<highest_reachable_sum < (uint16_t)-1>::type>
	struct minimum_unsigned_integral_t_impl<highest_reachable_num> { using type = uint16_t; };
	template <size_t highest_reachable_num, typename = std::enable_if<highest_reachable_sum < (uint32_t)-1>::type>
	struct minimum_unsigned_integral_t_impl<highest_reachable_num> { using type = uint32_t; };
	template <size_t highest_reachable_num, typename = std::enable_if<highest_reachable_sum < (uint64_t)-1>::type>
	struct minimum_unsigned_integral_t_impl<highest_reachable_num> { using type = uint64_t; };

	template <size_t highest_reachable_num>
	using minimum_unsigned_integral_t = typename minimum_unsigned_integral_t_impl<highest_reachable_num>::type;

	template <typename element_t, size_t capacity>
	class vector_t : public array_container_t<element_t, capacity> {
	public:
		// NOTE: Depending on what these types end up as, the memory layout here could be suboptimal,
		// but there's no way to fix that in current C++ as far as I'm aware.
		// In this case, it's not a big deal at all.
		using storage_size_t = minimum_unsigned_integral_t<capacity>;
		storage_size_t head;

		consteval bool push_back(const element_t &element) {
			if (head == capacity) { return false; }
			data[head++] = element;
			return true;
		}
		consteval element_t pop_back() { return data[--head]; }
	};

	template <typename source_t, typename target_t>
	concept implicitly_convertible_to_x_type_c = requires(source_t source_instance) {
		[](target_t){}(source_instance);
	};

	template <typename element_t, size_t capacity>
	class stack_t {
	public:
		vector_t<element_t, capacity> data;

		consteval bool push(const element_t &element) { return data.push_back(element); }

		template <implicitly_convertible_to_x_type_c<element_t> other_element_t, size_t other_capacity>
			// TODO: Move all the refs to the right side, as here, makes more sense given that int a, b, c behavior, just like for pointers.
		consteval bool push(const stack_t<other_element_t, other_capacity> &other_stack) { return data.push_back(other_stack.pop()); }

		consteval element_t pop() { return data.pop_back(); }

		// NOTE: IGNORES ERRORS, SO BE SURE THAT YOU WON'T GET ANY BEFORE USING!
		consteval operator<<(const element_t &right) { push(right); }

		// NOTE: IGNORES ERRORS, SO BE SURE THAT YOU WON'T GET ANY BEFORE USING!
		template <implicitly_convertible_to_x_type_c<element_t> other_element_t, size_t other_capacity>
		consteval operator<<(const stack_t<other_element_t, other_capacity> &other_stack) { push(other_stack); }

		consteval operator>>(element_t &right) { right = pop(); }
	};

	template <size_t next_vector_capacity_param, size_t marker_vector_capacity_param>
	struct nfa_table_element_t {
		static constexpr size_t next_vector_capacity = next_vector_capacity_param;
		static constexpr size_t marker_vector_capacity = marker_vector_capacity_param;

		vector_t<size_t, next_vector_capacity> next;
		vector_t<size_t, marker_vector_capacity> marker;
		uint16_t termination_handler;
	};

	// TODO: Do that retry code stuff for the marker_vector_capacity.
	// TODO: Is marker_vector_capacity exactly as big as the state buffer capacity?
	template <token_array_container_t_c token_array_container, size_t marker_vector_capacity>
	consteval auto generate_nfa_table_from_tokens() {
		using instanced_token_t = decltype(token_array_container)::type;
		constexpr size_t token_array_length = decltype(token_array_container)::length;
		// NOTE: Same work-around as the ones above.
		const instanced_token_t (&token_array)[token_array_length] = token_array_container.data;

		constexpr uint32_t table_width = instanced_token_t::row_length;

		constexpr auto [table_length, element_next_vector_capacity, kleene_stack_capacity] = calculate_nfa_table_metrics<token_array_container>();
		using instanced_nfa_table_element_t = nfa_table_element_t<element_next_vector_capacity, marker_vector_capacity>;
		array_container_t<instanced_nfa_table_element_t, table_length * table_width> result { };

		size_t table_head = 0;

		stack_t<size_t, kleene_stack_capacity> kleene_stack;

		constexpr auto superimpose_table_row = [
							&table_width
						       ]
						       (
							size_t row_number,
							const bool (&row)[table_width],
							bool inc_table_head = true
						       )
						       consteval
		{
			// TODO: Go through code and see where else you can use that minimum_unsigned_integral_t thing.
			const size_t row_offset = row_number * table_width;
			instanced_nfa_table_element *new_last_table_row = result + row_offset;
			for (uint32_t i = 0; i < instanced_token_t::row_length; i++) {
				if (row[i] == false) { continue; }
				instanced_nfa_table_element_t &element = result[row_offset + i];
				if (element.next.push_back(table_head) == false) {
					report_error("moken bug detected: nfa element next vector capacity exceeded");
				}
			}
			table_head++;
		};

		constexpr auto implementation = [
						 &token_array,
						 &token_array_length,
						 &result,
						 &superimpose_table_row,
						 &element_next_vector_capacity,
						 &table_width
						]
						(
						 size_t token_array_index,
						 size_t current_row,
						 const auto &push_to_last_next,
						 size_t nesting_depth,
						 size_t subexpression_end_target,
						 const auto& self
						)
						consteval
						-> std::pair<size_t, size_t, size_t, size_t>
		{
			// NOTE: THE FOLLOWING IS SUPER FUCKED, YOU MIGHT HAVE TO START OVER, FIGURE SOMETHING GOOD OUT.

			const instanced_token_t &token = token_array[token_array_index];
			switch (token.type) {

			case token_type_t::ALTERNATION: return { token_array_index + 1, nesting_depth };

			case token_type_t::KLEENE_CLOSURE_START:
				kleene_stack << current_row;	// TODO: Only leave this once you know the capacity is bullet-proof, as above.
				// TODO: And same with the other appearences of the << operator.
				return self(token_array_index + 1, current_row, last_table_element, self);

			case token_type_t::KLEENE_CLOSURE_END:
				last_table_element.next << kleene_stack;
				return self(token_array_index + 1, current_row, last_table_element, self);

			case token_type_t::SUBEXPRESSION_START:
				vector_t<size_t, element_next_vector_capacity> branch_end_row_nums;
				vector_t<array_container_t<bool, table_width>, element_next_vector_capacity> branch_end_row_filters;

				size_t new_token_array_index = token_array_index + 1;

				while (true) {
					const auto [returned_token_array_index, returned_nesting_depth, returned_end_row_num, returned_end_row_filter] = self(new_token_array_index,
																 table_head++,
																 last_table_element,
																 nesting_depth + 1,
																 current_row,
																 self);

					// TODO: error handling
					branch_end_row_nums.push_back(returned_end_row_num);
					branch_end_row_filters.push_back(returned_end_row_filters);

					new_token_array_index = returned_token_array_index;
					if (returned_nesting_depth == nesting_depth) { break; }
					if (returned_nesting_depth < nesting_depth) {
						// TODO: throw error
					}
				}

				for (size_t i = 0; i < decltype(branch_end_row_nums)::length; i++) {
					superimpose_table_row(branch_end_row_nums[i], branch_end_row_filters[i], superimpose_behavior::NO_INC_TABLE_HEAD);
				}

				constexpr auto new_push_to_last_next = []() {
					for (
				};

				return self(new_token_array_index, table_head, , nesting_depth, self);

			case token_type_t::SUBEXPRESSION_END: return self(token_array_index + 1, current_row, last_table_element, nesting_depth - 1, self);

			case token_type_t::MARKER:
				last_table_element.marker++;
				return self(token_array_index + 1, current_row, last_table_element, nesting_depth, self);

			case token_type_t::TABLE_ROW:
				nfa_table_element_t *new_last_table_row = superimpose_table_row(current_row, token.table_row);
				return self(token_array_index + 1, table_head, new_last_table_row, nesting_depth, self);

			}
		};

		size_t next_token_array_index = 0;
		while (true) {
			auto [new_next_token_array_index, new_nesting_depth] = implementation(next_token_array_index, 0, nullptr, 1, implementation);
			if (new_nesting_depth == 0) { break; }
			next_token_array_index = new_next_token_array_index;
		}

		return result;
	}

	struct dfa_table_element_t {
		const dfa_table_element_t *next;
		size_t markers;
		bool is_terminator;
	};

	template <typename T>
	struct is_nfa_table_element_type { static constexpr bool value = false; };
	template <size_t next_vector_capacity>
	struct is_nfa_table_element_type<nfa_table_element_t<next_vector_capacity>> { static constexpr bool value = true; };

	template <typename T>
	inline constexpr bool is_nfa_table_element_type_v = is_nfa_table_element_type<T>::value;

	template <typename T>
	concept nfa_table_element_type_c = is_nfa_table_element_type_v<T>;

	template <typename T>
	concept nfa_table_element_array_container_type_c = nfa_table_element_type_c<T::type>;

	template <nfa_table_element_array_container_type_c nfa_table_container, uint32_t table_width>
	consteval auto convert_nfa_to_dfa() {
		using instanced_nfa_table_element_t = decltype(nfa_table_container)::type;
		constexpr size_t nfa_table_1d_length = decltype(nfa_table_container)::length;
		constexpr size_t nfa_table_length = nfa_table_1d_length / table_width;
		const instanced_nfa_table_element_t (&nfa_table)[nfa_table_1d_length] = nfa_table_container.data;

		array_container_t<dfa_table_element_t, nfa_table_1d_length> dfa_table { };
		size_t dfa_table_head_row = 1;

		constexpr auto superimpose_element_next_vector_versions = [
									   &nfa_table
									  ]
									  <
									   size_t possible_states_buffer_capacity
									  >
									  (
									   // TODO: constraint
									   const auto &possible_states,
									   size_t row_index
									  )
									  consteval
		{
			vector_t<size_t, possible_states_buffer_capacity> result;
			for (size_t state : possible_states) {
				result.push_back(nfa_table[state * table_width + row_index].next);
			}
			result.remove_duplicates();
			return result;
		};

		constexpr auto implementation = [
						 &nfa_table,		// TODO: as const
			  			 &dfa_table,
						 &dfa_table_head_row
						]
						(
						 size_t dfa_row,
						 const auto &possible_states
						 const auto &self
						)
						consteval
		{
			// TODO: markers, create 2D array of lists of markers, which we'll fill up with all the possible marker lists up to this point.
			// This is because every state has it's own possible marker list up to this point and we need to store all of those.
			// We can measure out the container though, because the process isn't self-referential like the state process is.
			// So measure out the container and store the lists and then decide which list to finalize in the table when you reach a stop condition.
			// Maybe it's not possible, think about it again. These are essentially captures you know.
			bool finished_elements[table_width];
			for (size_t j = table_width * dfa_row; j < table_width * current_state + table_width; j++) {
				if (fin_el == true) { continue; }

				auto [possible_states_buffer_j, termination_handler] = superimpose_element_versions<possible_states_buffer_capacity>(possible_states,
																	possible_states_length,
																	j - table_width * current_state);

				finished_elements[j - table_width * dfa_row] = true;
				dfa_table[j].next = dfa_table_head_row;
				dfa_table[j].termination_handler = termination_handler;
				for (size_t k = j + 1; k < table_width * current_state + table_width; k++) {
					if (fin_el == true) { continue; }

				auto [possible_states_buffer_k, termination_handler] = superimpose_element_versions<possible_states_buffer_capacity>(possible_states,
																	possible_states_length,
																	j - table_width * current_state);

					if (possible_states_buffer_j == possible_states_buffer_k) {
						finished_elements[k - table_width * current_state] = true;
						dfa_table[k].next = dfa_table_head_row;
						dfa_table[k].termination_handler = termination_handler;
					}
				}
				self(dfa_table_head_row, possible_states_buffer_j, self);
				dfa_table_head_row++;
			}
		};

		return dfa_table;
	}

	template <size_t dfa_table_1d_length_param>
	class tokenizer_t {
	public:
		static constexpr size_t dfa_table_1d_length = dfa_table_1d_length_param;

		dfa_table_element_t dfa_table[dfa_table_1d_length];
	};

	template <compatible_array_container_type_c specification, spec_type_t spec_type = spec_type_t::EXTRA_NULL>
	consteval auto make_tokenizer_t() {
		constexpr auto spec_token_array = tokenize_specification<specification, spec_type>();
		constexpr auto nfa_table = generate_nfa_table_from_tokens<spec_token_array>();
		constexpr auto dfa_table = convert_nfa_to_dfa<nfa_table, /* table width */>();
		constexpr auto tail_combined_dfa_table = do_dfa_tail_combination<dfa_table, /* table width */>();
		tokenizer_t<decltype(tail_combined_dfa_table)::dfa_table_1d_length> result { tail_combined_dfa_table };
		return result;
	}

}

// TODO: We might not need the macro if we can do the finalizing in the constructor of the tokenizer_t.
// TODO: The trick is we only do it if we can take the address of the data, which is only valid sometimes, so you're gonna need some SFINAE.
// That way, no matter how the compiler chooses to return result from make_tokenizer_t(), we'll finalize it once and only once and we'll do it at the right time.
#define MAKE_MOKEN_TOKENIZER_T(NAME, SPECIFICATION, ...) auto NAME = make_tokenizer_t<SPECIFICATION __VA_OPT__(,) __VA_ARGS__>(); NAME.finalize();
