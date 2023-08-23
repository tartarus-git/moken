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
	consteval void check_specification_syntax() {
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
		bool is_inside_bracket_expression = false;

		// NOTE: I wanted to make this consteval instead of constexpr, but for some reason (which smells strongly of a compiler bug, TODO: REPORT!!!!)
		// consteval doesn't work, even though both versions are executed at compile-time in this case.
		auto func_implementation = [&i, &is_inside_bracket_expression](size_t nesting_depth, const auto& self) constexpr -> void {
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
					break;

				case '.':
					/* FALLTHROUGH */

				default:
					if (is_inside_bracket_expression) { break; }	// NOTE: Further syntax checking of bracket expressions is done below.
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
		// TODO: Replace this with something that doesn't use result, because we removed that.
		//if (result == 0) { report_error("moken spec syntax error: specification must produce at least one DFA table row"); }

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
		bool table_row[row_length];
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
	consteval std::tuple<size_t, size_t, size_t, size_t> calculate_table_metrics() {
		using instanced_token_t = decltype(token_array_container)::type;
		constexpr size_t token_array_length = decltype(token_array_container)::length;
		// NOTE: Same work-around as above.
		const instanced_token_t (&token_array)[token_array_length];

		size_t dfa_max_rows = 0;
		size_t nfa_max_rows = 0;

		size_t max_next_vector_capacity = 1;

		size_t current_nested_kleene_closures = 0;
		size_t max_nested_kleene_closures = current_nested_kleene_closures;

		size_t token_array_index = 0;

		constexpr auto implementation = [
						 &nfa_max_rows,
						 &dfa_max_rows,
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

				case token_type_t::ALTERNATION: current_next_vector_capacity++; nfa_max_rows++; break;

				case token_type_t::KLEENE_CLOSURE_BEGIN:
					current_nested_kleene_closures++;
					if (current_nested_kleene_closures > max_nested_kleene_closures) {
						max_nested_kleene_closures = current_nested_kleene_closures;
					}
					break;

				case token_type_t::KLEENE_CLOSURE_END: nested_kleene_closures--; nfa_max_rows++; break;

				case token_type_t::SUBEXPRESSION_BEGIN:
					token_array_index++;
					nfa_max_rows++;
					self(self);
					break;

				case token_type_t::SUBEXPRESSION_END:
					if (current_next_vector_capacity > max_next_vector_capacity) {
						max_next_vector_capacity = current_next_vector_capacity;
					}
					token_array_index++;
					nfa_max_rows++;
					return;

				case token_type_t::MARKER: nfa_max_rows++; break;

				case token_type_t::TABLE_ROW: nfa_max_rows++; dfa_max_rows++; break;

				}
			}
		};

		return { nfa_max_rows, dfa_max_rows, max_next_vector_capacity, max_nested_kleene_closures };
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

	template <size_t next_vector_capacity_param>
	struct nfa_table_element_t {
		static constexpr size_t next_vector_capacity = next_vector_capacity_param;

		vector_t<size_t, next_vector_capacity> next;
		bool marker;
	};

	// TODO: fix this, we need to accept the value and not the type
	template <token_array_container_t_c token_array_container,
		  size_t table_length,
		  size_t element_next_vector_capacity,
		  size_t kleene_stack_capacity>
	consteval auto generate_nfa_table_from_tokens() {
		using instanced_token_t = decltype(token_array_container)::type;
		constexpr size_t token_array_length = decltype(token_array_container)::length;
		// NOTE: Same work-around as the ones above.
		const instanced_token_t (&token_array)[token_array_length] = token_array_container.data;

		constexpr uint32_t table_width = instanced_token_t::row_length;

		using instanced_nfa_table_element_t = nfa_table_element_t<element_next_vector_capacity>;
		array_container_t<instanced_nfa_table_element_t, table_length * table_width> nfa_table { };

		array_container_t<bool, table_length> ghost_rows { };

		size_t table_head = 0;

		stack_t<size_t, kleene_stack_capacity> kleene_stack;

		constexpr auto create_new_row = [&table_length]() consteval {
			if (table_head >= table_length) {
				report_error("moken bug detected: nfa table head overflowed");
			}
			return table_head++;
		};

		constexpr auto register_ghost_row = [&ghost_rows](size_t row_number) consteval {
			if (row_number >= table_head) {
				report_error("moken bug detected: register_ghost_row(size_t) called with out-of-bounds row_number");
			}
			ghost_rows[row_number] = true;
		}

		constexpr auto superimpose_table_row = [
							&table_width,
							&nfa_table
						       ]
						       (
							size_t row_number,
							const bool (&row)[table_width],
							size_t target_row_number
						       )
						       consteval
			// TODO: Go through code and see where else you can use that minimum_unsigned_integral_t thing.
		{
			const size_t row_offset = row_number * table_width;

			for (uint32_t i = 0; i < instanced_token_t::row_length; i++) {
				if (row[i] == false) { continue; }

				instanced_nfa_table_element_t &element = nfa_table[row_offset + i];
				if (element.next.push_back(target_row_number) == false) {
					report_error("moken bug detected: nfa element next vector capacity blown while superimposing non-ghost row");
				}
			}
		};

		constexpr auto superimpose_ghost_row = [
							&table_width,
							&nfa_table
						       ]
						       (
							size_t row_number,
							size_t target_row_number
						       )
						       consteval
		{
			const size_t row_offset = row_number * table_width;

			if (ghost_rows[row_number] == false) {
				report_error("moken bug detected: cannot ghost superimpose onto non-ghost row");
			}

			if (nfa_table[row_offset].next.push_back(target_row_number) == false) {
				report_error("moken bug detected: nfa element next vector capacity blown while superimposing ghost row");
			}
		};

		constexpr auto implementation = [&]
						(
						 size_t token_array_index,
						 size_t current_row,
						 size_t last_table_row,
						 const auto& self
						)
						consteval
						-> std::pair<size_t, bool, size_t>
		{
			if (token_array_index >= token_array_length) {
				register_ghost_row(current_row);
				return { token_array_index, true, current_row };
			}

			const instanced_token_t &token = token_array[token_array_index];
			switch (token.type) {

			case token_type_t::ALTERNATION:
				register_ghost_row(current_row);
				return { token_array_index + 1, false, current_row };

			case token_type_t::KLEENE_CLOSURE_START:
				kleene_stack.push(current_row);
				return self(token_array_index + 1, current_row, last_table_row, self);

			case token_type_t::KLEENE_CLOSURE_END:
				register_ghost_row(current_row);
				superimpose_ghost_row(current_row, kleene_stack.pop());
				size_t next_row = create_new_row();
				superimpose_ghost_row(current_row, next_row);
				return self(token_array_index + 1, next_row, current_row, self);

			case token_type_t::SUBEXPRESSION_START:
				register_ghost_row(current_row);

				vector_t<size_t, element_next_vector_capacity> branch_end_rows;

				size_t new_token_array_index = token_array_index + 1;

				while (true) {
					const size_t target_row = create_new_row();
					superimpose_ghost_row(current_row, target_row);
					const auto [returned_token_array_index,
					      	    should_break_out,
						    returned_end_row] = self(new_token_array_index,
									     target_row,
									     last_table_element,
									     current_row,
									     self);

					if (branch_end_rows.push_back(returned_end_row) == false) {
						report_error("moken bug detected: branch_end_rows capacity blown in nfa gen SUBEXPRESSION_START");
					}

					new_token_array_index = returned_token_array_index;

					if (should_break_out) { break; }
				}

				const size_t ghost_sink = create_new_row();
				register_ghost_row(ghost_sink);
				for (size_t i = 0; i < decltype(branch_end_row_nums)::length; i++) {
					superimpose_ghost_row(branch_end_row_nums[i], ghost_sink);
				}

				const size_t next_row = create_new_row();
				superimpose_ghost_row(ghost_sink, next_row);
				return self(new_token_array_index, next_row, ghost_sink, self);

			case token_type_t::SUBEXPRESSION_END:
				register_ghost_row(current_row);
				return { token_array_index + 1, true, current_row };

			case token_type_t::MARKER:
				register_ghost_row(current_row);
				nfa_table[current_row * table_width].marker = true;
				size_t next_row = create_new_row();
				superimpose_ghost_row(current_row, next_row);
				return self(token_array_index + 1, next_row, current_row, self);

			case token_type_t::TABLE_ROW:
				size_t next_row = create_new_row();
				superimpose_table_row(current_row, token.table_row, next_row);
				return self(token_array_index + 1, next_row, current_row, self);

			}
		};


		vector_t<size_t, (uint16_t)-1 - 1> branch_end_rows;
		size_t next_token_array_index = 0;
		while (true) {
			auto [returned_next_token_array_index, should_break_out, returned_end_row] = implementation(next_token_array_index,
														    0,
														    -1,
														    1,
														    implementation);
			if (branch_end_rows.push_back(returned_end_row) == false) {
				// TODO: Check that this number doesn't exceed before-hand as form of user input validation.
				// At this stage, it IS a bug, if it hasn't been caught already.
				report_error("moken bug detected: top-level branch_end_rows vector length exceeded capacity while building nfa");
			}
			next_token_array_index = returned_next_token_array_index;
			if (should_break_out) { break; }
		}

		uint16_t current_termination_handler = 1;	// NOTE: Starting at 1 is necessary because nfa table is zero-ed out.
		for (size_t end_row : branch_end_rows) {
			nfa_table[end_row * table_width].termination_handler = current_termination_handler++;
		}

		return std::pair(nfa_table, ghost_rows);
	}

	struct dfa_table_element_t {
		const dfa_table_element_t *next;
		size_t markers;
	};

	template <typename T>
	class is_nfa_table_element_t {
	public:
		static constexpr bool value = false;
		consteval operator bool() const { return value; }
	};
	template <size_t next_vector_capacity>
	struct is_nfa_table_element_t<nfa_table_element_t<next_vector_capacity>> {
	public:
		static constexpr bool value = true;
		consteval operator bool() const { return value; }
	};

	template <typename T>
	inline constexpr bool is_nfa_table_element_t_v = is_nfa_table_element_t<T>::value;

	template <typename T>
	concept nfa_table_element_t_c = is_nfa_table_element_t_v<T>;

	template <array_container_t_c T>
	concept nfa_table_row_container_c = nfa_table_element_t_c<T::type>;

	template <typename T>
	class is_nfa_table {
	public:
		static constexpr bool value = false;
		consteval operator bool() const { return value; }
	};
	template <nfa_table_row_container_c nfa_table_row_container_t>
	class is_nfa_table<std::pair<nfa_table_row_container_t, array_container_t<bool, nfa_table_row_container_t::length>>> {
	public:
		static constexpr bool value = true;
		consteval operator bool() const { return value; }
	};

	template <typename T>
	inline constexpr bool is_nfa_table_v = is_nfa_table<T>::value;

	template <typename T>
	concept nfa_table_c = is_nfa_table_v<T>;

	template <auto nfa_table_package, size_t dfa_table_length, uint32_t table_width, size_t possible_states_capacity> requires nfa_table_c<decltype(nfa_table_package)>
	consteval auto convert_nfa_to_dfa() {
		auto nfa_table_container = nfa_table_package.first;
		auto nfa_ghost_rows = nfa_table_package.second;
		auto nfa_termination_handlers = nfa_table_package.third;

		using instanced_nfa_table_element_t = decltype(nfa_table_container)::type;
		constexpr size_t nfa_table_1d_length = decltype(nfa_table_container)::length;
		constexpr size_t nfa_table_length = nfa_table_1d_length / table_width;

		if (nfa_table_length < dfa_table_length) {
			report_error("moken bug detected: nfa_table_length is smaller than dfa_table_length in convert_nfa_to_dfa()");
		}

		const instanced_nfa_table_element_t (&nfa_table)[nfa_table_1d_length] = nfa_table_container.data;

		array_container_t<dfa_table_element_t, dfa_table_length * table_width> dfa_table { };
		array_container_t<size_t, dfa_table_length> dfa_termination_handlers { };
		size_t dfa_table_head_row = 0;

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

		constexpr auto implementation = [&]
						(
						 size_t dfa_row,
						 vector_t<size_t, possible_states_capacity> &possible_states,
						 const auto &self
						)
						consteval
						-> // TODO: return value
		{
			const auto possible_states_copy = possible_states;
			register_dfa_termination_handler(dfa_row, superimpose_termination_handler_versions(possible_states_copy));

			const size_t row_offset = dfa_row * table_width;

			// TODO: Do we have the type that we're looking for stored somewhere, cuz then we don't have to find it out here.
			size_t finished_elements[table_width] { (size_t)-1 };

			{
				array_container_t<vector_t<size_t, possible_states_capacity>, table_width> possible_states_for_every_element;
				for (size_t i = 0; i < table_width; i++) {
					auto &possible_states_of_element = possible_states_for_every_element[i];
					// TODO: remove row_offset from below
					superimpose_element_versions(possible_states_copy, possible_states_of_element, i + row_offset);
				}

				for (size_t i = 0; i < table_width; i++) {
					if (finished_elements[i] != -1) { continue; }

					size_t target_row = create_new_dfa_row();

					finished_elements[i] = target_row;

					set_dfa_next(dfa_row, i, target_row);

					for (size_t j = i + 1; j < row_offset + table_width; j++) {
						if (finished_elements[j] != 0) { continue; }

						if (possible_states_for_every_element[i] == possible_states_for_every_element[j]) {
							finished_elements[j] = target_row;
							set_dfa_next(dfa_row, j, target_row);
						}
					}
				}

				possible_states = possible_states_for_every_element[0];
			}

			self(finished_elements[0], possible_states, self);
			// TODO: For the 1 child case, try disposing possible_states_copy before recursing as well as anything else
			// you can possibly dispose. For memory savings.

			size_t last_dfa_row = 0;
			for (minimum_unsigned_t<table_width> i = 1; i < table_width; i++) {
				if (finished_elements[i] <= last_dfa_row) { continue; }
				last_dfa_row = finished_elements[i];
				superimpose_element_versions(possible_states_copy, possible_states, i + row_offset);
				self(finished_elements[i], possible_states, self);
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
		check_specification_syntax<specification, spec_type>();
		constexpr auto spec_token_array = tokenize_specification<specification, spec_type>();
		constexpr auto [nfa_max_rows, dfa_max_rows, element_next_vector_capacity, kleene_stack_capacity]
			= calculate_table_metrics<spec_token_array>();
		constexpr auto nfa_table = generate_nfa_table_from_tokens<spec_token_array, nfa_max_rows, element_next_vector_capacity, kleene_stack_capacity>();
		constexpr auto dfa_table = convert_nfa_to_dfa<nfa_table, dfa_max_rows, /* table width */>();
		constexpr auto tail_combined_dfa_table = do_dfa_tail_combination<dfa_table, /* table width */>();
		tokenizer_t<decltype(tail_combined_dfa_table)::dfa_table_1d_length> result { tail_combined_dfa_table };
		return result;
	}

}

// TODO: We might not need the macro if we can do the finalizing in the constructor of the tokenizer_t.
// TODO: The trick is we only do it if we can take the address of the data, which is only valid sometimes, so you're gonna need some SFINAE.
// That way, no matter how the compiler chooses to return result from make_tokenizer_t(), we'll finalize it once and only once and we'll do it at the right time.
#define MAKE_MOKEN_TOKENIZER_T(NAME, SPECIFICATION, ...) auto NAME = make_tokenizer_t<SPECIFICATION __VA_OPT__(,) __VA_ARGS__>(); NAME.finalize();
