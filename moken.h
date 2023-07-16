#pragma once

#include <cstddef>
#include <cstdint>
#include <utility>
#include <concepts>
#include <type_traits>

namespace moken {

	inline constexpr char marker_character = '@';

	void report_error(const char *message) noexcept;

	template <typename U, typename V>
	class are_types_same { public: consteval operator bool() { return false; } };
	template <typename T>
	class are_types_same<T, T> { public: consteval operator bool() { return true; } };

	template <typename U, typename V>
	concept same_as_c = ((bool)are_types_same<U, V>{});

	template <typename T>
	concept convertible_to_compile_time_array_c = requires(T instance, decltype(*(instance.begin())) element_instance) {
		{ *(instance.end())  } -> same_as_c<decltype(element_instance)>;
		{ element_instance++ } -> same_as_c<T>;
		{ ++element_instance } -> same_as_c<T&>;
		T::length;
	};

	template <convertible_to_compile_time_array_c source_container_t>
	array_container_t(const source_container_t& source_container) -> array_container_t<decltype(*(source_container.begin())), source_container_t::length>;

	template <typename element_t, size_t array_length>
	class array_container_t {
	public:
		using type = element_t;
		static constexpr size_t length = array_length;

		element_t data[length];

		consteval array_container_t(const element_t (&source_array)[array_length]) {
			for (size_t i = 0; i < length; i++) { data[i] = source_array[i]; }
		}

		template <convertible_to_compile_time_array_c source_container_t>
		consteval array_container_t(const source_container_t& source_container) {
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

		consteval iterator_t begin() { return &data; }
		consteval const_iterator_t begin() const { return &data; }

		consteval iterator_t end() { return &data + length; }
		consteval const_iterator_t end() const { return &data + length; }
	};

	template <typename T>
	struct is_array_container_type { static constexpr bool value = false; };
	template <typename element_t, size_t array_length>
	struct is_array_container_type<array_container_t<element_t, array_length>> { static constexpr bool value = true; };
	template <typename T>
	inline constexpr bool is_array_container_type_v = is_array_container_type<T>::value;

	template <typename T>
	concept array_container_type_c = is_array_container_type_v<T>;

	template <std::unsigned_integral T>
	concept compatible_array_container_element_type_c = (sizeof(T) <= 2);

	template <array_container_type_c array_container_type>
	concept compatible_array_container_type_c = compatible_array_container_element_type_c<typename array_container_type::type>;

	template <std::integral T>
	concept sufficiently_compatible_array_container_element_type_c = (sizeof(T) <= 2);

	template <array_container_type_c array_container_type>
	concept sufficiently_compatible_array_container_type_c = sufficiently_compatible_array_container_element_type_c<typename array_container_type::type>;

	template <auto array_container> requires sufficiently_compatible_array_container_type_c<decltype(array_container)>
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

	template <compatible_array_container_type_c specification_container, spec_type_t spec_type = spec_type_t::EXTRA_NULL>
	consteval size_t calculate_max_table_length() {
		using spec_element_t = typename decltype(specification_container)::type;
		// NOTE: The following line of code is a work-around for what seems to be a compiler bug.
		// I can't write specification_container[i], clang keeps complaining that i is non-const and can't be used in a constant expression, even though this should be an exception since we're using it from inside
		// a consteval function. I think the compiler must somehow forget the consteval-ness along the way and I think the error is a bug.
		// Further proof is that with the following line (which just establishes a reference called "specification" to the inner array in specification_container), everything works exactly as intended.
		// TODO: Something's fishy here, bug report this.
		const spec_element_t (&specification)[decltype(specification_container)::length] = specification_container.data;
		constexpr size_t spec_length = (spec_type == NO_EXTRA_NULL ? decltype(specification_container)::length : decltype(specification_container)::length - 1);

		// NOTE: The return 0 is necessary here to allow the constexpr if statement to actually works it's magic. Or else everything else will get instantiated anyway I suppose.
		if constexpr (spec_length == 0) { report_error("moken spec syntax error: specification cannot be empty"); return 0; }

		size_t i = 0;
		size_t result = 0;
		bool is_inside_bracket_expression = false;

		// NOTE: I wanted to make this consteval instead of constexpr, but for some reason (which smells strongly of a compiler bug, TODO: REPORT!!!!) consteval doesn't work, even though both versions are executed at compile-time in this case.
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
					// TODO: I wanna be able to put them after ) characters, but we need to switch the subexpression system tokenization to more of the likes of the kleene closure system for that to work, because we have to apply it to the table row tokens, as with kleene closure ends.
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
		TABLE_ROW,
		ALTERNATION,
		KLEENE_CLOSURE_BEGIN,
		KLEENE_CLOSURE_END,
		SUBEXPRESSION_BEGIN,
		SUBEXPRESSION_END,
		MARKER
	};

	struct token_t {
		token_type_t type;
		bool table_row[table_width];
		size_t additional_subexpression_skips;		// NOTE: only used when type is token_type_t::KLEENE_CLOSURE_END
	};

	template <compatible_array_container_type_c specification_container, spec_type_t spec_type = spec_type_t::EXTRA_NULL>
	consteval size_t calculate_token_array_length() {
		using spec_element_t = typename decltype(specification_container)::type;
		// NOTE: Same work-around as above.
		const spec_element_t (&specification)[decltype(specification_container)::length] = specification_container.data;
		constexpr size_t spec_length = (spec_type == spec_type_t::NO_EXTRA_NULL ? decltype(specification_container)::length : decltype(specification_container)::length - 1);

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

	template <typename specification_container_element_t> requires compatible_array_container_element_type_c<specification_container_element_t>;
	consteval uint32_t calculate_table_width() { return (uint32_t)(specification_container_element_t)-1 + 1; }

	template <compatible_array_container_type_c specification_container, spec_type_t spec_type = spec_type_t::EXTRA_NULL>
	consteval auto parse_bracket_expression(size_t spec_index, bool (&table_row)[calculate_table_width<typename decltype(specification_container)::type>()]) {
		using spec_element_t = typename decltype(specification_container)::type;
		// NOTE: Same work-around as above.
		const spec_element_t (&specification)[decltype(specification_container)::length] = specification_container.data;
		constexpr size_t spec_length = (spec_type == spec_type_t::NO_EXTRA_NULL ? decltype(specification_container)::length : decltype(specification_container)::length - 1);

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

		for (size_t i = spec_index + 2; specification[i] != ']'; i++) {
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
				// NOTE: We just add all the characters whose codes are in between the codes of the two boundary characters, and we also add the boundary characters themselves of course.
				// This is obviously dependant on the character encoding that your compiler is using, but I figure that aspect could be useful in and of itself.
				// I've also never seen a C++ compiler stray from ASCII for char encoding, so anything other than ASCII is probably very unlikely.
				// ASCII takes care of us very well in this situation: [A-Z, a-z, 0-9] all work great and subsets of those obviously work great as well.
				for (spec_element_t j = previous_character + 1; j <= character; j++) { add_character(table_row, specification[j]); }
				previous_character_available_for_hyphen = false;
				continue;
			}
			if (character == '\\') { i++; character = specification[i]; }
			add_character(table_row, character);
			previous_character = character;
			previous_character_available_for_hyphen = true;
		}
	}

}
