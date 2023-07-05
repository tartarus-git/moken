#pragma once

namespace moken {

	void report_error(const char *message) noexcept;

	template <typename element_t, size_t array_length>
	class array_container_t {
	public:
		using type = element_t;
		constexpr size_t length = array_length;

		element_t data[length];

		consteval element_t& operator[](size_t index) { return data[index]; }
	};

	/*
	template <auto first_element, decltype(first_element)... trailing_pack>
	class value_parameter_pack_first_element {
	public:
		using type = decltype(first_element);
		constexpr type value = first_element;
		consteval operator type() { return value; }
	};
	*/

	template <auto first_element, decltype(first_element)... trailing_pack>
	consteval auto value_parameter_pack_to_array_container_t() {
		array_container_t<decltype(first_element), sizeof...(pack)> result;

		result[0] = first_element;
		if constexpr (sizeof...(pack) == 0) { return result; }

		auto trailing_array = value_parameter_pack_to_array_container_t<trailing_pack...>();
		for (size_t i = 1; i < decltype(result)::length; i++) { result[i] = trailing_array[i - 1]; }

		return result;
	}

	template <char... specification>
	consteval size_t calculate_max_table_length() {
		auto spec_array = value_parameter_pack_to_array_container_t<specification>();

		size_t i = 0;
		size_t result = 0;
		bool is_inside_bracket_expression = false;

		auto func_implementation = [&spec_array, &i, &result, &is_inside_bracket_expression, &func_implementation](size_t nesting_depth) consteval {
			for (; i < decltype(spec_array)::length; i++) {
				char character = spec_array[i];
				switch (character) {
				case '|':
					if (i == 0) { report_error(R"(moken spec syntax error: alternation ("|") must be preceeded by something)"); }
					if (i == decltype(spec_array)::length - 1) { report_error(R"(moken spec syntax error: alternation ("|") must be succeeded by something)"); }
					if (is_inside_bracket_expression) { report_error(R"(moken spec syntax error: alternation ("|") invalid inside bracket expression)"); }
					switch (spec_array[i - 1]) {
					case '|': report_error(R"(moken spec syntax error: two alternations ("|") cannot be separated by nothing)");
					case '(': report_error(R"(moken spec syntax error: alternation ("|") cannot be preceeded by an opening parenthesis ("("))");
					}
					break;
				case '*':
					if (i == 0) { report_error(R"(moken spec syntax error: kleene closure ("*") must be preceeded by something)"); }
					if (is_inside_bracket_expression) { report_error(R"(moken spec syntax error: kleene closure ("*") invalid inside bracket expression)"); }
					switch (spec_array[i - 1]) {
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
					switch (spec_array[i - 1]) {
					case '[': report_error(R"(moken spec syntax error: bracket expression ("[...]") cannot be empty)");
					}
					is_inside_bracket_expression = false;
					result++;
					break;
				case '(':
					if (i == decltype(spec_array)::length - 1) { report_error(R"(moken spec syntax error: opening parenthesis ("(") must be succeeded by something)"); }
					if (!is_inside_bracket_expression) { report_error(R"~(moken spec syntax error: subexpression ("(...)") invalid inside bracket expression ("[...]"))~"); }
					i++;
					func_implementation(nesting_depth + 1);
					break;
				case ')':
					if (nesting_depth == 0) { report_error(R"~(moken spec syntax error: closing parenthesis (")") is invalid without corresponding opening parenthesis ("("))~"); }
					if (i == 0) { report_error(R"~(moken spec syntax error: subexpression ("(...)") cannot be empty)~"); }
					i++;
					return;
					break;
				case '\\':
					if (i == decltype(spec_array)::length - 1) { report_error(R"(moken spec syntax error: escape character ("\") must be succeeded by something)"); }
					switch (spec_array[i + 1]) {
					case '|':
					case '*':
					case '[':
					case ']':
					case '(':
					case ')':
					case '\\':
						break;
					default: report_error(R"(moken spec syntax error: escape character ("\") must be succeeded by metacharacter)");
					}
					result++;
					break;
				default:
					if (is_inside_bracket_expression) { break; }
					result++;
					break;
				}
			}
		};
		func_implementation(0);

		return result;
	}

	struct dfa_table_element_t {
		const dfa_table_element_t *next;
	};

	template <char... specification, size_t table_length = calculate_max_table_length<specification>()>
	struct relative_untrimmed_tokenizer_t {
		dfa_table_element_t table[table_length * (unsigned char)-1];

		consteval relative_untrimmed_tokenizer_t() {
			auto spec_array = value_parameter_pack_to_array_container_t<specification>();

		}
	};

	template <size_t table_length>
	class tokenizer_t {
	public:
		dfa_table_element_t table[table_length];

		tokenizer_t() = delete;
	};

	template <char... specification>
	consteval auto make_tokenizer_t() noexcept {
		relative_untrimmed_tokenizer_t<specification> relative_untrimmed;
		return nullptr;	// TODO
	}

}
