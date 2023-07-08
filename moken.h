#pragma once

#include <cstddef>
#include <cstdint>

namespace moken {

	constexpr size_t table_width = (unsigned char)-1;

	void report_error(const char *message) noexcept;

	template <typename element_t, size_t array_length>
	struct array_container_t {
		using type = element_t;
		static constexpr size_t length = array_length;

		element_t data[length];

		consteval array_container_t(const element_t (&source_array)[array_length]) {		// TODO: Consider making this even more general so that many different types could be accepted, std::string, etc...
			for (size_t i = 0; i < length; i++) { data[i] = source_array[i]; }
		}

		// NOTE: These two aren't used as far as I know, I would like to use them, but a compiler bug seems to be preventing me from doing so.
		// I use a work-around, but I'm keeping these two functions in case things change or I'm able to use them in a different spot or something.
		consteval element_t& operator[](size_t index) { return data[index]; }
		consteval const element_t& operator[](size_t index) const { return data[index]; }
	};

	template <array_container_t specification_container>
	consteval size_t calculate_max_table_length() {
		using spec_element_t = typename decltype(specification_container)::type;
		// NOTE: The following line of code is a work-around for what seems to be a compiler bug.
		// I can't write specification_container[i], clang keeps complaining that i is non-const and can't be used in a constant expression, even though this should be an exception since we're using it from inside
		// a consteval function. I think the compiler must somehow forget the consteval-ness along the way and I think the error is a bug.
		// Further proof is that with the following line (which just establishes a reference called "specification" to the inner array in specification_container), everything works exactly as intended.
		// TODO: Something's fishy here, bug report this.
		const spec_element_t (&specification)[decltype(specification_container)::length] = specification_container.data;
		constexpr size_t spec_length = decltype(specification_container)::length - 1;	// NOTE: Accounting for null-termination character. We could remove it, but for simplicity's sake, we're leaving it in the actual array, at least for now.

		size_t i = 0;
		size_t result = 0;
		bool is_inside_bracket_expression = false;

		// NOTE: I wanted to make this consteval instead of constexpr, but for some reason (which smells strongly of a compiler bug, TODO: REPORT!!!!) consteval doesn't work, even though both versions are executed at compile-time in this case.
		auto func_implementation = [&i, &result, &is_inside_bracket_expression](size_t nesting_depth, const auto& self) constexpr -> void {
			for (; i < spec_length; i++) {
				char character = specification[i];
				switch (character) {
				case '|':
					if (i == 0) { report_error(R"(moken spec syntax error: alternation ("|") must be preceeded by something)"); }
					if (i == spec_length - 1) { report_error(R"(moken spec syntax error: alternation ("|") must be succeeded by something)"); }
					if (is_inside_bracket_expression) { report_error(R"(moken spec syntax error: alternation ("|") invalid inside bracket expression)"); }
					switch (specification[i - 1]) {
					case '|': report_error(R"(moken spec syntax error: two alternations ("|") cannot be separated by nothing)");
					case '(': report_error(R"(moken spec syntax error: alternation ("|") cannot be preceeded by an opening parenthesis ("("))");
					}
					break;
				case '*':
					if (i == 0) { report_error(R"(moken spec syntax error: kleene closure ("*") must be preceeded by something)"); }
					if (is_inside_bracket_expression) { report_error(R"(moken spec syntax error: kleene closure ("*") invalid inside bracket expression)"); }
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
					switch (specification[i - 1]) {
					case '[': report_error(R"(moken spec syntax error: bracket expression ("[...]") cannot be empty)");
					}
					is_inside_bracket_expression = false;
					result++;
					break;
				case '(':
					if (i == spec_length - 1) { report_error(R"(moken spec syntax error: opening parenthesis ("(") must be succeeded by something)"); }
					if (is_inside_bracket_expression) { report_error(R"~(moken spec syntax error: subexpression ("(...)") invalid inside bracket expression ("[...]"))~"); }
					i++;
					self(nesting_depth + 1, self);
					break;
				case ')':
					if (nesting_depth == 0) { report_error(R"~(moken spec syntax error: closing parenthesis (")") is invalid without corresponding opening parenthesis ("("))~"); }
					if (i == 0) { report_error(R"~(moken spec syntax error: subexpression ("(...)") cannot be empty)~"); }
					return;
				case '\\':
					if (i == spec_length - 1) { report_error(R"(moken spec syntax error: escape character ("\") must be succeeded by something)"); }
					switch (specification[i + 1]) {
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
					// TODO: Give this code another look through.
				}
			}
		};
		func_implementation(0, func_implementation);

		return result;
	}

	enum class token_type_t : uint8_t {
		TABLE_ROW,
		ALTERNATION,
		KLEENE_CLOSURE,
		SUBEXPRESSION_BEGIN,
		SUBEXPRESSION_END
	};

	struct token_t {
		token_type_t type;
		bool table_row[table_width];
	};

	template <array_container_t specification_container>
	consteval size_t calculate_token_array_length() {
		using spec_element_t = typename decltype(specification_container)::type;
		// NOTE: Same work-around as above.
		const spec_element_t (&specification)[decltype(specification_container)::length] = specification_container.data;
		constexpr size_t spec_length = decltype(specification_container)::length - 1;	// NOTE: Accounting for null-termination character. We could remove it, but for simplicity's sake, we're leaving it in the actual array, at least for now.

		size_t result = 0;

		for (size_t i = 0; i < spec_length; i++) {
			spec_element_t character = specification[i];		// TODO: You need to use a type traits make unsigned type of thing here, or else UB when using it as array index.

			switch (character) {
			case '|': result++; break;
			case '*': result++; break;
			case '(': result++; break;
			case ')': result++; break;
			case '[':			// TODO: We're using equalities with normal character values in the switch statement, even though the spec_element_t is pretty general. Are there any cases where this wouldn't work? What should we do?
				result++;
				i++;
				for (; specification[i] != ']'; i++) { }
				break;
			default: result++; break;
			}
		}

		// TODO: Make sure all these mechanisms handle 0 length regex specifications correctly and specifications like "()" and stuff like that.
		return result;
	}

	template <array_container_t specification_container>
	consteval auto tokenize_specification() {
		using spec_element_t = typename decltype(specification_container)::type;
		// NOTE: Same work-around as above.
		const spec_element_t (&specification)[decltype(specification_container)::length] = specification_container.data;
		constexpr size_t spec_length = decltype(specification_container)::length - 1;	// NOTE: Accounting for null-termination character. We could remove it, but for simplicity's sake, we're leaving it in the actual array, at least for now.

		array_container_t<token_t, calculate_token_array_length<specification_container>()> token_array;

		size_t token_array_index = 0;

		for (size_t i = 0; i < spec_length; i++) {
			spec_element_t character = specification[i];		// TODO: You need to use a type traits make unsigned type of thing here, or else UB when using it as array index.

			switch (character) {
			case '|': token_array[token_array_index++].type = token_type_t::ALTERNATION; break;
			case '*': token_array[token_array_index++].type = token_type_t::KLEENE_CLOSURE; break;
				  // TODO: Redo kleene closure stuff to give token spans instead of actual kleene closure token, like below.
			case '(': token_array[token_array_index++].type = token_type_t::SUBEXPRESSION_BEGIN; break;
			case ')': token_array[token_array_index++].type = token_type_t::SUBEXPRESSION_END; break;
			case '[':
				token_t& token = token_array[token_array_index++];
				token.type = token_type_t::TABLE_ROW;
				parse_bracket_expression<specification_container>(++i, token.table_row);
				break;
				// TODO: add the backslash stuff
			default:
				token_t& token = token_array[token_array_index++];
				token.type = token_type_t::TABLE_ROW;
				for (size_t j = 0; j < character; j++) { token.table_row[j] = false; }
				token.table_row[character] = true;
				for (size_t j = character + 1; j < table_width; j++) { token.table_row[j] = false; }
				break;
			}
		}

		return token_array;
	}
	
	struct dfa_table_element_t {
		const dfa_table_element_t *next;
	};

	consteval void superimpose_table_row(dfa_table_element_t (&table)[table_length], const bool (&row)[table_width], size_t current_row, size_t (&new_current_rows)[table_width], size_t table_head_row) {
		for (size_t i = 0; i < table_width; i++) {
			dfa_table_element_t& element = table[current_row * table_width + i];
			if (element.next == nullptr) {
				element.next = table_head_row;
				new_current_rows[i] = table_head_row;
				continue;
			}
			new_current_rows[i] = element.next;
		}
	}

	template <array_container_t token_array_container, size_t table_length>
	consteval void fill_dfa_table_relatively(dfa_table_element_t (&table)[table_length]) {
		// NOTE: Same work-around as above.
		const token_t (&token_array)[decltype(token_array_container)::length] = token_array_container.data;
		constexpr size_t token_array_length = decltype(token_array_container)::length;

		size_t token_array_index = 0;		// TODO: Put this back in as parameter to the function or else none of this is gonna work.

		// NOTE: constexpr instead of consteval, for same reason as above.
		auto func_implementation = [&token_array_index](size_t current_row, size_t superimposition_target_row, const auto& self) constexpr -> bool {
			for (; token_array_index < token_array_length; token_array_index++) {
				token_t token = token_array[token_array_index];
				switch (token.type) {
				case token_type_t::ALTERNATION: return true;
				case '(':
					token_array_index++;
					while (self(current_row, self)) { }
					break;
				case ')':
					return false;
				case token_type_t::TABLE_ROW:
					// TODO: research order of execution with = operator. The stuff on the left gets evaluated first right, and then the stuff on the right? Very sure that's how it works.

					size_t new_current_rows[table_width];

					superimpose_table_row(table, token.table_row, current_row, new_current_rows, superimposition_target_row);

					for (size_t j = 0; j < table_width - 1; j++) {
						func_implementation(new_current_rows[i], kleene_base, self);
						// TODO: Keep this from going exponential by implementing a grouping algorithm.
					}
					return func_implementation(new_current_rows[i], kleene_base, self);

				case token_type_t::KLEENE_CLOSURE_TABLE_ROW:

					size_t new_current_rows[table_width];

					if (superimpose_table_row(table, token.table_row, current_row, new_current_rows, superimposition_target_row) == true) { return false; }
					// TODO: Add KLEENE_CLOSURE_TABLE_BEGIN and END type tokens. They should set the base and then start the process of connecting to the base respectively.
					// Maybe don't have KLEENE_CLOSURE_TABLE_ROW and the END things, and simply set a bool when START is encountered. That bool will be set back to false once an escape route back to the kleene base is found.
					// After connecting to the kleene base, we return, and the KLEENE START token thing should catch that and then skip to the next relevant tokens using the return value that's propagated back up.
					// A surrounding kleene closure bool and kleene base can then be used, since it's stored on that level of the stack, and the process can be done again if necessary. This makes kleenes nestable, which is imperative.

					for (size_t j = 0; j < table_width - 1; j++) {
						func_implementation(new_current_rows[i], kleene_base, self);
						// TODO: Keep this from going exponential by implementing a grouping algorithm.
					}
					return func_implementation(new_current_rows[i], kleene_base, self);
				}
			}
			return false;
		};
		while (func_implementation(0, 0, func_implementation) { }
	}

	template <array_container_t specification>
	struct relative_untrimmed_tokenizer_t {
		static constexpr size_t table_length = calculate_max_table_length<specification>();
		dfa_table_element_t table[table_length * (unsigned char)-1];

		consteval relative_untrimmed_tokenizer_t() {
			constexpr auto token_array = tokenize_specification<specification>();
			fill_dfa_table_relatively<token_array>(table);
		}
	};

	template <size_t table_length>
	class tokenizer_t {
	public:
		dfa_table_element_t table[table_length];

		tokenizer_t() = delete;
	};

	template <array_container_t specification>
	consteval auto make_tokenizer_t() noexcept {
		relative_untrimmed_tokenizer_t<specification> relative_untrimmed;
		return nullptr;	// TODO
	}

}
