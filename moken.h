#pragma once

namespace moken {

	struct dfa_table_element_t {
		const dfa_table_element_t *next;
	};

	template <char... specification, size_t depth>
	struct recursive_dfa_container_t {
		dfa_table_element_t row[128] { };
		recursive_dfa_container_t<specification, depth + 1> dfa;
	}

	template <char... specification, typename std::enable_if<>::type = true>

		// TODO: We've thought through all the different methods that are possible and all of them have restrictions. What we want to do isn't possible. At least not without an upper limit, which we were trying to avoid.
		// TODO: Solution: set an upper limit, but calculate it properly everytime so that it cannot be passed. Maybe a simple character count will suffice, see how you compile .* along with all the other symbols.
		// With upper limit set, simply fill in the stuff with consteval functions and then have a function that returns a cut off version of the table, that removes extra fat.

	template <size_t table_length>
	class tokenizer_t {
	public:
		dfa_table_element_t table[table_length];

		tokenizer_t() = delete;
	};

	template <char... specification>
	consteval auto make_tokenizer_t() noexcept {
		recursive_dfa_container_t<specification> dfa;
		return dfa.to_relative_table();
	}

}
