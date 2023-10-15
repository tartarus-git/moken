






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

