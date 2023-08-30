





	struct dfa_table_element_t {
		const dfa_table_element_t *next;
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

	template <auto nfa_table_package, uint32_t table_width, size_t dfa_table_length, size_t possible_states_capacity>
	requires nfa_table_c<decltype(nfa_table_package)>
	consteval auto convert_nfa_to_dfa() {
		constexpr auto &nfa_table_container = nfa_table_package.first;
		constexpr auto &nfa_ghost_rows = nfa_table_package.second;

		using instanced_nfa_table_element_t = decltype(nfa_table_container)::type;
		constexpr size_t nfa_table_1d_length = decltype(nfa_table_container)::length;
		constexpr size_t nfa_table_length = nfa_table_1d_length / table_width;

		static_assert(nfa_table_length >= dfa_table_length, "moken bug detected: nfa_table_length is smaller than dfa_table_length in convert_nfa_to_dfa()");

		constexpr instanced_nfa_table_element_t (&nfa_table)[nfa_table_1d_length] = nfa_table_container.data;

		array_container_t<dfa_table_element_t, dfa_table_length * table_width> dfa_table { };
		array_contaienr_t<uint16_t, dfa_table_length> dfa_terminators { };
		size_t dfa_table_head_row = 0;

		constexpr auto superimpose_termination_handler_versions = [
									   &nfa_table,
									   &nfa_ghost_rows
									  ]
									  <
									   size_t vector_capacity
									  >
									  (
									   vector_t<size_t, vector_capacity> &possible_states
									  )
									  consteval
									  -> uint32_t
		{
			vector_t<size_t, vector_capacity> new_possible_states;
			uint32_t result = -1;
			for (const size_t &state : possible_states) {
				if (nfa_ghost_rows[state] == false) { new_possible_states.push_back(state); continue; }
				if (nfa_table[state * table_width].next.length != 0) { new_possible_states.push_back(state); continue; }
				result = nfa_table[state * table_width + 1].next[0];
			}
			possible_states = new_possible_states;
			return result;
		};

		constexpr auto register_dfa_termination_handler = [
								   &dfa_terminators
								  ]
								  (
								   size_t dfa_row,
								   uint16_t termination_handler
								  )
								  consteval
		{
			if (dfa_row >= dfa_table_length) {
				report_error("moken bug detected: invalid dfa_row passed to register_dfa_termination_handler(), out-of-bounds");
			}
			dfa_terminators[dfa_row] = termination_handler;
		};

		constexpr auto follow_ghost_rows = [
						    &nfa_table,
						    &nfa_ghost_rows
						   ]
						   (
						    vector_t<size_t, possible_states_capacity> &possible_states,
						   )
						   consteval
		{
			// NOTE: For myself as future reference, feel free to ignore:
			// decltype(possible_states) doesn't get you the type that the reference is referring to,
			// it gets you the type of the reference. So we can't do ::length before we remove the reference.
			// decltype is more complex than it looks, the following is true, but there are more edge-cases
			// that you can find in the actual documentation:
			// 	1. decltype(id_expression) when id_expression refers to an entity:
			//		--> returns type of entity. This is reasonable since the type of the entity
			//		is what you want most of the time. The actual evaluation of the id_expression is an lvalue ref,
			//		but the type of the entity is given to you.
			//		--> If it's an id_expression that doesn't refer to a valid entity, the program is ill-formed.
			//	2. decltype(expression):
			//		--> When it's not an id_expression, the type of expression is given to you.
			//	3. You can always do decltype((id_expression/expression)) in order to parse the contained expression as
			//	a normal expression, which won't do that entity stuff that I talked about above.
			//		--> Interestingly, this probably isn't an explicitly programmed feature in the compiler.
			//		Rather, the evaluation of (id_expression) probably simply returns an lvalue ref typed normal expression,
			//		as per the parsing rules of the C++ parser, and since it's a normal expression and it's not identifiable
			//		as an id_expression anymore, that entity stuff isn't done.
			//	4. expressions that are prvalues (means pure rvalues AFAIK) have special handling:
			//		--> The rvalue ref is removed and you get the normal type. As far as I can tell,
			//		this is simply done because that's the behavior that's the most useful in most cases.
			//		For example, it's more practical that decltype((2 + 0.1f)) give you float instead of float&&.
			//	--> Note that regular rvalues aren't handled like that. Those are returned as-is, which makes sense.

			for (minimum_unsigned_integral_t<possible_states_capacity> i = 0;
			     i < std::remove_reference_t<decltype(possible_states)>::length;
			     i++)
			{
				size_t state = possible_states[i];
				if (nfa_ghost_rows[state] == true) {
					if (nfa_table[state * table_width].next.length == 0) { continue; }
					possible_states.pluck(i);
					if (possible_states.push_back(nfa_table[state * table_width].next) == false) { return false; }
					i--;
					continue;
				}
			}

			return true;
		};

		// NOTE: If any of possible_states are ghost rows, this function will throw an error.
		constexpr auto superimpose_element_next_vector_versions = [
									   &nfa_table
									  ]
									  (
									   const vector_t<size_t, possible_states_capacity> &possible_states,
									   size_t row_index
									  )
									  consteval
									  -> std::pair<size_t, vector_t<size_t, possible_states_capacity>>
		{
			vector_t<size_t, possible_states_buffer_capacity> result;
			for (size_t state : possible_states) {
				if (nfa_ghost_rows[state] == true) {
					report_error("moken bug detected: superimpose_element_next_vector_versions() called with ghost row/s in possible_states");
				}
				auto next_states = nfa_table[state * table_width + row_index].next;
				if (follow_ghost_rows(next_states) == false) { return { false, result }; }
				if (result.push_back(next_states) == false) {
					result.remove_duplicates();
					if (result.push_back(next_states) == false) { return { false, result }; }
				}
			}
			result.remove_duplicates();	// TODO: implement this
			return { true, result };
		};

		constexpr auto superimpose_element_versions = [
							       &nfa_table = std::as_const(nfa_table),
							       &nfa_ghost_rows = std::as_const(nfa_ghost_rows)
							      ]
							      <
							       size_t vector_1_capacity,
							       size_t vector_2_capacity
							      >
							      (
							       const vector_t<size_t, vector_1_capacity> &possible_states,
							       vector_t<size_t, vector_2_capacity> &target_buffer,
							       minimum_unsigned_integral_t<table_width> element_in_row
							      )
							      consteval
		{
			for (const size_t &state : possible_states) {
				if (nfa_ghost_rows[state] == true) {
					if (nfa_table[state * table_width].next.length == 0) {
						report_error("moken bug detected: superimpose_element_versions() called with terminator state/s in possible_states");
					}
					report_error("moken bug detected: superimpose_element_versions() called with non-terminator ghost row state/s in possible_states");
				}
				if (target_buffer.push_back(nfa_table[state * table_width + element_in_row].next) == false) {
					target_buffer.sort_and_remove_duplicates();
					if (target_buffer.push_back(nfa_table[state * table_width + element_in_row].next) == false) { return false; }
				}
			}
			target_buffer.sort_and_remove_duplicates();
			return true;
		};

		constexpr auto implementation = [&]
						(
						 size_t dfa_row,
						 vector_t<size_t, possible_states_capacity> &possible_states,
						 const auto &self
						)
						consteval
						-> bool
		{
			if (follow_ghost_rows(possible_states) == false) { return false; }
			// TODO: Have the below remove the temination ghost rows from the thing as well, so the following code doesn't mess up.
			uint32_t termination_handler = superimpose_termination_handler_versions(possible_states);
			// TODO: make sure the conversions do what you want. is it it unsigned->signed->expand or unsigned->expand->signed, cuz that changes things
			if (termination_handler != -1) { register_dfa_termination_handler(dfa_row, termination_handler); }

			size_t one_child_target_dfa_row;
			{
				auto possible_states_copy = possible_states;

				size_t finished_elements[table_width] { (size_t)-1 };

				{
					array_container_t<vector_t<size_t, possible_states_capacity>, table_width> possible_states_for_every_element;
					for (size_t i = 0; i < table_width; i++) {
						auto &possible_states_of_element = possible_states_for_every_element[i];
						superimpose_element_versions(possible_states, possible_states_of_element, i);
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

				for (minimum_unsigned_integral_t<table_width> i = 1; i < table_width; i++) {
					if (finished_elements[i] != -1 && finished_elements[i] != finished_elements[0]) { goto for_check_failed; }
				}
				// NOTE: This jump is necessary because you can't conditionally end a scope in C++.
				// In almost all situations, not being able to is fine since the only thing you would do after
				// ending the scope is either jumping out to other code that doesn't rely on the scope or returning out
				// of the function. In both situations, the scope is ended by the compiler anyway, so ending it by hand
				// is useless.
				// I can think of a very small handful of situations where conditionally ending the scope could come in handy and allow you
				// to do things you otherwise wouldn't be able to do.
				// This normally isn't one of those situations, but as far as I'm aware it's only because of compiler optimizations
				// that are normally applied.
				// If you call a function and return directly after:
				// 1. tail call optimization is applied
				// 2. If that doesn't happen for some reason, the stack should at least be cleaned up (which means scope ends),
				// before doing the function call.
				// 3. But none of that is technically guaranteed, those are just optimizations.
				// Although it would be stupid, the compiler could recurse with a big stack in this situation.
				// I would never expect it to happen at runtime, but I don't want to take the risk of a barely-optimizing
				// compile-time interpreter messing things up at compile-time, as could be the case here.
				// So I've jumped out of the scope before recursing, to ensure that the stack is at least smaller,
				// even if I can't ensure that it completely goes away before the call.
				// This incurs a jump, which wouldn't be necessary if I could conditionally end the scope, so it's not the ideal solution,
				// but I've got no better options AFAIK.

				// TODO: Yeah I now know that you can't goto in consteval functions, which is stupid.
				// Work around it with a flag and some ifs.

				one_child_target_dfa_row = finished_elements[0];
				goto one_child_recurse;
for_check_failed:

				self(finished_elements[0], possible_states, self);

				size_t last_dfa_row = 0;
				for (minimum_unsigned_t<table_width> i = 1; i < table_width; i++) {
					if (finished_elements[i] == -1 || finished_elements[i] <= last_dfa_row) { continue; }
					last_dfa_row = finished_elements[i];
					superimpose_element_versions(possible_states_copy, possible_states, i);
					self(finished_elements[i], possible_states, self);
				}
			}
one_child_recurse:
			return self(one_child_target_dfa_row, possible_states, self);
		};
		vector_t<size_t, possible_states_capacity> possible_states;
		if (possible_states.push_back(0) == false) {
			report_error("moken bug detected: possible_states vector capacity blown in convert_nfa_to_dfa() while seeding");
		}
		implementation(create_new_dfa_row(), possible_states, implementation);

		return std::pair(dfa_table, dfa_terminators);
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

