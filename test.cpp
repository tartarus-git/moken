#include "moken.h"
#include <iostream>

int main() {
	constexpr auto dfa_table = moken::make_tokenizer_t<moken::array_container_t("(hi|((bye)))*end")>();
	/*for (auto token : token_array) {
		switch (token.type) {
		case moken::token_type_t::ALTERNATION: std::cout << "alternation\n"; break;
		case moken::token_type_t::SUBEXPRESSION_BEGIN: std::cout << "subexpression begin\n"; break;
		case moken::token_type_t::SUBEXPRESSION_END: std::cout << "subexpression end\n"; break;
		case moken::token_type_t::KLEENE_CLOSURE_BEGIN: std::cout << "kleene closure begin\n"; break;
		case moken::token_type_t::KLEENE_CLOSURE_END: std::cout << "kleene closure end\n"; break;
		case moken::token_type_t::TABLE_ROW: std::cout << "table row\n"; break;
		}
	}*/

	/*
	for (size_t i = 0; i < decltype(nfa_table.first)::length; i += decltype(nfa_table.first)::length / decltype(nfa_table.second)::length) {
		for (size_t j = i; j < i + decltype(nfa_table.first)::length / decltype(nfa_table.second)::length; j++) {
			std::cout << " {";
			for (size_t k = 0; k < decltype(nfa_table.first)::type::next_vector_capacity; k++) {
				std::cout << ":" << nfa_table.first[j].next[k] << ":";
			}
			std::cout << "} ";
		}
		std::cout << '\n';
	}

	for (size_t i = 0; i < decltype(nfa_table.second)::length; i++) {
		std::cout << nfa_table.second[i] << '\n';
	}
	*/
}
