#include "moken.h"
#include <iostream>

int main() {
	moken::make_tokenizer_t<moken::array_container_t("(hi|((bye)))*end")>();
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
}
