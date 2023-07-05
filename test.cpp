#include "moken.h"

int main() {
	moken::make_tokenizer_t<"(hi|((bye)))*end">();
}
