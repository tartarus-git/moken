#pragma once

#include <cstddef>
#include <cstdint>
#include <utility>
#include <tuple>
#include <concepts>
#include <type_traits>
// TODO: Check to make sure we need all these headers
#include <algorithm>

namespace moken {

	void report_error(const char *message) noexcept;

	template <typename U, typename V>
	class are_types_same {
	public:
		static constexpr bool value = false;
		// NOTE: We don't actually ever use this bool conversion functionality AFAIK,
		// but it's in here for completeness. I see the value in keeping the code short and cutting out fluff,
		// because it's all in a header, but I chose to opt for completeness because I find it more elegant
		// and the extra space usage doesn't seem like it'll be a problem in this case.
		// TO READERS: Hello, thx for taking a look at my code, if you feel we should strip some of
		// the unnecessary stuff out of here, post an issue, we'll talk.
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

	template <typename T>
	using remove_const_and_ref_t = std::remove_const_t<std::remove_reference_t<T>>;

	template <auto value>
	inline constexpr bool is_at_least_one_v = (value >= 1);

	template <typename element_t, size_t array_length>
	requires is_at_least_one_v<array_length>
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
	concept minimal_container_c = requires(T instance) { *(instance.begin()); };

	template <minimal_container_c T>
	using container_element_t = std::remove_reference_t<decltype(*((*(const T*)nullptr).begin()))>;

	template <typename T>
	concept convertible_to_compile_time_array_c = requires(T instance, std::remove_reference_t<decltype(instance.begin())> iterator) {
		requires same_as_c<std::remove_reference_t<decltype(instance.end())>, decltype(iterator)>;
		requires same_as_c<std::remove_reference_t<decltype(iterator++)>, decltype(iterator)>;
		requires same_as_c<std::remove_reference_t<decltype(++iterator)>, decltype(iterator)>;
		// NOTE: We're not using { x } -> y format here because the x is subject to almost the same rules as decltype,
		// which means that we weren't getting the types that we wanted in most of the cases, because there were added refs.
		// We're doing it like this because we have to remove those refs to make a good comparison.
		// NOTE: x behaves as if decltype((x)), which means it doesn't do that id-expression stuff.
		// TODO: figure out what the reasoning behind that is
		requires is_constant_expression([]() { T::length; });
	};

	template<typename lambda_t, char=(lambda_t{}(), 0)>
	constexpr bool is_constant_expression(lambda_t) { return true;  }
	constexpr bool is_constant_expression(...)      { return false; }

	template <typename T>
	concept vector_convertible_to_compile_time_array_c = requires(T instance, std::remove_reference_t<decltype(instance.begin())> iterator) {
		requires same_as_c<std::remove_reference_t<decltype(instance.end())>, decltype(iterator)>;
		requires same_as_c<std::remove_reference_t<decltype(iterator++)>, decltype(iterator)>;
		requires same_as_c<std::remove_reference_t<decltype(++iterator)>, decltype(iterator)>;
		requires is_constant_expression([]() { T::capacity; });
		// NOTE: At first thought, when converting vectors to arrays, the length member variable inside vector_t might seem
		// like a more logical choice for this. But that doesn't work because we are accepting the source container as a function
		// argument and as such cannot use it's non-constexpr member variables in constant-expressions, as would be the case
		// if we were to try to use it as the length of an array_container_t.
		// Instead, we have to use the capacity, since that is a constant expression. We'll simply only transfer up to length.
		requires std::integral<decltype(instance.length)>;
		// I suppose in cases like this, you have to be super careful that a given type doesn't fall into both concepts,
		// because then the compiler won't know which (for example) constructor to use.
		// I know there are precedence rules but I don't think any of them would apply here (if we removed the instance.length line).
		// I suppose in that case we would have to add extra code to the concepts to make them mutually exclusive,
		// by determinizing any ambigious cases.
		// We don't have to go through the trouble in this case though, because the fact that one concept expects length to be static
		// and another expects it to be instanced means that there are no ambigious cases that match both concepts.
	};

	template <typename source_t, typename target_t>
	concept implicitly_convertible_to_x_type_c = requires(source_t source_instance) { [](target_t){}(source_instance); };

	template <typename T>
	concept sortable_element_c = requires(T first, T second) {
		{ first < second } -> same_as_c<bool>;
	};

	template <typename T>
	concept equatable_element_c = requires(T first, T second) {
		{ first == second } -> same_as_c<bool>;
	};

	/*
	template <typename = void>
	struct does_uint8_t_exist { static constexpr bool value = false; };
	template <>
	struct does_uint8_t_exist<std::void_t<uint8_t>> { static constexpr bool value = true; };
	template <typename = void>
	struct does_uint16_t_exist { static constexpr bool value = false; };
	template <>
	struct does_uint16_t_exist<std::void_t<uint16_t>> { static constexpr bool value = true; };
	template <typename = void>
	struct does_uint32_t_exist { static constexpr bool value = false; };
	template <>
	struct does_uint32_t_exist<std::void_t<uint32_t>> { static constexpr bool value = true; };
	template <typename = void>
	struct does_uint64_t_exist { static constexpr bool value = false; };
	template <>
	struct does_uint64_t_exist<std::void_t<uint64_t>> { static constexpr bool value = true; };
	*/

	// TODO: Write a note about the trick with the if constexpr and the return value type because it's a pretty cool trick.
	template <size_t highest_reachable_num>
	consteval auto minimum_unsigned_integral_t_impl() {
		if constexpr (highest_reachable_num <= (uint8_t)-1) { return (uint8_t)0; }
		else if constexpr (highest_reachable_num <= (uint16_t)-1) { return (uint16_t)0; }
		else if constexpr (highest_reachable_num <= (uint32_t)-1) { return (uint32_t)0; }
		else if constexpr (highest_reachable_num <= (uint64_t)-1) { return (uint64_t)0; }
		else {
			static_assert(highest_reachable_num != highest_reachable_num, "moken bug detected: minimum_unsigned_integral_t_impl() somehow failed to find a usable type");
		}
	}

	template <size_t highest_reachable_num>
	using minimum_unsigned_integral_t = decltype(minimum_unsigned_integral_t_impl<highest_reachable_num>());

	template <typename source_integral_t>
	struct next_larger_integral_t_impl { };
	template <>
	struct next_larger_integral_t_impl<int8_t> { using type = int16_t; };
	template <>
	struct next_larger_integral_t_impl<int16_t> { using type = int32_t; };
	template <>
	struct next_larger_integral_t_impl<int32_t> { using type = int64_t; };
	template <>
	struct next_larger_integral_t_impl<uint8_t> { using type = uint16_t; };
	template <>
	struct next_larger_integral_t_impl<uint16_t> { using type = uint32_t; };
	template <>
	struct next_larger_integral_t_impl<uint32_t> { using type = uint64_t; };

	template <typename T>
	using next_larger_integral_t = typename next_larger_integral_t_impl<T>::type;

	// NOTE: This deduction should technically be taken care of by an implicitly generated deduction guide,
	// making my explicit version unnecessary. I don't know why that's not happening. All I know is my code doesn't compile
	// without this.
	// TODO: compiler bug?
	template <typename element_t, size_t array_length>
	array_container_t(const element_t (&)[array_length]) -> array_container_t<element_t, array_length>;

	// NOTE: Converting from source containers is well and good, but if the source container
	// is another array_container_t, then we oughta use the default copy constructor, because I assume that's
	// potentially much faster at compile-time.
	template <convertible_to_compile_time_array_c source_container_t>
	requires (!is_array_container_t_v<source_container_t>)
	array_container_t(const source_container_t &source_container)
	-> array_container_t<std::remove_reference_t<decltype(*(source_container.begin()))>, source_container_t::length>;
	// NOTE: We gotta do the remove_reference_t stuff because that decltype is gonna give us element_t& instead of element_t, which
	// we can't use in the way that we want to. So we have to remove the reference first before inputting it into the template argument.

	template <vector_convertible_to_compile_time_array_c source_vector_container_t>
	array_container_t(const source_vector_container_t &source_vector_container)
	-> array_container_t<std::remove_reference_t<decltype(*(source_vector_container.begin()))>, source_vector_container_t::capacity>;

	template <typename element_t, size_t array_length>
	requires is_at_least_one_v<array_length>
	class array_container_t {
	public:
		using type = element_t;
		using storage_size_t = minimum_unsigned_integral_t<array_length>;
		static constexpr storage_size_t length = array_length;
		using loose_storage_size_t = next_larger_integral_t<storage_size_t>;

		element_t data[length] { };	// TODO: Fix those { } instances in all the functions because those don't value initialize. Just default construct there.

		consteval array_container_t() = default;

		consteval array_container_t(const element_t &default_element_value) {
			for (element_t &element : data) { element = default_element_value; }
		}

		// NOTE: I wanted to use std::initializer_list to enable aggregate initialization for this class.
		// But that's impossible unless the size of data is fixed. It seems std::initializer_list isn't
		// as useful as I thought. They oughta make a better system.
		// Here's a thought: What if aggregate initialization simply mapped to the below constructor?
		// That would be fine right?

		consteval array_container_t(const element_t (&source_array)[array_length]) {
			for (size_t i = 0; i < length; i++) { data[i] = source_array[i]; }
		}

		// NOTE: As said above, this doesn't double as the copy constructor, we use the default one.
		template <convertible_to_compile_time_array_c source_container_t>
		requires (!is_array_container_t_v<source_container_t>)
		consteval array_container_t(const source_container_t &source_container) {
			size_t i = 0;
			for (const element_t &element : source_container) { data[i++] = element; }
		}

		// NOTE: For instantiating from vector-like containers, array length is vector capacity,
		// but we only fill the array up to vector length amount of units.
		template <vector_convertible_to_compile_time_array_c source_vector_container_t>
		consteval array_container_t(const source_vector_container_t &source_vector_container) {
			size_t i = 0;
			for (const element_t &element : source_vector_container) { data[i++] = element; }
		}

		consteval const array_container_t& operator=(const element_t (&source_array)[length]) const {
			for (size_t i = 0; i < length; i++) { data[i] = source_array[i]; }
			return *this;
		}

		consteval array_container_t& operator=(const element_t (&source_array)[length]) {
			((const array_container_t*)this)->operator=(source_array);
			return *this;
		}

		template <convertible_to_compile_time_array_c source_container_t>
		requires implicitly_convertible_to_x_type_c<container_element_t<source_container_t>, element_t> &&
		(source_container_t::length <= length)
		consteval void overlay_other_container(const source_container_t &source_container) {
			element_t *ptr = data;
			for (const container_element_t<source_container_t> &source_element : source_container) { *(ptr++) = source_element; }
		}

		template <convertible_to_compile_time_array_c source_container_t>
		requires implicitly_convertible_to_x_type_c<container_element_t<source_container_t>, element_t> &&
		(source_container_t::length == length)
		consteval const array_container_t& operator=(const source_container_t &source) const {
			element_t *ptr = data;
			// TODO: Get these functions to show themselves as compilation errors, and then remove them
			// const operator= is stupid.
			for (const container_element_t<source_container_t> &source_element : source) { *(ptr++) = source_element; }
			return *this;
		}

		template <convertible_to_compile_time_array_c source_container_t>
		requires implicitly_convertible_to_x_type_c<container_element_t<source_container_t>, element_t> &&
		(source_container_t::length == length)
		consteval array_container_t& operator=(const source_container_t &source) {
			((const array_container_t*)this)->operator=(source);
			return *this;
		}

		template <vector_convertible_to_compile_time_array_c source_vector_container_t>
		requires implicitly_convertible_to_x_type_c<container_element_t<source_vector_container_t>, element_t> &&
		(source_vector_container_t::capacity <= length)
		consteval const array_container_t& operator=(const source_vector_container_t &source_vector) const {
			element_t *ptr = data;
			for (const container_element_t<source_vector_container_t> &source_element : source_vector) { *(ptr++) = source_element; }
			return *this;
		}

		template <vector_convertible_to_compile_time_array_c source_vector_container_t>
		requires implicitly_convertible_to_x_type_c<container_element_t<source_vector_container_t>, element_t> &&
		(source_vector_container_t::capacity <= length)
		consteval array_container_t& operator=(const source_vector_container_t &source_vector) {
			((const array_container_t*)this)->operator=(source_vector);
			return *this;
		}

		// NOTE: If you need sorted vector, add template parameter that changes the member function behavior for finding and
		// inserting. Then you can create sorted and unsorted vectors on instantiation. Best way as far as I can see for this situation.
		consteval loose_storage_size_t find(const element_t &target, storage_size_t begin_index, storage_size_t end_index)
		requires equatable_element_c<element_t>
		{
		if (end_index >= length) { report_error("moken bug detected: array_container_t::find called with end_index >= length"); }
		if (end_index < begin_index) { report_error("moken bug detected: array_container_t::find called with end_index < begin_index"); }
		if (begin_index >= length) { report_error("moken bug detected: array_container_t::find called with begin_index >= length"); }
			for (storage_size_t i = begin_index; i < end_index; i++) {
				if (target == (*this)[i]) { return i; }
			}
			return -1;
		}
		consteval loose_storage_size_t find(const element_t &target)
		requires equatable_element_c<element_t>
		{ return find(target, 0, length); }

		// NOTE: The following functions are constexpr so that you can use them from runtime as well.
		// Useful for the table jumper algorithm that uses the table at runtime and also for debugging.

		constexpr element_t& operator[](size_t index) noexcept { return data[index]; }
		constexpr const element_t& operator[](size_t index) const noexcept { return data[index]; }

		using iterator_t = element_t*;
		using const_iterator_t = const element_t*;

		constexpr iterator_t begin() noexcept { return data; }
		constexpr const_iterator_t begin() const noexcept { return data; }

		constexpr iterator_t end() noexcept { return data + length; }
		constexpr const_iterator_t end() const noexcept { return data + length; }
	};

	// NOTE: This whole compatible/sufficiently_compatible business is basically just this:
	// compatible array containers are array_container_t's with unsigned integral elements that are smaller than 3 bytes (uint8s and uint16s).
	// sufficiently compatible array containers are the same, but the elements don't have to be unsigned.
	// The interface between library and user accepts sufficiently compatible array containers and then internally converts
	// them to compatible ones, because we need unsigned stuff all over the place for indexing into tables and such.
	// The reason we go to all this trouble is because it's implementation defined whether or not the type char is signed or unsigned,
	// and if it's signed we can't safely rely on it as an array index without converting to unsigned first.
	// This system allows us to handle string literals as inputs to template parameters properly.
	// I know that we could enforce all this directly inside the constructor for array_container_t and probably use less code to do it,
	// but array_container_t is supposed to be a general-purpose structure, and I think it's more elegant to enforce this stuff from
	// outside of the class, rather than compromise array_container_t's purpose.

	template <typename T>
	concept compatible_array_container_element_t_c = std::unsigned_integral<T> && (sizeof(T) <= 2);

	template <typename array_container_type>
	concept compatible_array_container_t_c = array_container_t_c<array_container_type>
		&& compatible_array_container_element_t_c<typename array_container_type::type>;

	template <typename T>
	concept sufficiently_compatible_array_container_element_t_c = std::integral<T> && (sizeof(T) <= 2);

	template <typename array_container_type>
	concept sufficiently_compatible_array_container_t_c = array_container_t_c<array_container_type>
		&& sufficiently_compatible_array_container_element_t_c<typename array_container_type::type>;

	template <array_container_t array_container> requires sufficiently_compatible_array_container_t_c<decltype(array_container)>
	consteval auto convert_sufficiently_compatible_array_container_to_compatible_array_container() {
		array_container_t<std::make_unsigned_t<typename decltype(array_container)::type>, decltype(array_container)::length> result;
		for (size_t i = 0; i < decltype(result)::length; i++) {
			result[i] = array_container[i];
		}
		return result;
	}

	enum class spec_type_t : bool {
		EXTRA_NULL,
		NO_EXTRA_NULL
	};

	template <array_container_t specification_container, spec_type_t spec_type = spec_type_t::EXTRA_NULL>
	requires compatible_array_container_t_c<decltype(specification_container)>
	consteval void check_specification_syntax() {
		using spec_element_t = typename decltype(specification_container)::type;
		constexpr size_t spec_length = (
						spec_type == spec_type_t::NO_EXTRA_NULL
						? decltype(specification_container)::length
						: decltype(specification_container)::length - 1
					       );
		// NOTE: The following line of code is a work-around for what seems to be a compiler bug.
		// I can't write specification_container[i], clang keeps complaining that i is non-const and can't be used in a constant expression,
		// even though this should be an exception since we're using it from inside
		// a consteval function. I think the compiler must somehow forget the consteval-ness along the way and I think the error is a bug.
		// Further proof is that with the following line (which just establishes a reference called "specification" to the inner array
		// in specification_container), everything works exactly as intended.
		// TODO: Something's fishy here, bug report this.
		constexpr const spec_element_t (&specification)[decltype(specification_container)::length] = specification_container.data;
		// NOTE: constexpr const isn't something you see everyday, it makes sense here though.
		// The constexpr applies to the reference, saying the "value" of the reference (what it points to) is constexpr.
		// The const applies to the data that is being pointed to.
		// "constexpr const type*" is also something that can happen.

		static_assert(spec_length != 0, "moken spec syntax error: specification cannot be empty");

		size_t i = 0;
		bool is_inside_bracket_expression = false;

		// NOTE: I wanted to make this consteval instead of constexpr, but for some reason (which smells strongly of a compiler bug,
		// TODO: REPORT!!!!) consteval doesn't work, even though both versions are executed at compile-time in this case.
		// NOTE: Removing it also works, but only because AFAIK any lambda that can be constexpr is constexpr, as per the C++ standard.
		auto func_implementation = [&](size_t nesting_depth, const auto &self) constexpr -> void {
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

				case '\\':
					if (i == spec_length - 1) { report_error(R"(moken spec syntax error: escape character ("\") must be succeeded by something)"); }
					switch (specification[i + 1]) {
					case '|':	// NOTE: A couple of these don't need an escape character in front of
							// them if you write them in a bracket expression, but we'll allow it anyway for simplicity.
					case '*':
					case '[':
					case ']':
					case '(':
					case ')':
					case '.':
					case '-':	// NOTE: This only needs to be escaped inside bracket expressions, but we'll
							// allow it anywhere for simplicity.
					case '\\':
						break;
					default: report_error(R"(moken spec syntax error: escape character ("\") must be succeeded by metacharacter)");
					}
					break;

				case '.':
				/* FALLTHROUGH */
				default:
					// NOTE: Further syntax checking of bracket expressions is done below.
					if (is_inside_bracket_expression) { break; }
					break;
				}
			}

			if (is_inside_bracket_expression) { report_error(R"(moken spec syntax error: invalid bracket expression ("[...]"), no closing square bracket ("]"))"); }
			if (nesting_depth != 0) { report_error(R"~(moken spec syntax error: invalid subexpression ("(...)"), no closing parenthesis (")"))~"); }

		};
		func_implementation(0, func_implementation);
	}

	enum class token_type_t : uint8_t {
		ALTERNATION,
		KLEENE_CLOSURE_BEGIN,
		KLEENE_CLOSURE_END,
		SUBEXPRESSION_BEGIN,
		SUBEXPRESSION_END,
		TABLE_ROW,
	};

	template <uint32_t table_width>
	struct token_t {
		static constexpr uint32_t row_length = table_width;

		token_type_t type;
		bool table_row[row_length];
	};

	// TODO: See if you can remove the container stuff when the stuff isn't in a lambda
	// TODO: Instead of putting spec type through to here, just create a new specification_container in the caller and pass that,
	// no NULL guaranteed.
	template <array_container_t specification_container, spec_type_t spec_type = spec_type_t::EXTRA_NULL>
	requires compatible_array_container_t_c<decltype(specification_container)>
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
			case '*': result += 2; break;
			case '(': result++; break;
			case ')': result++; break;
			case '[':
				result++;
				i++;
				for (; specification[i] != ']'; i++) { }
				break;
			case '\\': i++;
			/* FALLTHROUGH */
			case '.':
			/* FALLTHROUGH */
			default: result++; break;
			}
		}

		return result;
	}

	template <compatible_array_container_element_t_c specification_container_element_t>
	consteval uint32_t calculate_table_width() { return (uint32_t)(specification_container_element_t)-1 + 1; }

	// NOTE: My bracket expressions stray from POSIX bracket expressions because I find those unnecessarily complicated.
	// That whole group definition syntax with :uppercase: and :lowercase: and such is redundant when you can simply say
	// "A-Z" or "a-z". Also, I remember that the handling of various edge-cases in the syntax was particularly strange.
	// TO READERS: If you've got a problem with this, post an issue, we can talk.

	// TODO: You probably want to implement the ^ syntax for bracket expressions, because that is very useful.
	// It should only change the following function, so it's not a big deal.

	template <array_container_t specification_container, uint32_t table_width, spec_type_t spec_type = spec_type_t::EXTRA_NULL>
	requires compatible_array_container_t_c<decltype(specification_container)>
	consteval size_t parse_bracket_expression(size_t spec_index, bool (&table_row)[table_width]) {
		using spec_element_t = typename decltype(specification_container)::type;
		// NOTE: Same work-around as above.
		const spec_element_t (&specification)[decltype(specification_container)::length] = specification_container.data;
		constexpr size_t spec_length = (
						spec_type == spec_type_t::NO_EXTRA_NULL
						? decltype(specification_container)::length
						: decltype(specification_container)::length - 1
					       );

		auto add_character = [](bool (&table_row)[table_width], spec_element_t character) consteval {
			bool& row_bool = table_row[character];
			if (row_bool == true) { report_error(R"(moken spec syntax error: a character cannot be directly or indirectly specified more than once in a bracket expression ("[...]"))"); }
			row_bool = true;
		};

		// NOTE: We can be sure there's a corresponding end bracket and at least one character inside of the brackets
		// because of previous syntax checks.
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
				// I've also never seen a C++ compiler stray from ASCII for char encoding, so anything other than
				// ASCII is probably very unlikely. ASCII takes care of us very well in this situation:
				// [A-Z, a-z, 0-9] all work great and subsets of those obviously work great as well.
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

	template <array_container_t specification_container, uint32_t table_width, spec_type_t spec_type = spec_type_t::EXTRA_NULL>
	requires compatible_array_container_t_c<decltype(specification_container)>
	consteval auto tokenize_specification() {
		using spec_element_t = typename decltype(specification_container)::type;
		// NOTE: Same work-around as above.
		const spec_element_t (&specification)[decltype(specification_container)::length] = specification_container.data;
		constexpr size_t spec_length = (
						spec_type == spec_type_t::NO_EXTRA_NULL
						? decltype(specification_container)::length
						: decltype(specification_container)::length - 1
					       );

		using instanced_token_t = token_t<table_width>;

		constexpr size_t token_array_length = calculate_token_array_length<specification_container, spec_type>();
		array_container_t<instanced_token_t, token_array_length> result_container { };
		instanced_token_t (&result)[token_array_length] = result_container.data;

		size_t token_array_index = 0;
		size_t nesting_depth = 0;
		uint16_t current_termination_handler = 0;

		auto insert_token = [
				     &token_array = result,
				     &token_array_head = token_array_index
				    ]
				    (
				     size_t token_array_index,
				     const instanced_token_t& token
				    )
				    consteval
		{
			for (size_t i = token_array_head; i > token_array_index; i--) { token_array[i] = token_array[i - 1]; }
			token_array[token_array_index] = token;
			token_array_head++;
		};

		for (size_t i = 0; i < spec_length; i++) {
			spec_element_t element = specification[i];
			switch (element) {

			case '|':
				result[token_array_index].type = token_type_t::ALTERNATION;
				token_array_index++;
				break;

			case '*':
				{
					result[token_array_index++].type = token_type_t::KLEENE_CLOSURE_END;
					size_t token_backtrack_index = token_array_index - 1;
					bool finished = false;
					switch (result[--token_backtrack_index].type) {
					case token_type_t::SUBEXPRESSION_END:
						{
							size_t nesting_depth = 1;
							// NOTE: Part of avoiding underflow and out-of-bounds array access.
							// See note below.
							token_backtrack_index--;
							while (true) {
								switch (result[token_backtrack_index].type) {
								case token_type_t::SUBEXPRESSION_END: nesting_depth++; break;
								case token_type_t::SUBEXPRESSION_BEGIN:
									nesting_depth--;
									// NOTE: We can't use gotos because the designers
									// made bad decisions. Such is life.
									// We work around it with a flag.
									if (nesting_depth == 0) { finished = true; }
									break;
								}
								if (finished) { break; }
								// NOTE: We do this down here to avoid underflow and subsequent
								// out-of-bounds array indexing when "(" is at beginning of token array.
								token_backtrack_index--;
							}
							break;
						}
					case token_type_t::TABLE_ROW: break;
					// NOTE: Any other cases cannot happen because we've already filtered those
					// constellations out via syntax checks.
					}
					insert_token(token_backtrack_index, { token_type_t::KLEENE_CLOSURE_BEGIN, { } });
					break;
				}

			case '[':
				result[token_array_index].type = token_type_t::TABLE_ROW;
				i = parse_bracket_expression<specification_container, table_width, spec_type>(i, result[token_array_index].table_row);
				token_array_index++;
				break;

			case '(':
				result[token_array_index++].type = token_type_t::SUBEXPRESSION_BEGIN;
				nesting_depth++;
				break;

			case ')':
				result[token_array_index++].type = token_type_t::SUBEXPRESSION_END;
				nesting_depth--;
				break;

			case '.':
				result[token_array_index].type = token_type_t::TABLE_ROW;
				// TODO: Can't do this, write not about how the assignment initializer list is a useless feature,
				// since you can only use it for construction anyway.
				//result[token_array_index].table_row = { true };
				for (uint32_t i = 0; i < table_width; i++) { result[token_array_index].table_row[i] = true; }
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

		return result_container;
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

	template <typename T>
	concept token_array_container_t_c = array_container_t_c<T> && token_t_c<typename T::type>;

	template <array_container_t token_array_container>
	requires token_array_container_t_c<decltype(token_array_container)>
	consteval std::tuple<size_t, size_t, size_t, size_t> calculate_table_metrics() {
		using instanced_token_t = typename decltype(token_array_container)::type;
		constexpr size_t token_array_length = decltype(token_array_container)::length;
		// NOTE: Same work-around as above.
		const instanced_token_t (&token_array)[token_array_length] = token_array_container.data;

		size_t dfa_max_rows = 0;
		size_t nfa_max_rows = 0;

		size_t max_next_vector_capacity = 1;

		size_t current_nested_kleene_closures = 0;
		size_t max_nested_kleene_closures = current_nested_kleene_closures;

		size_t token_array_index = 0;

		auto implementation = [&](const auto &self) consteval -> void {
			size_t current_next_vector_capacity = 1;

			for (; token_array_index < token_array_length; token_array_index++) {
				const instanced_token_t &token = token_array[token_array_index];
				switch (token.type) {

				case token_type_t::ALTERNATION: current_next_vector_capacity++; nfa_max_rows++; break;

				case token_type_t::KLEENE_CLOSURE_BEGIN:
					current_nested_kleene_closures++;
					if (current_nested_kleene_closures > max_nested_kleene_closures) {
						max_nested_kleene_closures = current_nested_kleene_closures;
					}
					break;

				case token_type_t::KLEENE_CLOSURE_END: current_nested_kleene_closures--; nfa_max_rows++; break;

				case token_type_t::SUBEXPRESSION_BEGIN:
					token_array_index++;
					nfa_max_rows += 2;	// NOTE: One ghost row at start, one at end.
					self(self);
					break;

				case token_type_t::SUBEXPRESSION_END:
					if (current_next_vector_capacity > max_next_vector_capacity) {
						max_next_vector_capacity = current_next_vector_capacity;
					}
					nfa_max_rows++;		// NOTE: One ghost row at end of current subpath.
					return;

				case token_type_t::TABLE_ROW: nfa_max_rows++; dfa_max_rows++; break;

				}
			}
		};
		implementation(implementation);

		nfa_max_rows++;		// NOTE: Because of last ghost row needed for last subpath in top-level alternation set.

		return { nfa_max_rows, dfa_max_rows + 50, max_next_vector_capacity, max_nested_kleene_closures };
	}

	// NOTE: Technically, one would expect pushing onto a vector to start the lifetime of an object and
	// popping off of a vector to end the lifetime of an object. Also, one would expect the constructors and destructors
	// to run, respectively. Implementing those things at compile-time is possible, but since I don't have access to
	// malloc, which is what one would normally use in this case, the implementation would be too clunky and
	// incur compile-time performance costs that could potentially be non-negligable.
	// If we just start the lifetime once and never end it, these problems go away.
	// None of the rest of the code relies on constructors/destructors being run or lifetimes starting/ending, so this is fine for this project.
	// It's kind of a shame that this class, which I originally intended to be very general-purpose, is now specialized for
	// this project, but I think it's the best decision in this case.

	// NOTE: Note that the omission of the expected lifetime and constructor/destructor behavior doesn't mean we don't use std::move.
	// When taking things out of the vector, we std::move, because moving (if the move constructor is written by someone
	// who isn't crazy) doesn't rely on destruction of any sort (since moved-from entities get naturally destructed all the time
	// and calling the destructor twice is undefined behavior).
	// We can move from the internal array and turn right back around and copy a new value into the slot.
	// Lifetime never ended, everythings good.

	template <typename element_t, size_t capacity_param>
	class vector_t : private array_container_t<element_t, capacity_param> {
	public:
		static constexpr size_t capacity = capacity_param;

		using typename array_container_t<element_t, capacity>::iterator_t;
		using typename array_container_t<element_t, capacity>::const_iterator_t;

		using array_container_t<element_t, capacity>::data;
		using array_container_t<element_t, capacity>::operator[];
		using array_container_t<element_t, capacity>::begin;

		consteval const_iterator_t end() const { return data + length; }
		consteval iterator_t end() { return data + length; }

		// NOTE: Depending on what these types end up as, the memory layout here could be suboptimal,
		// but there's no way to fix that in current C++ as far as I'm aware.
		// In this case, it's not a big deal at all.
		using typename array_container_t<element_t, capacity>::storage_size_t;
		storage_size_t length = 0;

		using typename array_container_t<element_t, capacity>::loose_storage_size_t;

		consteval vector_t() = default;

		consteval vector_t(const element_t (&source_array)[capacity_param]) {
			length = capacity;
			for (size_t i = 0; i < length; i++) { data[i] = source_array[i]; }
		}

		consteval vector_t(const vector_t<element_t, capacity_param> &other)
		: array_container_t<element_t, capacity>(other), length(other.length)
		{ }

		// TODO: Add vector_t type transmitting constructor for general containers as well.

		template <typename other_element_t, size_t other_capacity>
		requires implicitly_convertible_to_x_type_c<other_element_t, element_t> && (other_capacity <= capacity)
		consteval vector_t(const vector_t<other_element_t, other_capacity> &other)
		: array_container_t<element_t, capacity>(other), length(other.length)
		{ }

		// TODO: Add vector_t type comparing constructor for general containers.

		template <typename other_element_t, size_t other_capacity>
		requires implicitly_convertible_to_x_type_c<other_element_t, element_t> && (other_capacity <= capacity)
		consteval vector_t& operator=(const vector_t<other_element_t, other_capacity> &other) {
			array_container_t<element_t, capacity>::operator=(other);
			length = other.length;
			return *this;
		}

		template <convertible_to_compile_time_array_c source_container_t>
		requires implicitly_convertible_to_x_type_c<container_element_t<source_container_t>, element_t>
		&& (source_container_t::length <= capacity)
		consteval vector_t& operator=(const source_container_t &source) {
			overlay_other_container(source);
			length = source_container_t::length;
			return *this;
		}

		template <typename other_element_t, size_t other_capacity>
			// TODO: Think about if you want this to be a type error or a compile-time value return error situation.
			// It's interesting that you can lower the error to the level that you want it in.
		requires implicitly_convertible_to_x_type_c<element_t, other_element_t>
		|| implicitly_convertible_to_x_type_c<other_element_t, element_t>
		// NOTE: C++20 and above assumes that a == b is true exactly when b == a is true, and as such one operator== function can supply the implementation
		// for both a == b and b == a. Basically, the arguments are also tried in reverse order when finding an operator==.
		// Because of this, you can get ambigiouities. For example if I left out the const at the end of the below line,
		// then both argument orders would have different meanings when matching with this function, and they will both match with this function.
		// Those two meanings have the same precedence, hence ambigiouity. The tie-breaker is to go with the non-reversed order, so everythings
		// fine, but you will get a warning. You really should avoid this warning though because it's more elegant.
		// As far as I can tell, the only reason it's a warning and not an error is to preserve as much backwards-compatibility as possible.
		// Preserving all of it is impossible though. This change in assumption for the == operator introduces a breaking change,
		// which is unfortunate.
		// TODO: Check that note is okay.
		consteval bool operator==(const vector_t<other_element_t, other_capacity> &other) const {
			if (length != other.length) { return false; }
			const_iterator_t other_ptr = other.begin();
			for (const_iterator_t ptr = begin(); ptr < end(); ptr++) {
				if (*ptr != *other_ptr) { return false; }
			}
			return true;
		}

		consteval bool push_back(const element_t &element) {
			if (length == capacity) { return false; }
			if (length > capacity) {
				report_error("moken bug detected: vector_t head somehow got bigger than value for capacity, unexpected");
			}
			data[length++] = element;
			return true;
		}

		// TODO: Make this accept anything that has the necessary properties.
		// Like compatible_container or whatever we used up at the top.
		// Make that one more general by moving the requires condition outside of it and into the usage-location in array_container_t.
		template <size_t other_capacity>
		consteval bool push_back(const vector_t<element_t, other_capacity> &other) {
			for (minimum_unsigned_integral_t<other_capacity> i = 0; i < other.length; i++) {
				if (push_back(other[i]) == false) { length -= i; return false; }
			}
			return true;
		}

		consteval element_t pop_back() { return std::move(data[--length]); }

		consteval element_t pluck(storage_size_t index) {
			element_t result = std::move(data[index]);	// NOTE: Can't be const since we want to be able to move from it.
			length--;
			for (storage_size_t i = index; i < length; i++) {
				data[index] = data[index + 1];
			}
			return result;
			// NOTE: At compile-time: Guaranteed to be moved (at least once. I'm not sure if it sometimes can be moved more
			// than once while returning and being received by the caller).
			// At run-time (if this theoretically were being executed at run-time):
			// Guaranteed to be AT LEAST moved. If it isn't moved, that's because optimizations have been applied
			// and move elision has taken place (google RVO for more details).
			// Fun fact: This is one of the only places in C++ where the as-if rule doesn't apply.
			// Reasoning for diff between run and compile:
			// The afore-mentioned optimizations can only be applied in certain standardized situations.
			// Additionally, the question of whether or not these optimizations can be applied is
			// not solvable for every possible function. Some are too complex to be able to be sure
			// that optimizations can be applied. It's impossible to make a general rule,
			// so the behavior is left undetermined, determined only by the sophistication of the compiler.
			// For compile-time, it's obviously very useful to have rigorous, deterministic behavior,
			// especially since speed isn't as important. That's why move is enforced in cases like this,
			// and the optimizations that might be applicable are not allowed.
			// There are edge-cases, like when the whole function body consists of one return statement,
			// where I'm pretty sure compile-time also does the optimizations, because they're guaranteed
			// at runtime as well, but I'm not too sure. Better check the documentation to be sure.
		}

		// TODO: Add error handling and bounds checking and all that to all of these functions in this class and in others that don't
		// have it yet.

		consteval element_t pluck(iterator_t ptr) {
			element_t result = std::move(*ptr);
			length--;		// TODO: Something wrong here, fix.
			for (; ptr < end(); ptr++) { *ptr = *(ptr + 1); }
			return result;
		}

		consteval void sort()
		requires sortable_element_c<element_t>
		{
			std::sort(begin(), end(), [](const element_t &a, const element_t &b) consteval { return a < b; });
		}

		consteval void sort_and_remove_duplicates()
		requires sortable_element_c<element_t> && equatable_element_c<element_t>
		{
			sort();
			const_iterator_t previous_element_ptr = begin();
			iterator_t ptr = begin() + 1;	// NOTE: This will always work, even for zero-length vectors in this case (cuz capacity must be >0).
			while (ptr < end()) {
				if (*ptr == *previous_element_ptr) {
					pluck(ptr);

					// NOTE: I wanted to use ptr-- to make sure the next iteration refers to the correct element,
					// but that doesn't work when ptr == begin() because having ptr one (or any number of units) below an object
					// is UB. This is because the geometry of the memory space isn't defined by the standard and as such
					// the following is true:
					// --> pointer underflow and overflow is UB
					// --> only within objects is the space guaranteed to be contiguous (and 1 element unit past the object
					// if it's an array or something)
					// --> since objects can be implicitly stored in various places with various rules (strange memory structure,
					// or maybe some sort of address space paging thing like in those old consoles, etc...),
					// any space outside of objects is the wild west.
					// --> also the difference between pointers that are pointing to different objects is implementation defined
					// (so still usable, but only if you're targeting a specific platform)
					// -----> The problem in this case is the overflow underflow thing. You can't under any circumstances let a
					// pointer underflow/overflow (some systems even have hardware interrupts for that which cause errors/exceptions
					// and such). Since any object could be at the start of the address space (the geometry could also
					// fold the address space so that the bottom is somewhere in the middle, also remember ASLR),
					// going one below the object isn't something you should do.
					// Obviously it's not an error and only UB because this is something you cannot enforce at runtime
					// without overhead, which is why C++ opts for no overhead.
					// Unless of course your runtime is at compile-time and overhead isn't the main concern, as is the case here.
					// Then what is UB turns into a hard error and is determinized, as is the case here.
					// The bottom line is we can't do this. We definitely shouldn't do this at runtime either!
					// P.S: Casting some integer to a pointer is UB as well if that pointer doesn't refer to an object/element.
					// And subtracting something and then adding the same thing back to a pointer isn't gonna give you what you
					// started with in all cases. If the pointer doesn't refer to a valid object/element for the whole time,
					// then it's UB and anything can happen. If you subtract two valid pointers in different objects,
					// that number doesn't have to correspond to the actual distance between them, AFAIK,
					// all it has to satisfy is this: If you add that number to the bottom pointer, you'll get the top pointer.
					// As I said above, the actual value for the difference is implementation defined AFAIK.

					//ptr--;

					continue;
				}
				previous_element_ptr = ptr;
				ptr++;
			}
		}

		// NOTE: Same note about expanding as the one in array_container_t.
		consteval loose_storage_size_t find(const element_t &target, size_t begin_index, size_t end_index)
		requires equatable_element_c<element_t>
		{
			if (end_index >= length) { report_error("moken bug detected: vector_t::find called with end_index >= length"); }
			if (end_index < begin_index) { report_error("moken bug detected: vector_t::find called with end_index < begin_index"); }
			if (begin_index >= length) { report_error("moken bug detected: vector_t::find called with begin_index >= length"); }
			return array_container_t<element_t, capacity>::find(target, begin_index, end_index);
		}
		consteval loose_storage_size_t find(const element_t &target)
		requires equatable_element_c<element_t>
		{
			return array_container_t<element_t, capacity>::find(target, 0, length);
		}

		consteval bool is_empty() { return length == 0; }

		consteval void clear() { length = 0; }
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

		// NOTE: This is a good context to talk about the following:
		// C++ turns every mention of stack_t without a template parameter list (provided it's mentioned within the class stack_t)
		// into the same type as the instantiated surrounding class, unless of course you provide an explicit parameter list.
		// I guess the designers thought this a useful feature, because as you can see below, you can use it in the return value for example,
		// to avoid clunky parameter lists.
		// IN REALITY, IT'S STUPID! --> Doing something like stack_t(...) won't deduce the type of the class using template argument deduction,
		// like one would expect. Instead, it does what I mentioned above, which could cause either subtle errors or compilation failures.
		// That's disgusting and confusing for no reason.
		// A MUCH BETTER SOLUTION:
		// C++ should just do nothing. Either you can use auto instead of stack_t& below, or you can define something like:
		// using myself_t = decltype(*this);
		// Or you can just do decltype(*this) as the return value below.
		// That would avoid the unnecessary confusion and the gotcha's that the current behavior causes.

		// NOTE: We don't use the below functions, I just couldn't resist implementing them because the >> and << operators look really nice
		// for stack operations. It's a shame error handling isn't elegantly implementable (which is why we don't use them).

		// NOTE: IGNORES ERRORS, SO BE SURE THAT YOU WON'T GET ANY BEFORE USING!
		consteval stack_t& operator<<(const element_t &right) { push(right); return *this; }

		// NOTE: IGNORES ERRORS, SO BE SURE THAT YOU WON'T GET ANY BEFORE USING!
		template <implicitly_convertible_to_x_type_c<element_t> other_element_t, size_t other_capacity>
		consteval stack_t& operator<<(const stack_t<other_element_t, other_capacity> &other_stack) { push(other_stack); return *this; }

		consteval stack_t& operator>>(element_t &right) { right = pop(); return *this; }

		// NOTE: IGNORES ERRORS, SO BE SURE THAT YOU WON'T GET ANY BEFORE USING!
		template <typename other_element_t, size_t other_capacity>
		requires implicitly_convertible_to_x_type_c<element_t, other_element_t>
		consteval stack_t& operator>>(stack_t<other_element_t, other_capacity> &other_stack) { other_stack << *this; return *this; }
	};

	template <size_t next_vector_capacity_param>
	struct nfa_table_element_t {
		static constexpr size_t next_vector_capacity = next_vector_capacity_param;

		vector_t<size_t, next_vector_capacity> next;
	};

		// TODO: research how to remove defines after header is finished. There oughta be something we can wrap the header in
		// so that the defined defines don't leak out into whatever else.

	template <array_container_t token_array_container,
		  size_t table_length,
		  size_t element_next_vector_capacity,
		  size_t kleene_stack_capacity>
	requires token_array_container_t_c<decltype(token_array_container)>
	consteval auto generate_nfa_from_tokens() {
		using instanced_token_t = typename decltype(token_array_container)::type;
		constexpr size_t token_array_length = decltype(token_array_container)::length;
		// TODO: Why does this compile? It shouldn't right? Since the address isn't known and you can't even use it in for a template parameter. I think this might be another compiler bug.
		constexpr const instanced_token_t (&token_array)[token_array_length] = token_array_container.data;

		constexpr uint32_t table_width = instanced_token_t::row_length;

		using instanced_nfa_table_element_t = nfa_table_element_t<element_next_vector_capacity>;
		constexpr size_t nfa_table_1d_length = table_length * table_width;
		array_container_t<instanced_nfa_table_element_t, nfa_table_1d_length> nfa_table;

		array_container_t<bool, table_length> ghost_rows;

		size_t table_head = 0;

		stack_t<size_t, kleene_stack_capacity> kleene_stack;

		// NOTE: You can't write constexpr in front of auto for these lambdas because that would mean that the whole
		// lambda class instance is constexpr, which would mean that the reference to table_head is constant expression,
		// which it cannot be in this situation. That means we can't write constexpr, because the instance cannot be constexpr.
		// The operator() can be consteval, but the instance cannot be constexpr.
		// Obviously, if the capture is empty then you can write constexpr, because the class instance can very well be constant
		// expression.
		// TODO: Think about this a bit more. Does the fact that the capture variables are private inside the class matter in any way?

		auto create_new_row = [&table_head]() consteval {
			if (table_head >= table_length) {
				report_error("moken bug detected: nfa table head overflowed");
			}
			return table_head++;
		};

		auto register_ghost_row = [
						     &ghost_rows,
						     &table_head = std::as_const(table_head)
						    ]
						    (
						     size_t row_number
						    )
						    consteval
		{
			// TODO: Create shortcut to select last selection, or figure out how to do it, because that will come in
			// handy when trying to indent something you just pasted.
			if (row_number >= table_head) {
				report_error("moken bug detected: register_ghost_row() called with out-of-bounds row_number");
			}
			ghost_rows[row_number] = true;
		};

		auto register_terminator = [
						      &nfa_table,
						      &ghost_rows = std::as_const(ghost_rows),
						      &table_head
						     ]
						     (
						      size_t row_number,
						      uint16_t termination_handler
						     )
						     consteval
		{
			if (row_number >= table_head) {
				report_error("moken bug detected: register_terminator() called with out-of-bounds row_number");
			}
			if (ghost_rows[row_number] == false) {
				report_error("moken bug detected: register_terminator() called for a non-ghost row");
			}
			nfa_table[row_number * table_width].next.clear();
			nfa_table[row_number * table_width + 1].next.clear();
			nfa_table[row_number * table_width + 1].next.push_back(termination_handler);
		};

		auto superimpose_table_row = [
							&nfa_table
						       ]
						       (
							size_t row_number,
							const bool (&row)[table_width],
							size_t target_row_number
						       )
						       consteval
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

		auto superimpose_ghost_row = [
							&nfa_table,
							&ghost_rows = std::as_const(ghost_rows)
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

		auto implementation = [&]
						(
						 size_t token_array_index,
						 size_t current_row,
						 size_t last_table_row,
						 const auto &self
						)
						consteval
						-> std::tuple<size_t, bool, size_t>
		{
			if (token_array_index == token_array_length) {
				register_ghost_row(current_row);
				return { token_array_index, true, current_row };
			}
			if (token_array_index > token_array_length) {
				report_error("moken bug detected: token_array_index exceeded 1-past-token_array in implementation() in generate_nfa_table_from_tokens()");
			}

			const instanced_token_t &token = token_array[token_array_index];
			switch (token.type) {

			case token_type_t::ALTERNATION:
				register_ghost_row(current_row);
				return { token_array_index + 1, false, current_row };

			case token_type_t::KLEENE_CLOSURE_BEGIN:
				if (kleene_stack.push(current_row) == false) {
					report_error("TODO: Write bug error message about how kleene stack was blown");
				}
				return self(token_array_index + 1, current_row, last_table_row, self);

			case token_type_t::KLEENE_CLOSURE_END:
				{
					register_ghost_row(current_row);
					superimpose_ghost_row(current_row, kleene_stack.pop());
					size_t next_row = create_new_row();
					superimpose_ghost_row(current_row, next_row);
					return self(token_array_index + 1, next_row, current_row, self);
				}

			case token_type_t::SUBEXPRESSION_BEGIN:
				{
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
										     current_row,
										     self);

						if (branch_end_rows.push_back(returned_end_row) == false) {
							report_error("moken bug detected: branch_end_rows capacity blown in nfa gen SUBEXPRESSION_BEGIN");
						}

						new_token_array_index = returned_token_array_index;

						if (should_break_out) { break; }
					}

					const size_t ghost_sink = create_new_row();
					register_ghost_row(ghost_sink);
					for (size_t i = 0; i < branch_end_rows.length; i++) {
						superimpose_ghost_row(branch_end_rows[i], ghost_sink);
					}

					const size_t next_row = create_new_row();
					superimpose_ghost_row(ghost_sink, next_row);
					return self(new_token_array_index, next_row, ghost_sink, self);
				}

			case token_type_t::SUBEXPRESSION_END:
				register_ghost_row(current_row);
				return { token_array_index + 1, true, current_row };

			case token_type_t::TABLE_ROW:
				{
					size_t next_row = create_new_row();
					superimpose_table_row(current_row, token.table_row, next_row);
					return self(token_array_index + 1, next_row, current_row, self);
				}

			}
		};

			// TODO: Go through code and see where else you can use that minimum_unsigned_integral_t thing.

		vector_t<size_t, (uint16_t)-1> branch_end_rows;
		size_t next_token_array_index = 0;
		const size_t first_row = create_new_row();
		while (true) {
			auto [returned_next_token_array_index, should_break_out, returned_end_row] = implementation(next_token_array_index,
														    first_row,
														    -1,
														    implementation);
			if (branch_end_rows.push_back(returned_end_row) == false) {
				// TODO: Check that this number doesn't exceed before-hand as form of user input validation.
				// At this stage, it IS a bug, if it hasn't been caught already.
				report_error("moken bug detected: top-level branch_end_rows vector length exceeded capacity while building nfa");
			}
			next_token_array_index = returned_next_token_array_index;
			if (should_break_out) { break; }
		}

		for (uint16_t i = 0; i < branch_end_rows.length; i++) {
			register_terminator(branch_end_rows[i], i);
		}

		return std::pair(nfa_table, ghost_rows);
	}

	struct relative_dfa_table_element_t {
		size_t next;
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

	template <typename T>
	concept nfa_table_container_c = array_container_t_c<T> && nfa_table_element_t_c<typename T::type>;

	template <uint32_t table_width, typename T>
	class is_nfa_table {
	public:
		static constexpr bool value = false;
		consteval operator bool() const { return value; }
	};
	template <uint32_t table_width, nfa_table_container_c nfa_table_container_t>
	class is_nfa_table<table_width, std::pair<nfa_table_container_t, array_container_t<bool, nfa_table_container_t::length / table_width>>> {
	public:
		static constexpr bool value = true;
		consteval operator bool() const { return value; }
	};

	template <uint32_t table_width, typename T>
	inline constexpr bool is_nfa_table_v = is_nfa_table<table_width, T>::value;

	template <typename T, uint32_t table_width>
	concept nfa_table_c = is_nfa_table_v<table_width, T>;

	template <const auto &nfa_table_package, uint32_t table_width, size_t dfa_table_length, size_t possible_states_capacity>
	requires nfa_table_c<remove_const_and_ref_t<decltype(nfa_table_package)>, table_width>
	&& (possible_states_capacity >= decltype(nfa_table_package.first)::type::next_vector_capacity)
	consteval std::tuple<bool,
		  	     array_container_t<relative_dfa_table_element_t, dfa_table_length * table_width>,
			     array_container_t<uint16_t, dfa_table_length>>
		  convert_nfa_to_dfa()
	{
		constexpr auto &nfa_table = nfa_table_package.first;
		using nfa_table_type = remove_const_and_ref_t<decltype(nfa_table)>;
		constexpr auto &nfa_ghost_rows = nfa_table_package.second;
		using nfa_ghost_rows_type = remove_const_and_ref_t<decltype(nfa_ghost_rows)>;

		using instanced_nfa_table_element_t = typename nfa_table_type::type;
		constexpr size_t nfa_table_1d_length = nfa_table_type::length;
		constexpr size_t nfa_table_length = nfa_table_1d_length / table_width;

		// TODO: Enable again.
		//static_assert(nfa_table_length >= dfa_table_length, "moken bug detected: nfa_table_length is smaller than dfa_table_length in convert_nfa_to_dfa()");

		array_container_t<relative_dfa_table_element_t, dfa_table_length * table_width> dfa_table({ (size_t)-1 });
		array_container_t<uint16_t, dfa_table_length> dfa_terminators(-1);
		vector_t<vector_t<size_t, possible_states_capacity>, dfa_table_length> dfa_table_index;
		size_t dfa_table_head_row = 0;

		const auto create_new_dfa_row = [
						 &dfa_table,
						 &dfa_table_index,
						 &dfa_table_head_row
						]
						()
						consteval
		{
			if (dfa_table_head_row == dfa_table_length) {
				report_error("moken bug detected: dfa_table_head_row overflowed in create_new_dfa_row()");
			}
			if (dfa_table_head_row > dfa_table_length) {
				report_error("moken bug detected: dfa_table_head_row somehow got more than 1 unit past end of dfa_table");
			}
			dfa_table_index.push_back({});
			return dfa_table_head_row++;
		};

		const auto set_dfa_next = [
					   &dfa_table,
					   &dfa_table_head_row = std::as_const(dfa_table_head_row)
					  ]
					  (
					   size_t row,
					   minimum_unsigned_integral_t<table_width> inner_index,
					   size_t target_row
					  )
					  consteval
		{
			if (row >= dfa_table_head_row) {
				report_error("moken bug detected: out-of-bounds row passed to set_dfa_next()");
			}
			if (inner_index >= table_width) {
				report_error("moken bug detected: out-of-bounds inner_index passed to set_dfa_next()");
			}
			if (target_row >= dfa_table_head_row) {
				report_error("moken bug detected: out-of-bounds target_row passed to set_dfa_next()");
			}
			dfa_table[row * table_width + inner_index].next = target_row;
		};

		// NOTE: making constexpr variable doesn't work here because of the capture. Same for all the other lambdas in this function.
		const auto superimpose_termination_handler_versions = [
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
		{
			vector_t<size_t, vector_capacity> new_possible_states;
			uint16_t result = -1;
			for (const size_t &state : possible_states) {
				if (nfa_ghost_rows[state] == false) { new_possible_states.push_back(state); continue; }
				if (nfa_table[state * table_width].next.length != 0) {
		report_error("moken bug detected: possible_states cannot contain ghost rows in superimpose_termination_handler_versions");
				}
				result = nfa_table[state * table_width + 1].next[0];
			}
			possible_states = new_possible_states;
			return result;
		};

		const auto register_dfa_termination_handler = [
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

		const auto follow_ghost_rows = [
						&nfa_table,
						&nfa_ghost_rows
					       ]
					       <
						size_t capacity
					       >
					       (
						vector_t<size_t, capacity> &possible_states
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

			for (minimum_unsigned_integral_t<capacity> i = 0; i < possible_states.length; i++) {
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
		const auto superimpose_element_next_vector_versions = [
								       &nfa_table,
								       &follow_ghost_rows
								      ]
								      (
								       const vector_t<size_t, possible_states_capacity> &possible_states,
								       size_t row_index
								      )
								      consteval
								      -> std::pair<size_t, vector_t<size_t, possible_states_capacity>>
		{
			vector_t<size_t, possible_states_capacity> result;
			for (size_t state : possible_states) {
				if (nfa_ghost_rows[state] == true) {
				report_error("moken bug detected: superimpose_element_next_vector_versions() called with ghost row/s in possible_states");
				}
				// NOTE: Using possible_states_capacity here instead of next_vector_capacity on purpose. We need the storage space.
				vector_t<size_t, possible_states_capacity> next_states = nfa_table[state * table_width + row_index].next;
				if (follow_ghost_rows(next_states) == false) { return { false, result }; }
				if (result.push_back(next_states) == false) {
					result.sort_and_remove_duplicates();
					if (result.push_back(next_states) == false) { return { false, result }; }
				}
			}
			result.sort_and_remove_duplicates();
			return { true, result };
		};

		const auto superimpose_element_versions = [
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

		const auto implementation = [&]
					    (
					     size_t dfa_row,
					     vector_t<size_t, possible_states_capacity> &possible_states,
					     const auto &self
					    )
					    consteval
					    -> bool
		{
			/*
			   // TODO: If we're gonna make this system super optimized by even using all the DFA stuff in the first place,
			   we might as well go all the way. That means you oughta consider if the way we plan to handle non-matches is
			   the most efficient. Maybe we should have an error state that is jumped to and then the handler for that
			   state will handle errors. More efficient and more customizable by the user, think about it.
			   // TODO: Also think about and research that grep algorithm that you read about online once.
			   Because once we realize the text is not a match up to a certain point, we have to be able to calculate how
			   much of the text we can skip before starting a new match. That's some sort of function of the structure of
			   the DFA, and calculating that is an interesting problem. But we oughta do that in order to speed things up
			   by quite a great deal potentially.
			*/

			if (follow_ghost_rows(possible_states) == false) { return false; }
			uint16_t termination_handler = superimpose_termination_handler_versions(possible_states);
			if (termination_handler != (uint16_t)-1) { register_dfa_termination_handler(dfa_row, termination_handler); }

			size_t one_child_target_dfa_row;
			{
				auto possible_states_copy = possible_states;

				// NOTE: signed -> unsigned conversions are considered narrowing even if it's
				// from a smaller to a bigger type.
				// I guess narrowing simply means that the value will change, since it can't be represented.
				// And obviously narrowing conversions aren't allowed in aggregate initializations.
				size_t finished_elements[table_width];
				for (size_t &element : finished_elements) { element = -1; }
				size_t first_new_target_element = -1;

				{
					array_container_t<vector_t<size_t, possible_states_capacity>, table_width> possible_states_for_every_element;
					for (size_t i = 0; i < table_width; i++) {
						auto &possible_states_of_element = possible_states_for_every_element[i];
						if (superimpose_element_versions(possible_states, possible_states_of_element, i) == false) {
							return false;
						}
					}

					for (size_t i = 0; i < table_width; i++) {
						if (possible_states_for_every_element[i].is_empty()
						    || finished_elements[i] != -1)
						{ continue; }

						typename decltype(dfa_table_index)::loose_storage_size_t twin_target_row
							= dfa_table_index.find(possible_states_for_every_element[i]);
						if (twin_target_row != (decltype(twin_target_row))-1) {
							finished_elements[i] = -2;
							set_dfa_next(dfa_row, i, twin_target_row);

							for (size_t j = i + 1; j < table_width; j++) {
								if (finished_elements[j] != -1) { continue; }

								const auto &a_states = possible_states_for_every_element[i];
								const auto &b_states = possible_states_for_every_element[j];
								if (a_states == b_states) {
									finished_elements[j] = -2;
									set_dfa_next(dfa_row, j, twin_target_row);
								}
							}
							continue;
						}

						size_t target_row = create_new_dfa_row();

						finished_elements[i] = target_row;

						set_dfa_next(dfa_row, i, target_row);

						if (first_new_target_element == -1) { first_new_target_element = i; }

						dfa_table_index[target_row].push_back(possible_states_for_every_element[i]);

						for (size_t j = i + 1; j < table_width; j++) {
							if (finished_elements[j] != -1) { continue; }

							const auto &a_states = possible_states_for_every_element[i];
							const auto &b_states = possible_states_for_every_element[j];
							if (a_states == b_states) {
								finished_elements[j] = target_row;
								set_dfa_next(dfa_row, j, target_row);
							}
						}
					}

					// NOTE: If there are no elements with new target, then there are no new rows
					// for us to make. We're done (only this recursion is done), return success.
					if (first_new_target_element == -1) { return true; }

					possible_states = possible_states_for_every_element[first_new_target_element];
				}

				bool one_child_recurse = true;
				for (minimum_unsigned_integral_t<table_width> i = first_new_target_element + 1; i < table_width; i++) {
					if (finished_elements[i] < -2) {
						if (finished_elements[i] != finished_elements[first_new_target_element]) {
							one_child_recurse = false;
							break;
						}
					}
				}
				// NOTE: This one_child_recurse flag is necessary because you can't conditionally end a scope in C++.
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
				// So I've gotten out of the scope before recursing, to ensure that the stack is at least smaller,
				// even if I can't ensure that it completely goes away before the call.
				// This incurs a jump, which wouldn't be necessary if I could conditionally end the scope,
				// so it's not the ideal solution, but I've got no better options AFAIK.

				// NOTE: You'll notice that there isn't a jump, that's simply because goto isn't allowed in consteval functions.
				// That's fucking stupid, I don't like that. But I have no choice but to work around it with a flag.
				// So that's what I've done. No jump, instead a flag.

				// NOTE: Used not only for one child recurse.
				one_child_target_dfa_row = finished_elements[first_new_target_element];

				if (!one_child_recurse) {
					self(one_child_target_dfa_row, possible_states, self);

					size_t last_dfa_row = 0;
					for (minimum_unsigned_integral_t<table_width> i = first_new_target_element + 1; i < table_width; i++) {
						if (finished_elements[i] >= -2 || finished_elements[i] <= last_dfa_row) { continue; }
						last_dfa_row = finished_elements[i];
						possible_states.clear();
						if (superimpose_element_versions(possible_states_copy, possible_states, i) == false) {
							return false;
						}
						self(finished_elements[i], possible_states, self);
					}

					return true;
				}
			}

			return self(one_child_target_dfa_row, possible_states, self);
		};

		vector_t<size_t, possible_states_capacity> possible_states;
		if (possible_states.push_back(0) == false) {
			report_error("moken bug detected: possible_states vector capacity blown in convert_nfa_to_dfa() while seeding");
		}
		if (implementation(create_new_dfa_row(), possible_states, implementation) == false) { return { false, { }, { } }; }

		return { true, dfa_table, dfa_terminators };
	}

	template <const auto &nfa_table_package, uint32_t table_width, size_t dfa_table_length,
		  size_t possible_states_capacity = 16/*decltype(nfa_table_package.first)::type::next_vector_capacity*/>
	requires nfa_table_c<remove_const_and_ref_t<decltype(nfa_table_package)>, table_width>
	&& (possible_states_capacity >= decltype(nfa_table_package.first)::type::next_vector_capacity)
	consteval auto convert_nfa_to_dfa_function_runner() {
		constexpr auto dfa_table_package = convert_nfa_to_dfa<nfa_table_package,
		      						      table_width,
								      dfa_table_length,
								      possible_states_capacity>();

		// NOTE: AFAIK, constexpr if only lives up to it's full potential if you actually make use of the else clause.
		// If you just rely on the fact that the return statement implies an else, I don't think the compiler will actually
		// template instantiate the way you would expect it to. It's not an optimization-based thing because it's supposed to
		// be consistent, AFAIK. So it'll only choose between two paths if you actually use an else. Remember that.
		if constexpr (std::get<0>(dfa_table_package) == false) {
			report_error("RETRY REQUESTED!");
			return convert_nfa_to_dfa_function_runner<nfa_table_package,
			       					  table_width,
								  dfa_table_length,
								  possible_states_capacity * 2>();
		} else { return std::pair(std::get<1>(dfa_table_package), std::get<2>(dfa_table_package)); }
	}

	// TODO: Add check to make sure when extra null is selected that the last thing really is a NUL character.
	// TODO: Better yet, make it select EXTRA NULL automatically when it sees a null and not when it doesn't.
	// You can override the behavior by setting spec type explicitly I guess. Is that a good idea or is it confusing?
	template <array_container_t specification, spec_type_t spec_type = spec_type_t::EXTRA_NULL>
	requires sufficiently_compatible_array_container_t_c<decltype(specification)>
	consteval auto make_tokenizer_t() {
		constexpr auto compatible_specification
			= convert_sufficiently_compatible_array_container_to_compatible_array_container<specification>();

		check_specification_syntax<compatible_specification, spec_type>();

		constexpr uint32_t table_width = calculate_table_width<typename decltype(compatible_specification)::type>();

		constexpr auto token_array = tokenize_specification<compatible_specification, table_width, spec_type>();

		// NOTE: Doesn't work because constexpr is syntactically not allowed here, for whatever reason.
		// Hopefully we'll get that feature in future versions of C++.
		/*constexpr auto [
				nfa_max_rows,
				dfa_max_rows,
				element_next_vector_capacity,
				kleene_stack_capacity
			       ]
			       = calculate_table_metrics<token_array>();*/
		constexpr auto table_metrics = calculate_table_metrics<token_array>();
		constexpr size_t nfa_max_rows = std::get<0>(table_metrics);
		constexpr size_t dfa_max_rows = std::get<1>(table_metrics);
		constexpr size_t element_next_vector_capacity = std::get<2>(table_metrics);
		constexpr size_t kleene_stack_capacity = std::get<3>(table_metrics);

		static constexpr auto nfa_table = generate_nfa_from_tokens<token_array,
			  					    nfa_max_rows,
								    element_next_vector_capacity,
								    kleene_stack_capacity>();

		/*
		NOTE: Passing nfa_table by value into the template parameters doesn't work because nfa_table.first is too large.
		It SHOULD work, so I suppose this is a compiler bug!
		In order to work around this, we've passed it by reference. Honestly, we should've probably been doing that anyway,
		so it's not a big deal at all, but it is nontheless a bug as far as I can see.
		TODO: Obviously report and post on forum and all that.

		NOTE: We're lucky that static variables are allowed in consteval functions as of c++23, because or else we wouldn't be
		able to pass the reference through the template parameter. We can't use c++23 as of clang-16, but we can use the working
		draft, which is nice. We don't have to explicitly do that though because clang-16 is allowing the static variable to go through
		with nothing but a warning saying it's from the working draft. I don't know if it's just this functionality that
		it's letting slip through from the working draft, or if it's the entire working draft. In either case,
		I guess we're gonna utilize the C++23 working draft in this project. Compatibility-wise it's not optimal,
		but maybe we can find a more compatible way to do this at a later date.
		TODO: think about that.
		*/

		constexpr auto relative_dfa_table = convert_nfa_to_dfa_function_runner<nfa_table, table_width, dfa_max_rows>();

		return relative_dfa_table;
	}

}

// TODO: We might not need the macro if we can do the finalizing in the constructor of the tokenizer_t.
// TODO: The trick is we only do it if we can take the address of the data, which is only valid sometimes, so you're gonna need some SFINAE.
// That way, no matter how the compiler chooses to return result from make_tokenizer_t(), we'll finalize it once and only once and we'll do it at the right time.
#define MAKE_MOKEN_TOKENIZER_T(NAME, SPECIFICATION, ...) auto NAME = make_tokenizer_t<SPECIFICATION __VA_OPT__(,) __VA_ARGS__>(); NAME.finalize();
