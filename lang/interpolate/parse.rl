package interpolate

import (
	"fmt"
)

%%{
	machine interpolate;
	write data;
}%%

// Parse performs string interpolation on the input. It returns the list of
// tokens found. It looks for variables of the format ${foo}. The curly braces
// are required.
func Parse(data string) (out Stream, _ error) {
	var (
		// variables used by Ragel
		cs  = 0 // current state
		p   = 0 // current position in data
		pe  = len(data)
		eof = pe // eof == pe if this is the last data block

		// Index in data where the currently captured string started.
		idx int

		l Literal  // The string literal being read, if any.
		v Variable // The variable being read, if any.

		// Current token. This is either the variable that we just read
		// or the string literal. We will append it to `out` and move
		// on.
		t Token
	)

	%%{
		# Record the current position as the start of a string. This is
		# usually used with the entry transition (>) to start capturing
		# the string when a state machine is entered.
		#
		# fpc is the current position in the string (basically the same
		# as the variable `p` but a special Ragel keyword) so after
		# executing `start`, data[idx:fpc+1] is the string from when
		# start was called to the current position (inclusive).
		action start { idx = fpc }

		# TODO: check this !
		# A variable always starts with an alphabetical char and
		# contains alphanumeric characters or numbers, underscores, and
		# non-consecutive dots.
		var_name
			= ( [a-z] ([a-z0-9_]
			  | ('.' | '_') [a-z0-9_])*
			  )
			>start
			@{ v.Name = data[idx:fpc+1] }
			;

		# var is a reference to a variable.
		var = '${' var_name '}'
			;

		# Anything followed by a '\' is used as-is.
		escaped_lit = '\\' any @{ l = Literal{Value: data[fpc:fpc+1]} };

		# Anything followed by a '$' that is not a '{' is used as-is
		# with the dollar.
		dollar_lit = '$' (any - '{') @{ l = Literal{Value: data[fpc-1:fpc+1]} };

		# Literal strings that don't contain '$' or '\'.
		simple_lit
			= (any - '$' - '\\')+
			>start
			@{ l = Literal{Value: data[idx:fpc + 1]} }
			;

		lit = escaped_lit | dollar_lit | simple_lit;

		# Tokens are the two possible components in a string. Either a
		# literal or a variable reference.
		token = (var @{ t = v }) | (lit @{ t = l });

		main := (token %{ out = append(out, t) })**;

		write init;
		write exec;
	}%%

	if cs < %%{ write first_final; }%% {
		return out, fmt.Errorf("cannot parse string: %s", data)
	}

	return out, nil
}
