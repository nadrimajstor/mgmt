// Mgmt
// Copyright (C) 2013-2020+ James Shubin and the project contributors
// Written by James Shubin <james@shubin.ca> and the project contributors
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

package lang // TODO: move this into a sub package of lang/$name?

import (
	"fmt"
	"strconv"
	"strings"
	"unicode/utf8"

	"github.com/purpleidea/mgmt/lang/interfaces"
	"github.com/purpleidea/mgmt/lang/interpolate"
	"github.com/purpleidea/mgmt/util/errwrap"

	"github.com/hashicorp/hil"
	hilast "github.com/hashicorp/hil/ast"
)

const (
	// UseHilInterpolation specifies that we use the legacy Hil interpolate.
	// This can't properly escape a $ in the standard way. It's here in case
	// someone wants to play with it and examine how the AST stuff worked...
	UseHilInterpolation = false
)

// Pos represents a position in the code.
// TODO: consider expanding with range characteristics.
type Pos struct {
	Line     int    // line number starting at 1
	Column   int    // column number starting at 1
	Filename string // optional source filename, if known
}

// InterpolateStr interpolates a string and returns the representative AST.
func InterpolateStr(str string, pos *Pos, data *interfaces.Data) (interfaces.Expr, error) {
	if data.Debug {
		data.Logf("interpolating: %s", str)
	}

	if UseHilInterpolation {
		return InterpolateHil(str, pos, data)
	}
	return InterpolateRagel(str, pos, data)
}

// InterpolateRagel interpolates a string and returns the representative AST. It
// uses the ragel parser to perform the string interpolation.
func InterpolateRagel(str string, pos *Pos, data *interfaces.Data) (interfaces.Expr, error) {
	sequence, err := interpolate.Parse(str)
	if err != nil {
		return nil, errwrap.Wrapf(err, "parser failed")
	}

	exprs := []interfaces.Expr{}
	for _, term := range sequence {

		switch t := term.(type) {
		case interpolate.Literal:
			expr := &ExprStr{
				V: t.Value,
			}
			exprs = append(exprs, expr)

		case interpolate.Variable:
			expr := &ExprVar{
				Name: t.Name,
			}
			exprs = append(exprs, expr)
		default:
			return nil, fmt.Errorf("unknown term (%T): %+v", t, t)
		}
	}

	// The parser produces non-optimal results where two strings are next to
	// each other, when they could be statically combined together.
	simplified, err := simplifyExprList(exprs)
	if err != nil {
		return nil, errwrap.Wrapf(err, "expr list simplify failed")
	}

	return concatExprListIntoCall(simplified)
}

// InterpolateHil interpolates a string and returns the representative AST. This
// particular implementation uses the hashicorp hil library and syntax to do so.
func InterpolateHil(str string, pos *Pos, data *interfaces.Data) (interfaces.Expr, error) {
	var line, column int = -1, -1
	var filename string
	if pos != nil {
		line = pos.Line
		column = pos.Column
		filename = pos.Filename
	}
	hilPos := hilast.Pos{
		Line:     line,
		Column:   column,
		Filename: filename,
	}
	// should not error on plain strings
	tree, err := hil.ParseWithPosition(str, hilPos)
	if err != nil {
		return nil, errwrap.Wrapf(err, "can't parse string interpolation: `%s`", str)
	}
	if data.Debug {
		data.Logf("tree: %+v", tree)
	}

	transformData := &interfaces.Data{
		// TODO: add missing fields here if/when needed
		Fs:         data.Fs,
		FsURI:      data.FsURI,
		Base:       data.Base,
		Files:      data.Files,
		Imports:    data.Imports,
		Metadata:   data.Metadata,
		Modules:    data.Modules,
		Downloader: data.Downloader,
		//World:      data.World,
		Prefix: data.Prefix,
		Debug:  data.Debug,
		Logf: func(format string, v ...interface{}) {
			data.Logf("transform: "+format, v...)
		},
	}
	result, err := hilTransform(tree, transformData)
	if err != nil {
		return nil, errwrap.Wrapf(err, "error running AST map: `%s`", str)
	}
	if data.Debug {
		data.Logf("transform: %+v", result)
	}

	// make sure to run the Init on the new expression
	return result, errwrap.Wrapf(result.Init(data), "init failed")
}

// hilTransform returns the AST equivalent of the hil AST.
func hilTransform(root hilast.Node, data *interfaces.Data) (interfaces.Expr, error) {
	switch node := root.(type) {
	case *hilast.Output: // common root node
		if data.Debug {
			data.Logf("got output type: %+v", node)
		}

		if len(node.Exprs) == 0 {
			return nil, fmt.Errorf("no expressions found")
		}
		if len(node.Exprs) == 1 {
			return hilTransform(node.Exprs[0], data)
		}

		// assumes len > 1
		args := []interfaces.Expr{}
		for _, n := range node.Exprs {
			expr, err := hilTransform(n, data)
			if err != nil {
				return nil, errwrap.Wrapf(err, "root failed")
			}
			args = append(args, expr)
		}

		// XXX: i think we should be adding these args together, instead
		// of grouping for example...
		result, err := concatExprListIntoCall(args)
		if err != nil {
			return nil, errwrap.Wrapf(err, "function grouping failed")
		}
		return result, nil

	case *hilast.Call:
		if data.Debug {
			data.Logf("got function type: %+v", node)
		}
		args := []interfaces.Expr{}
		for _, n := range node.Args {
			arg, err := hilTransform(n, data)
			if err != nil {
				return nil, fmt.Errorf("call failed: %+v", err)
			}
			args = append(args, arg)
		}

		return &ExprCall{
			Name: node.Func, // name
			Args: args,
		}, nil

	case *hilast.LiteralNode: // string, int, etc...
		if data.Debug {
			data.Logf("got literal type: %+v", node)
		}

		switch node.Typex {
		case hilast.TypeBool:
			return &ExprBool{
				V: node.Value.(bool),
			}, nil

		case hilast.TypeString:
			return &ExprStr{
				V: node.Value.(string),
			}, nil

		case hilast.TypeInt:
			return &ExprInt{
				// node.Value is an int stored as an interface
				V: int64(node.Value.(int)),
			}, nil

		case hilast.TypeFloat:
			return &ExprFloat{
				V: node.Value.(float64),
			}, nil

		// TODO: should we handle these too?
		//case hilast.TypeList:
		//case hilast.TypeMap:

		default:
			return nil, fmt.Errorf("unmatched type: %T", node)
		}

	case *hilast.VariableAccess: // variable lookup
		if data.Debug {
			data.Logf("got variable access type: %+v", node)
		}
		return &ExprVar{
			Name: node.Name,
		}, nil

	//case *hilast.Index:
	//	if va, ok := node.Target.(*hilast.VariableAccess); ok {
	//		v, err := NewInterpolatedVariable(va.Name)
	//		if err != nil {
	//			resultErr = err
	//			return n
	//		}
	//		result = append(result, v)
	//	}
	//	if va, ok := node.Key.(*hilast.VariableAccess); ok {
	//		v, err := NewInterpolatedVariable(va.Name)
	//		if err != nil {
	//			resultErr = err
	//			return n
	//		}
	//		result = append(result, v)
	//	}

	default:
		return nil, fmt.Errorf("unmatched type: %+v", node)
	}
}

// concatExprListIntoCall takes a list of expressions, and combines them into an
// expression which ultimately concatenates them all together with a + operator.
// TODO: this assumes they're all strings, do we need to watch out for int's?
func concatExprListIntoCall(exprs []interfaces.Expr) (interfaces.Expr, error) {
	if len(exprs) == 0 {
		return nil, fmt.Errorf("empty list")
	}

	operator := &ExprStr{
		V: "+", // for PLUS this is a `+` character
	}

	if len(exprs) == 1 {
		return exprs[0], nil // just return self
	}
	//if len(exprs) == 1 {
	//	arg := exprs[0]
	//	emptyStr := &ExprStr{
	//		V: "", // empty str
	//	}
	//	return &ExprCall{
	//		Name: operatorFuncName, // concatenate the two strings with + operator
	//		Args: []interfaces.Expr{
	//			operator, // operator first
	//			arg,      // string arg
	//			emptyStr,
	//		},
	//	}, nil
	//}

	head, tail := exprs[0], exprs[1:]

	grouped, err := concatExprListIntoCall(tail)
	if err != nil {
		return nil, err
	}

	return &ExprCall{
		Name: operatorFuncName, // concatenate the two strings with + operator
		Args: []interfaces.Expr{
			operator, // operator first
			head,     // string arg
			grouped,  // nested function call which returns a string
		},
	}, nil
}

// simplifyExprList takes a list of *ExprStr and *ExprVar and groups the
// sequential *ExprStr's together. If you pass it a list of Expr's that contains
// a different type of Expr, then this will error.
func simplifyExprList(exprs []interfaces.Expr) ([]interfaces.Expr, error) {
	last := false
	result := []interfaces.Expr{}

	for _, x := range exprs {
		switch v := x.(type) {
		case *ExprStr:
			if !last {
				last = true
				result = append(result, x)
				continue
			}

			// combine!
			last := result[len(result)-1] // there has to be at least one
			str, ok := last.(*ExprStr)
			if !ok {
				// programming error
				return nil, fmt.Errorf("unexpected type (%T)", last)
			}
			str.V += v.V // combine!
			//last = true // redundant, it's already true
			// ... and don't append, we've combined!

		case *ExprVar:
			last = false // the next one can't combine with me
			result = append(result, x)

		default:
			return nil, fmt.Errorf("unsupported type (%T)", x)
		}
	}

	return result, nil
}

// Unquote interprets s as a single-quoted, double-quoted, or backquoted Go
// string literal, returning the string value that s quotes. (If s is
// single-quoted, it would be a Go character literal; Unquote returns the
// corresponding one-character string.)
// NOTE: This was copied and modified from the strconv package.
func Unquote(s string) (string, error) {
	n := len(s)
	if n < 2 {
		return "", strconv.ErrSyntax
	}
	quote := s[0]
	if quote != s[n-1] {
		return "", strconv.ErrSyntax
	}
	s = s[1 : n-1]

	if quote == '`' {
		if contains(s, '`') {
			return "", strconv.ErrSyntax
		}
		if contains(s, '\r') {
			// -1 because we know there is at least one \r to remove.
			buf := make([]byte, 0, len(s)-1)
			for i := 0; i < len(s); i++ {
				if s[i] != '\r' {
					buf = append(buf, s[i])
				}
			}
			return string(buf), nil
		}
		return s, nil
	}
	if quote != '"' && quote != '\'' {
		return "", strconv.ErrSyntax
	}
	if contains(s, '\n') {
		return "", strconv.ErrSyntax
	}

	// Is it trivial? Avoid allocation.
	if !contains(s, '\\') && !contains(s, quote) {
		switch quote {
		case '"':
			if utf8.ValidString(s) {
				return s, nil
			}
		case '\'':
			r, size := utf8.DecodeRuneInString(s)
			if size == len(s) && (r != utf8.RuneError || size != 1) {
				return s, nil
			}
		}
	}

	var runeTmp [utf8.UTFMax]byte
	buf := make([]byte, 0, 3*len(s)/2) // Try to avoid more allocations.
	for len(s) > 0 {
		c, multibyte, ss, err := UnquoteChar(s, quote)
		if err != nil {
			return "", err
		}
		s = ss
		if c < utf8.RuneSelf || !multibyte {
			buf = append(buf, byte(c))
		} else {
			n := utf8.EncodeRune(runeTmp[:], c)
			buf = append(buf, runeTmp[:n]...)
		}
		if quote == '\'' && len(s) != 0 {
			// single-quoted must be single character
			return "", strconv.ErrSyntax
		}
	}
	return string(buf), nil
}

// UnquoteChar decodes the first character or byte in the escaped string or
// character literal represented by the string s. It returns four values:
//
// 1) value, the decoded Unicode code point or byte value;
// 2) multibyte, a boolean indicating whether the decoded character requires a
// multibyte UTF-8 representation;
// 3) tail, the remainder of the string after the character; and
// 4) an error that will be nil if the character is syntactically valid.
//
// The second argument, quote, specifies the type of literal being parsed and
// therefore which escaped quote character is permitted. If set to a single
// quote, it permits the sequence \' and disallows unescaped '. If set to a
// double quote, it permits \" and disallows unescaped ". If set to zero, it
// does not permit either escape and allows both quote characters to appear
// unescaped.
// NOTE: This was copied and modified from the strconv package.
func UnquoteChar(s string, quote byte) (value rune, multibyte bool, tail string, err error) {
	// easy cases
	if len(s) == 0 {
		err = strconv.ErrSyntax
		return
	}
	switch c := s[0]; {
	case c == quote && (quote == '\'' || quote == '"'):
		err = strconv.ErrSyntax
		return
	case c >= utf8.RuneSelf:
		r, size := utf8.DecodeRuneInString(s)
		return r, true, s[size:], nil
	case c != '\\':
		return rune(s[0]), false, s[1:], nil
	}

	// hard case: c is backslash
	if len(s) <= 1 {
		err = strconv.ErrSyntax
		return
	}
	c := s[1] // this is the thing after the slash so "n" if it's \n
	s = s[2:] // this is what comes after the "\n"

	switch c {
	case 'a':
		value = '\a'
	case 'b':
		value = '\b'
	case 'f':
		value = '\f'
	case 'n':
		value = '\n'
	case 'r':
		value = '\r'
	case 't':
		value = '\t'
	case 'v':
		value = '\v'
	case 'x', 'u', 'U':
		n := 0
		switch c {
		case 'x':
			n = 2
		case 'u':
			n = 4
		case 'U':
			n = 8
		}
		var v rune
		if len(s) < n {
			err = strconv.ErrSyntax
			return
		}
		for j := 0; j < n; j++ {
			x, ok := unhex(s[j])
			if !ok {
				err = strconv.ErrSyntax
				return
			}
			v = v<<4 | x
		}
		s = s[n:]
		if c == 'x' {
			// single-byte string, possibly not UTF-8
			value = v
			break
		}
		if v > utf8.MaxRune {
			err = strconv.ErrSyntax
			return
		}
		value = v
		multibyte = true
	case '0', '1', '2', '3', '4', '5', '6', '7':
		v := rune(c) - '0'
		if len(s) < 2 {
			err = strconv.ErrSyntax
			return
		}
		for j := 0; j < 2; j++ { // one digit already; two more
			x := rune(s[j]) - '0'
			if x < 0 || x > 7 {
				err = strconv.ErrSyntax
				return
			}
			v = (v << 3) | x
		}
		s = s[2:]
		if v > 255 {
			err = strconv.ErrSyntax
			return
		}
		value = v
	case '\\':
		value = '\\'
	case '\'', '"':
		if c != quote {
			err = strconv.ErrSyntax
			return
		}
		value = rune(c)
	case '$': // we got a \$
		value = '\\' // this is a single backslash
		s = "$" + s  // prepend the left over $
	default:
		err = strconv.ErrSyntax
		return
	}
	tail = s
	return
}

// contains reports whether the string contains the byte c.
// NOTE: This was copied and modified from the strconv package.
func contains(s string, c byte) bool {
	// original implementation before the bytealg optimization:
	//for i := 0; i < len(s); i++ {
	//	if s[i] == c {
	//		return true
	//	}
	//}
	//return false
	//return strings.Contains(s, string(c)) // probably much slower
	//return bytealg.IndexByteString(s, c) != -1 // modern version
	return strings.IndexByte(s, c) != -1 // modern equivalent
}

// NOTE: This was copied and modified from the strconv package.
func unhex(b byte) (v rune, ok bool) {
	c := rune(b)
	switch {
	case '0' <= c && c <= '9':
		return c - '0', true
	case 'a' <= c && c <= 'f':
		return c - 'a' + 10, true
	case 'A' <= c && c <= 'F':
		return c - 'A' + 10, true
	}
	return
}
