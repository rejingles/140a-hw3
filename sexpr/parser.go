package sexpr

import "errors"

// ErrParser is the error value returned by the Parser if the string is not a
// valid term.
// See also https://golang.org/pkg/errors/#New
// and // https://golang.org/pkg/builtin/#error
var ErrParser = errors.New("parser error")

//
// <sexpr>       ::= <atom> | <pars> | QUOTE <sexpr>
// <atom>        ::= NUMBER | SYMBOL
// <pars>        ::= LPAR <dotted_list> RPAR | LPAR <proper_list> RPAR
// <dotted_list> ::= <proper_list> <sexpr> DOT <sexpr>
// <proper_list> ::= <sexpr> <proper_list> | \epsilon
//
type Parser interface {
	Parse(string) (*SExpr, error)
}

type parserObj struct {
	lex 	*lexer		// create tokens
	token 	*token		// current token
}

// NewParser creates a struct of a type that satisfies the Parser interface.
func NewParser() Parser {
	return &parserObj{}
}

func (p *parserObj) Parse(input string) (*SExpr, error) {
	p.lex = newLexer(input) // newLexer creates a new instance of the type lexer with the input string
	p.token = nil	// initilize current token to nil
	expr, err := p.parseSExpr()	// parse one S-Expression
	if err != nil {
		return nil, err
	}

	next, err := p.nextToken()	// check if the next token is nil/EOF
	if err != nil {
		return nil, ErrParser
	}
	if next.typ != tokenEOF {
		return nil, ErrParser
	}

	return expr, nil	// return expr
}

// move to next token
func (p *parserObj) nextToken() (*token, error) {
	if p.token != nil {	// if non-nil, return token
		tok := p.token
		p.token = nil	// reset current token idx
		return tok, nil
	}
	return p.lex.next()
}

// check next token
func (p *parserObj) Token() (*token, error) {
	if p.token != nil {	// if token is non-nil, return token
		return p.token, nil
	}
	tok, err := p.lex.next()
	if err != nil {
		return nil, err
	}
	p.token = tok	// buffer the token
	return tok, nil
}

func (p *parserObj) parseSExpr() (*SExpr, error) {
	tok, err := p.Token()
	if err != nil {
		return nil, ErrParser
	}

	switch tok.typ {	// check token type
	case tokenQuote:	// if quote
		p.nextToken()	// move to next token
		list, err := p.parseSExpr()
		if err != nil {
			return nil, err
		}
		return mkConsCell(mkSymbol("QUOTE"), mkConsCell(list, mkNil())), nil	// make a cons cell
	case tokenLpar:	// if (, throw to handler function 
		return p.parsePars()
	case tokenNumber, tokenSymbol:	// if number or symbol, throw to handlr function
		return p.parseAtom()
	default:	// else return error
		return nil, ErrParser
	}
}

// parsing numbers or symbols
func (p *parserObj) parseAtom() (*SExpr, error) {
	tok, err := p.nextToken()
	if err != nil {
		return nil, ErrParser
	}
	switch tok.typ {
	case tokenNumber:	// if number, return numbers
		return mkNumber(tok.num), nil
	case tokenSymbol:	// if symbol, return symbol
		if tok.literal == "NIL" {	// if the symbol is nil, return nil
			return mkNil(), nil
		}
		return mkSymbol(tok.literal), nil
	default:	// else return error
		return nil, ErrParser
 	}
}

// parsing parentheses
func (p *parserObj) parsePars() (*SExpr, error) {
	_, err := p.nextToken()
	if err != nil {
		return nil, ErrParser
	}

	var elems []*SExpr	// values inside the parentheses

	for {
		tok, err := p.Token()
		if err != nil {
			return nil, ErrParser
		}
		if tok.typ == tokenRpar {	// if ), we have reached the end of the expression
			p.nextToken()
			return makeCons(elems, mkNil()), nil // create cons cell
		}
		if tok.typ == tokenDot { // if . there is one more S-Expression before we reach the )
			p.nextToken()
			tail, err := p.parseSExpr()
			if err != nil {
				return nil, err
			}
			closing, err := p.nextToken() // check for )
			if err != nil || closing.typ != tokenRpar {
				return nil, ErrParser
			}
			return makeCons(elems, tail), nil // create cons cell
		}
		if tok.typ == tokenEOF {	// if we reach EOF before ), return error
			return nil, ErrParser
		}
		selem, err := p.parseSExpr()	// else, token starts a new S-expression
		if err != nil {
			return nil, err
		}
		elems = append(elems, selem) // append new expression into element list
	}
}

// make a cons cell
func makeCons(elems []*SExpr, tail *SExpr) *SExpr {
	cons := tail
	for i := len(elems) - 1; i >=0; i-- {
		cons = mkConsCell(elems[i], cons)
	}
	return cons
}