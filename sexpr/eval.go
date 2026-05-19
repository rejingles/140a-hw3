package sexpr

import (
	"errors"
	"math/big"
)

// ErrEval is the error value returned by the Evaluator if the contains
// an invalid token.
// See also https://golang.org/pkg/errors/#New
// and // https://golang.org/pkg/builtin/#error
var ErrEval = errors.New("eval error")

func (expr *SExpr) Eval() (*SExpr, error) {

	if expr.isNil() {	// if expr is nil, return nil
		return mkNil(), nil
	}

	if expr.isNumber() {	// if expr is a number, return the number
		return mkNumber(new(big.Int).Set(expr.atom.num)), nil
	}

	if expr.isSymbol() {	
		switch expr.atom.literal {
		case "T":		// if T, return true
			return mkSymbolTrue(), nil
		case "NIL":		// if NIL, return nil
			return mkNil(), nil
		default:	// if not t or nil, return error
		return nil, ErrEval
		}
	}

	if !expr.isConsCell() {	// remaining values should be a cons cell
		return nil, ErrEval	// if not cons cell, return an error
	}

	// separate the cons cell from the function and the args
	head := expr.car
	args := expr.cdr

	if !head.isSymbol() {	// if the head of the cons cell is a symbol, return an error
		return nil, ErrEval	// must be a function
	}

	// read head of the cons cell, throw args to appropriate handler function
	switch head.atom.literal {
	case "QUOTE":
		return evalQUOTE(args)
	case "CAR":
		return evalCAR(args)
	case "CDR":
		return evalCDR(args)
	case "CONS":
		return evalCONS(args)
	case "LENGTH":
		return evalLENGTH(args)
	case "+":
		return evalSum(args)
	case "*":
		return evalProduct(args)
	case "ATOM":
		return evalATOM(args)
	case "LISTP":
		return evalLISTP(args)
	case "ZEROP":
		return evalZEROP(args)
	default:
		return nil, ErrEval		// returns error if none of these are caught
	}
}

// evaluate arg list
func evalArgs(args *SExpr) ([]*SExpr, error) {
	var evalargs []*SExpr	// evaluated args
	idx := args	// idx for evaluated args

	for {
		if idx.isNil() {
			return evalargs, nil	// there are no more args to evaluate, return
		}
		if !idx.isConsCell() {	// if not a cons cell, return error
			return nil, ErrEval
		}
		evaluated, err := idx.car.Eval()	// evalute current element
		if err != nil {			// error checking
			return nil, err
		}
		evalargs = append(evalargs, evaluated)	//	add evaluated element to args list
		idx = idx.cdr	// move index to the right
	}
}

// evaluate quote
func evalQUOTE(args *SExpr) (*SExpr, error) {
	if !args.isConsCell() || args.isNil() {	// if args is nil or if it's not a cons, return error
		return nil, ErrEval
	}

	// separate the head
	head := args.car
	tail := args.cdr

	if !tail.isNil() {		// tail should be nil
		return nil, ErrEval
	}

	// return head
	return head, nil
}

// evaluate car
func evalCAR(args *SExpr) (*SExpr, error) {
	evaluated, err := evalArgs(args)
	if err != nil {
		return nil, err
	}
	if len(evaluated) != 1 {	// only one argument for car function, else return error
		return nil, ErrEval
	}

	list := evaluated[0]		// store list

	if list.isNil() {			// if list is nil or a cons cell, return error
		return mkNil(), nil
	}
	if !list.isConsCell() {
		return nil, ErrEval
	}
	return list.car, nil		// return head of list
}

//evaluate cdr
func evalCDR(args *SExpr) (*SExpr, error) {
	evaluated, err := evalArgs(args)
	if err != nil {
		return nil, err
	}
	if len(evaluated) != 1 {	// only one argument for cdr function, else return error
		return nil, ErrEval
	}

	arg := evaluated[0]			// store list

	if arg.isNil() {			// if list is nil or a cons cell, return error
		return mkNil(), nil
	}
	if !arg.isConsCell() {
		return nil, ErrEval
	}
	return arg.cdr, nil			// return the tail of the list
}	

// evaluate a cons cell
func evalCONS(args *SExpr) (*SExpr, error) {
	evaluated, err := evalArgs(args)
	if err != nil {
		return nil, err
	}
	if len(evaluated) != 2 {	// must have 2 args to create a cons cell, otherwise error
		return nil, ErrEval
	}
	// create a new cons cell
	return mkConsCell(evaluated[0], evaluated[1]), nil
}

// evaluate length
func evalLENGTH(args *SExpr) (*SExpr, error) {
	evaluated, err := evalArgs(args)
	if err != nil {
		return nil, err
	}
	if len(evaluated) != 1 {	// only 1 list must be present to check length
		return nil, ErrEval
	}
	// initialize the count to 0
	arg := evaluated[0]
	count := 0
	idx := arg
	for {
		if idx.isNil() {	// if there are no more elements, return count
			return mkNumber(big.NewInt(int64(count))), nil
		}
		if !idx.isConsCell() {	// if not a cons cell, return error
			return nil, ErrEval
		}
		count++	// raise count
		idx = idx.cdr	// move idx
	}
}

// evaluate sum
func evalSum(args *SExpr) (*SExpr, error) {
	evaluated, err := evalArgs(args)
	if err != nil {
		return nil, err
	}

	total := big.NewInt(0)	// initilize to 0

	for _, arg := range evaluated {
		if !arg.isNumber() {	// if not adding to a number, return error
			return nil, ErrEval
		}
		total.Add(total, arg.atom.num)	// add current number to total
	}
	return mkNumber(total), nil	// return total number
}

func evalProduct(args *SExpr) (*SExpr, error) {
	evaluated, err := evalArgs(args)
	if err != nil {
		return nil, err
	}

	total := big.NewInt(1)	// initilize to 1 

	for _, arg := range evaluated {
		if !arg.isNumber() {	// if not mulitplying to a number, return error
			return nil, ErrEval
		}
		total.Mul(total, arg.atom.num)	// multiply current number by total
	}
	return mkNumber(total), nil	// return total number
}

// if an atom, return true, else return false
func evalATOM(args *SExpr) (*SExpr, error) {
	evaluated, err := evalArgs(args)
	if err != nil {
		return nil, err
	}
	if len(evaluated) != 1 {
		return nil, ErrEval
	}
	if evaluated[0].isAtom() {
		return mkSymbolTrue(), nil
	}
	return mkNil(), nil
}

// if an list, return true, else return false
func evalLISTP(args *SExpr) (*SExpr, error) {
	evaluated, err := evalArgs(args)
	if err != nil {
		return nil, err
	}
	if len(evaluated) != 1 {
		return nil, ErrEval
	}
	if evaluated[0].isConsCell() {
		return mkSymbolTrue(), nil
	}
	return mkNil(), nil
}

// if number is 0, return true, else return false
func evalZEROP(args *SExpr) (*SExpr, error) {
	evaluated, err := evalArgs(args)
	if err != nil {
		return nil, err
	}
	if len(evaluated) != 1 {
		return nil, ErrEval
	}
	arg := evaluated[0]
	if !arg.isNumber() {	// if not a number, return error
		return nil, ErrEval
	}
	if arg.atom.num.Sign() == 0 {	// if number is not positive or negative, then it is zero
		return mkSymbolTrue(), nil
	}
	return mkNil(), nil
}