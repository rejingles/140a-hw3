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

	if expr.isNil() {	
		return mkNil(), nil
	}

	if expr.isNumber() {	
		return mkNumber(new(big.Int).Set(expr.atom.num)), nil
	}

	if expr.isSymbol() {	
		switch expr.atom.literal {
		case "T":		
			return mkSymbolTrue(), nil
		case "NIL":		
			return mkNil(), nil
		default:	
		return nil, ErrEval
		}
	}

	if !expr.isConsCell() {	
		return nil, ErrEval	
	}


	head := expr.car
	args := expr.cdr

	if !head.isSymbol() {	
		return nil, ErrEval	
	}

	
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
		return nil, ErrEval		
	}
}


func evalArgs(args *SExpr) ([]*SExpr, error) {
	var evalargs []*SExpr	
	idx := args	

	for {
		if idx.isNil() {
			return evalargs, nil	
		}
		if !idx.isConsCell() {	
			return nil, ErrEval
		}
		evaluated, err := idx.car.Eval()	
		if err != nil {			
			return nil, err
		}
		evalargs = append(evalargs, evaluated)	
		idx = idx.cdr	
	}
}


func evalQUOTE(args *SExpr) (*SExpr, error) {
	if !args.isConsCell() || args.isNil() {	
		return nil, ErrEval
	}

	
	head := args.car
	tail := args.cdr

	if !tail.isNil() {		
		return nil, ErrEval
	}

	
	return head, nil
}


func evalCAR(args *SExpr) (*SExpr, error) {
	evaluated, err := evalArgs(args)
	if err != nil {
		return nil, err
	}
	if len(evaluated) != 1 {	
		return nil, ErrEval
	}

	list := evaluated[0]		

	if list.isNil() {			
		return mkNil(), nil
	}
	if !list.isConsCell() {
		return nil, ErrEval
	}
	return list.car, nil		
}


func evalCDR(args *SExpr) (*SExpr, error) {
	evaluated, err := evalArgs(args)
	if err != nil {
		return nil, err
	}
	if len(evaluated) != 1 {	
		return nil, ErrEval
	}

	arg := evaluated[0]			

	if arg.isNil() {			
		return mkNil(), nil
	}
	if !arg.isConsCell() {
		return nil, ErrEval
	}
	return arg.cdr, nil			
}	


func evalCONS(args *SExpr) (*SExpr, error) {
	evaluated, err := evalArgs(args)
	if err != nil {
		return nil, err
	}
	if len(evaluated) != 2 {	
		return nil, ErrEval
	}
	
	return mkConsCell(evaluated[0], evaluated[1]), nil
}


func evalLENGTH(args *SExpr) (*SExpr, error) {
	evaluated, err := evalArgs(args)
	if err != nil {
		return nil, err
	}
	if len(evaluated) != 1 {	
		return nil, ErrEval
	}
	
	arg := evaluated[0]
	count := 0
	idx := arg
	for {
		if idx.isNil() {	
			return mkNumber(big.NewInt(int64(count))), nil
		}
		if !idx.isConsCell() {	
			return nil, ErrEval
		}
		count++	
		idx = idx.cdr	
	}
}


func evalSum(args *SExpr) (*SExpr, error) {
	evaluated, err := evalArgs(args)
	if err != nil {
		return nil, err
	}

	total := big.NewInt(0)	

	for _, arg := range evaluated {
		if !arg.isNumber() {	
			return nil, ErrEval
		}
		total.Add(total, arg.atom.num)	
	}
	return mkNumber(total), nil	
}

func evalProduct(args *SExpr) (*SExpr, error) {
	evaluated, err := evalArgs(args)
	if err != nil {
		return nil, err
	}

	total := big.NewInt(1)	

	for _, arg := range evaluated {
		if !arg.isNumber() {	
			return nil, ErrEval
		}
		total.Mul(total, arg.atom.num)	
	}
	return mkNumber(total), nil	
}


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


func evalZEROP(args *SExpr) (*SExpr, error) {
	evaluated, err := evalArgs(args)
	if err != nil {
		return nil, err
	}
	if len(evaluated) != 1 {
		return nil, ErrEval
	}
	arg := evaluated[0]
	if !arg.isNumber() {	
		return nil, ErrEval
	}
	if arg.atom.num.Sign() == 0 {	
		return mkSymbolTrue(), nil
	}
	return mkNil(), nil
}