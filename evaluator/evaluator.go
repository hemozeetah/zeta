package evaluator

import (
	"slices"

	"github.com/hemozeetah/zeta/ast"
	"github.com/hemozeetah/zeta/object"
)

func Eval(node ast.Node, env *object.Environment) object.Object {
	switch node := node.(type) {
	case *ast.Program:
		evaluated := evalStatements(node.Statements, env)
		if returnValue, ok := evaluated.(*object.ReturnValue); ok { // unwrap return value
			return returnValue.Value
		}
		return evaluated

	case *ast.ExpressionStatement:
		return Eval(node.Expression, env)

	case *ast.IntegerLiteral:
		return &object.Integer{Value: node.Value}

	case *ast.StringLiteral:
		return &object.String{Value: node.Value}

	case *ast.BooleanLiteral:
		return object.BooleanConstant(node.Value)

	case *ast.NullLiteral:
		return object.NullConstant()

	case *ast.Identifier:
		if val, ok := env.Get(node.Value); ok {
			return val
		}
		return object.NewError("identifier not found: %s", node.Value)

	case *ast.PrefixExpression:
		right := Eval(node.Right, env)
		if object.IsError(right) {
			return right
		}
		return evalPrefixExpression(node.Operator, right)

	case *ast.InfixExpression:
		left := Eval(node.Left, env)
		if object.IsError(left) {
			return left
		}
		right := Eval(node.Right, env)
		if object.IsError(right) {
			return right
		}
		return evalInfixExpression(left, node.Operator, right)

	case *ast.VarStatement:
		value := Eval(node.Value, env)
		if object.IsError(value) {
			return value
		}
		env.Set(node.Name.Value, value)

	case *ast.ReturnStatement:
		value := Eval(node.ReturnValue, env)
		if object.IsError(value) {
			return value
		}
		return &object.ReturnValue{Value: value}

	case *ast.BlockStatement:
		return evalStatements(node.Statements, env)

	case *ast.IfExpression:
		condition := Eval(node.Condition, env)
		if object.IsError(condition) {
			return condition
		}
		if object.IsTruthy(condition) {
			return Eval(node.Consequence, env)
		} else if node.Alternative != nil {
			return Eval(node.Alternative, env)
		} else {
			return object.NULL
		}

	case *ast.FunctionLiteral:
		return &object.Function{Parameters: node.Parameters, Env: env, Body: node.Body}

	case *ast.CallExpression:
		return nil

	case *ast.ArrayLiteral:
		elements := evalExpressions(node.Elements, env)
		if len(elements) == 1 && object.IsError(elements[0]) {
			return elements[0]
		}
		return &object.Array{Elements: elements}

	case *ast.MapLiteral:
		pairs := make(map[object.HashKey]object.Pair)
		for keyNode, valueNode := range node.Pairs {
			key := Eval(keyNode, env)
			if object.IsError(key) {
				return key
			}
			hashKey, ok := key.(object.Hasher)
			if !ok {
				return object.NewError("key map of type %d is not hashable", key.Type())
			}
			value := Eval(valueNode, env)
			if object.IsError(value) {
				return value
			}
			pairs[hashKey.HashKey()] = object.Pair{Key: key, Value: value}
		}
		return &object.Map{Pairs: pairs}

	case *ast.IndexExpression:
		left := Eval(node.Left, env)
		if object.IsError(left) {
			return left
		}
		index := Eval(node.Index, env)
		if object.IsError(index) {
			return index
		}
		switch {
		case left.Type() == object.ARRAY_OBJ && index.Type() == object.INTEGER_OBJ:
			arrayObj := left.(*object.Array)
			idx := index.(*object.Integer).Value
			if idx < 0 || idx >= int64(len(arrayObj.Elements)) {
				return object.NULL
			}
			return arrayObj.Elements[idx]

		case left.Type() == object.MAP_OBJ:
			mapObj := left.(*object.Map)
			key, ok := index.(object.Hasher)
			if !ok {
				return object.NewError("unusable as hash key: %s", object.ObjectMap[index.Type()])
			}
			pair, ok := mapObj.Pairs[key.HashKey()]
			if !ok {
				return object.NULL
			}
			return pair.Value

		default:
			return object.NewError("index operator not supported: %s", object.ObjectMap[left.Type()])
		}
	}

	return nil
}

func evalStatements(statements []ast.Statement, env *object.Environment) object.Object {
	var result object.Object
	for _, s := range statements {
		result = Eval(s, env)
		if object.IsError(result) {
			break
		}
		if result != nil && result.Type() == object.RETURN_VALUE_OBJ {
			break
		}
	}
	return result
}

func evalExpressions(expressions []ast.Expression, env *object.Environment) []object.Object {
	var result []object.Object
	for _, e := range expressions {
		evaluated := Eval(e, env)
		if object.IsError(evaluated) {
			result = []object.Object{evaluated}
			break
		}
		result = append(result, evaluated)
	}
	return result
}

func evalPrefixExpression(operator string, right object.Object) object.Object {
	switch operator {
	case "+":
		if right.Type() != object.INTEGER_OBJ {
			return object.NewError("unknown operator: +%s", object.ObjectMap[right.Type()])
		}
		value := right.(*object.Integer).Value
		return &object.Integer{Value: value}

	case "-":
		if right.Type() != object.INTEGER_OBJ {
			return object.NewError("unknown operator: -%s", object.ObjectMap[right.Type()])
		}
		value := right.(*object.Integer).Value
		return &object.Integer{Value: -value}

	case "!":
		return object.BooleanConstant(!object.IsTruthy(right))

	default:
		return object.NewError("unknown operator: %s%s", operator, object.ObjectMap[right.Type()])
	}
}

func evalInfixExpression(left object.Object, operator string, right object.Object) object.Object {
	switch {
	case left.Type() == object.INTEGER_OBJ && right.Type() == object.INTEGER_OBJ:
		leftVal := left.(*object.Integer).Value
		rightVal := right.(*object.Integer).Value
		switch operator {
		case "+":
			return &object.Integer{Value: leftVal + rightVal}
		case "-":
			return &object.Integer{Value: leftVal - rightVal}
		case "*":
			return &object.Integer{Value: leftVal * rightVal}
		case "/":
			return &object.Integer{Value: leftVal / rightVal}
		case "==":
			return object.BooleanConstant(leftVal == rightVal)
		case "!=":
			return object.BooleanConstant(leftVal != rightVal)
		case "<":
			return object.BooleanConstant(leftVal < rightVal)
		case "<=":
			return object.BooleanConstant(leftVal <= rightVal)
		case ">":
			return object.BooleanConstant(leftVal > rightVal)
		case ">=":
			return object.BooleanConstant(leftVal >= rightVal)
		default:
			return object.NewError("unknown operator: %s %s %s", object.ObjectMap[left.Type()], operator, object.ObjectMap[right.Type()])
		}

	case left.Type() == object.STRING_OBJ && right.Type() == object.STRING_OBJ:
		leftVal := left.(*object.String).Value
		rightVal := right.(*object.String).Value
		switch operator {
		case "+":
			return &object.String{Value: leftVal + rightVal}
		case "==":
			return object.BooleanConstant(leftVal == rightVal)
		case "!=":
			return object.BooleanConstant(leftVal != rightVal)
		case "<":
			return object.BooleanConstant(leftVal < rightVal)
		case "<=":
			return object.BooleanConstant(leftVal <= rightVal)
		case ">":
			return object.BooleanConstant(leftVal > rightVal)
		case ">=":
			return object.BooleanConstant(leftVal >= rightVal)
		default:
			return object.NewError("unknown operator: %s %s %s", object.ObjectMap[left.Type()], operator, object.ObjectMap[right.Type()])
		}

	case left.Type() == object.BOOLEAN_OBJ && right.Type() == object.BOOLEAN_OBJ:
		switch operator {
		case "==":
			return object.BooleanConstant(left == right)
		case "!=":
			return object.BooleanConstant(left != right)
		default:
			return object.NewError("unknown operator: %s %s %s", object.ObjectMap[left.Type()], operator, object.ObjectMap[right.Type()])
		}

	case left.Type() == object.ARRAY_OBJ && right.Type() == object.ARRAY_OBJ:
		leftVal := left.(*object.Array).Elements
		rightVal := right.(*object.Array).Elements
		switch operator {
		case "==":
			return object.BooleanConstant(slices.CompareFunc(leftVal, rightVal, cmpFunc) == 0)
		case "!=":
			return object.BooleanConstant(slices.CompareFunc(leftVal, rightVal, cmpFunc) != 0)
		case "<":
			return object.BooleanConstant(slices.CompareFunc(leftVal, rightVal, cmpFunc) == -1)
		case "<=":
			return object.BooleanConstant(slices.CompareFunc(leftVal, rightVal, cmpFunc) <= 0)
		case ">":
			return object.BooleanConstant(slices.CompareFunc(leftVal, rightVal, cmpFunc) == 1)
		case ">=":
			return object.BooleanConstant(slices.CompareFunc(leftVal, rightVal, cmpFunc) >= 0)
		default:
			return object.NewError("unknown operator: %s %s %s", object.ObjectMap[left.Type()], operator, object.ObjectMap[right.Type()])
		}

	case left.Type() == object.NULL_OBJ || right.Type() == object.NULL_OBJ:
		switch operator {
		case "==":
			return object.BooleanConstant(left.Type() ==  right.Type())
		case "!=":
			return object.BooleanConstant(left.Type() !=  right.Type())
		default:
			return object.NewError("unknown operator: %s %s %s", object.ObjectMap[left.Type()], operator, object.ObjectMap[right.Type()])
		}

	default:
		if left.Type() != right.Type() {
			return object.NewError("type mismatch: %s %s %s", object.ObjectMap[left.Type()], operator, object.ObjectMap[right.Type()])
		}
		return object.NewError("unknown operator: %s %s %s", object.ObjectMap[left.Type()], operator, object.ObjectMap[right.Type()])
	}
}

func cmpFunc(x, y object.Object) int {
	if object.IsTruthy(evalInfixExpression(x, "<", y)) {
		return -1
	} else if object.IsTruthy(evalInfixExpression(x, ">", y)) {
		return 1
	}
	return 0
}
