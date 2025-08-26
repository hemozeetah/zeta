package evaluator

import (
	"github.com/hemozeetah/zeta/ast"
	"github.com/hemozeetah/zeta/object"
)

func Eval(node ast.Node, env *object.Environment) object.Object {
	switch node := node.(type) {
	case *ast.Program:
		return evalStatements(node.Statements, env)

	case *ast.ExpressionStatement:
		return Eval(node.Expression, env)

	case *ast.IntegerLiteral:
		return &object.Integer{Value: node.Value}

	case *ast.StringLiteral:
		return &object.String{Value: node.Value}

	case *ast.BooleanLiteral:
		return &object.Boolean{Value: node.Value}

	case *ast.Identifier:
		if val, ok := env.Get(node.Value); ok {
			return val
		}
		return object.NewError("identifier not found: %s", node.Value)

	case *ast.PrefixExpression:
		return nil

	case *ast.InfixExpression:
		return nil

	case *ast.VarStatement:
		value := Eval(node.Value, env)
		if object.IsError(value) {
			return value
		}
		env.Set(node.Name.Value, value)

	case *ast.ReturnStatement:
		return nil

	case *ast.BlockStatement:
		return evalStatements(node.Statements, env)

	case *ast.IfExpression:
		return nil

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
		return nil
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
