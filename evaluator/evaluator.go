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
		return nil

	case *ast.PrefixExpression:
		return nil

	case *ast.InfixExpression:
		return nil

	case *ast.VarStatement:
		return nil

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
		return &object.Array{Elements: evalExpressions(node.Elements, env)}

	case *ast.MapLiteral:
		pairs := make(map[object.HashKey]object.Pair)
		for keyNode, valueNode := range node.Pairs {
			key := Eval(keyNode, env)
			hashKey, ok := key.(object.Hasher)
			if !ok {
				return nil
			}
			pairs[hashKey.HashKey()] = object.Pair{Key: key, Value: Eval(valueNode, env)}
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
	}
	return result
}

func evalExpressions(expressions []ast.Expression, env *object.Environment) []object.Object {
	var result []object.Object
	for _, e := range expressions {
		result = append(result, Eval(e, env))
	}
	return result
}
