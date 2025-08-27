package evaluator

import (
	"testing"

	"github.com/hemozeetah/zeta/lexer"
	"github.com/hemozeetah/zeta/object"
	"github.com/hemozeetah/zeta/parser"
)

func testEval(input string) object.Object {
	return Eval(parser.New(lexer.New(input)).ParseProgram(), object.NewEnvironment())
}

func TestEvalIntegerExpression(t *testing.T) {
	tests := []struct {
		input    string
		expected int64
	}{
		{"5", 5},
		{"10", 10},
		{"-5", -5},
		{"-10", -10},
		{"5 + 5 + 5 + 5 - 10", 10},
		{"2 * 2 * 2 * 2 * 2", 32},
		{"-50 + 100 + -50", 0},
		{"5 * 2 + 10", 20},
		{"5 + 2 * 10", 25},
		{"20 + 2 * -10", 0},
		{"50 / 2 * 2 + 10", 60},
		{"2 * (5 + 10)", 30},
		{"3 * 3 * 3 + 10", 37},
		{"3 * (3 * 3) + 10", 37},
		{"(5 + 10 * 2 + 15 / 3) * 2 + -10", 50},
	}

	for _, tt := range tests {
		testIntegerObject(t, testEval(tt.input), tt.expected)
	}
}

func testIntegerObject(t *testing.T, obj object.Object, expected int64) bool {
	result, ok := obj.(*object.Integer)
	if !ok {
		t.Errorf("object is not Integer. got=%T (%+v)", obj, obj)
		return false
	}
	if result.Value != expected {
		t.Errorf("object has wrong value. got=%d, want=%d", result.Value, expected)
		return false
	}
	return true
}

func TestStringLiteral(t *testing.T) {
	input := `"Hello World!"`

	evaluated := testEval(input)
	str, ok := evaluated.(*object.String)
	if !ok {
		t.Fatalf("object is not String. got=%T (%+v)", evaluated, evaluated)
	}
	if str.Value != "Hello World!" {
		t.Errorf("String has wrong value. got=%q", str.Value)
	}
}

func TestStringConcatenation(t *testing.T) {
	input := `"Hello" + " " + "World!"`

	evaluated := testEval(input)
	str, ok := evaluated.(*object.String)
	if !ok {
		t.Fatalf("object is not String. got=%T (%+v)", evaluated, evaluated)
	}
	if str.Value != "Hello World!" {
		t.Errorf("String has wrong value. got=%q", str.Value)
	}
}

func TestEvalBooleanExpression(t *testing.T) {
	tests := []struct {
		input    string
		expected bool
	}{
		{"true", true},
		{"false", false},
		{"1 < 2", true},
		{"1 > 2", false},
		{"1 < 1", false},
		{"1 > 1", false},
		{"1 <= 2", true},
		{"1 >= 2", false},
		{"1 <= 1", true},
		{"1 >= 1", true},
		{"1 == 1", true},
		{"1 != 1", false},
		{"1 == 2", false},
		{"1 != 2", true},
		{"true == true", true},
		{"false == false", true},
		{"true == false", false},
		{"true != false", true},
		{"false != true", true},
		{"(1 < 2) == true", true},
		{"(1 < 2) == false", false},
		{"(1 > 2) == true", false},
		{"(1 > 2) == false", true},
		//
		{"1", true},
		{"0", false},
		{"3", true},
		{`"foo"`, true},
		{`""`, false},
		{"[]", false},
		{"[1]", true},
		{"[1, 2]", true},
		//
		{`"" == ""`, true},
		{`"foo" == "foo"`, true},
		{`"foo" != "foo"`, false},
		{`"foo" < "foo"`, false},
		{`"foo" <= "foo"`, true},
		{`"foo" > "foo"`, false},
		{`"foo" >= "foo"`, true},
		{`"foobar" == "foo"`, false},
		{`"foobar" != "foo"`, true},
		{`"foobar" < "foo"`, false},
		{`"foobar" <= "foo"`, false},
		{`"foobar" > "foo"`, true},
		{`"foobar" >= "foo"`, true},
		//
		{"[1, 2, 3] == [1, 2, 3]", true},
		{"[1, 2, 3] != [1, 2, 3]", false},
		{"[1, 2, 3] < [1, 2, 3]", false},
		{"[1, 2, 3] <= [1, 2, 3]", true},
		{"[1, 2, 3] > [1, 2, 3]", false},
		{"[1, 2, 3] >= [1, 2, 3]", true},
		{"[1, 2] == [1, 2, 3]", false},
		{"[1, 2] != [1, 2, 3]", true},
		{"[1, 2] < [1, 2, 3]", true},
		{"[1, 2] <= [1, 2, 3]", true},
		{"[1, 2] > [1, 2, 3]", false},
		{"[1, 2] >= [1, 2, 3]", false},
	}

	for _, tt := range tests {
		testBooleanObject(t, testEval(tt.input), tt.expected)
	}
}

func testBooleanObject(t *testing.T, obj object.Object, expected bool) bool {
	if object.IsTruthy(obj) != expected {
		t.Fatalf("object %s has wrong truthy. got=%t, want=%t", obj.Inspect(), object.IsTruthy(obj), expected)
		return false
	}
	return true
}

func TestBangOperator(t *testing.T) {
	tests := []struct {
		input    string
		expected bool
	}{
		{"!true", false},
		{"!false", true},
		{"!5", false},
		{"!!true", true},
		{"!!false", false},
		{"!!5", true},
		{`!""`, true},
		{`!"aa"`, false},
	}

	for _, tt := range tests {
		testBooleanObject(t, testEval(tt.input), tt.expected)
	}
}

func TestVarStatements(t *testing.T) {
	tests := []struct {
		input    string
		expected int64
	}{
		{"var a = 5; a;", 5},
		{"var a = 5 * 5; a;", 25},
		{"var a = 5; var b = a; b;", 5},
		{"var a = 5; var b = a; var c = a + b + 5; c;", 15},
	}

	for _, tt := range tests {
		testIntegerObject(t, testEval(tt.input), tt.expected)
	}
}

func TestArrayLiterals(t *testing.T) {
	input := "[1, 2 * 2, 3 + 3]"

	evaluated := testEval(input)
	result, ok := evaluated.(*object.Array)
	if !ok {
		t.Fatalf("object is not Array. got=%T (%+v)", evaluated, evaluated)
	}
	if len(result.Elements) != 3 {
		t.Fatalf("array has wrong num of elements. got=%d", len(result.Elements))
	}
	testIntegerObject(t, result.Elements[0], 1)
	testIntegerObject(t, result.Elements[1], 4)
	testIntegerObject(t, result.Elements[2], 6)
}

func TestMapLiterals(t *testing.T) {
	input := `
var two = "two";
{
"one": 10 - 9,
two: 1 + 1,
"thr" + "ee": 6 / 2,
4: 4,
true: 5,
false: 6
}`

	evaluated := testEval(input)
	result, ok := evaluated.(*object.Map)
	if !ok {
		t.Fatalf("Eval didn't return Map. got=%T (%+v)", evaluated, evaluated)
	}

	expected := map[object.HashKey]int64{
		(&object.String{Value: "one"}).HashKey():   1,
		(&object.String{Value: "two"}).HashKey():   2,
		(&object.String{Value: "three"}).HashKey(): 3,
		(&object.Integer{Value: 4}).HashKey():      4,
		object.TRUE.HashKey():                      5,
		object.FALSE.HashKey():                     6,
	}

	if len(result.Pairs) != len(expected) {
		t.Fatalf("Map has wrong num of pairs. got=%d", len(result.Pairs))
	}
	for expectedKey, expectedValue := range expected {
		pair, ok := result.Pairs[expectedKey]
		if !ok {
			t.Errorf("no pair for given key in Pairs")
		}
		testIntegerObject(t, pair.Value, expectedValue)
	}
}
