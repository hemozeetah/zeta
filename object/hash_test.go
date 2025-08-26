package object

import "testing"

func TestStringHashKey(t *testing.T) {
	tests := []struct {
		first  *String
		second *String
		equal  bool
	}{
		{
			first:  &String{Value: "Hello World"},
			second: &String{Value: "Hello World"},
			equal:  true,
		},
		{
			first:  &String{Value: "foobar"},
			second: &String{Value: "foobar"},
			equal:  true,
		},
		{
			first:  &String{Value: "foo"},
			second: &String{Value: "bar"},
			equal:  false,
		},
	}


	for _, tt := range tests {
		if (tt.first.HashKey() == tt.second.HashKey()) != tt.equal {
			t.Errorf("strings %s and %s expcted hash equals to be %t", tt.first.Inspect(), tt.second.Inspect(), tt.equal)
		}
	}
}
