package object

var (
	TRUE  = &boolean{Value: true}
	FALSE = &boolean{Value: false}
	NULL =  &null{}
)

func BooleanConstant(value bool) *boolean {
	if value {
		return TRUE
	}
	return FALSE
}

func NullConstant() *null {
	return NULL
}

func IsTruthy(obj Object) bool {
	switch obj {
	case TRUE:
		return true
	case FALSE:
		return false
	case NULL:
		return false
	}
	switch obj := obj.(type) {
	case *Integer:
		return obj.Value != 0
	case *String:
		return len(obj.Value) != 0
	case *Array:
		return len(obj.Elements) != 0
	case *Map:
		return len(obj.Pairs) != 0
	default:
		return true
	}
}
