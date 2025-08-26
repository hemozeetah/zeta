package object

import "fmt"

type Error struct {
	Message string
}

func NewError(format string, a ...any) *Error {
	return &Error{Message: fmt.Sprintf(format, a...)}
}

func IsError(obj Object) bool {
	return obj != nil && obj.Type() == ERROR_OBJ
}

func (e *Error) Type() ObjectType { return ERROR_OBJ }
func (e *Error) Inspect() string  { return "ERROR: " + e.Message }
