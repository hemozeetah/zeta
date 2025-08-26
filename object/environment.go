package object

type Environment struct {
	vars map[string]Object
	outer *Environment
}

func NewEnvironment() *Environment {
	return &Environment{vars: make(map[string]Object), outer: nil}
}

func NewEnclosedEnvironment(outer *Environment) *Environment {
	return &Environment{vars: make(map[string]Object), outer: outer}
}

func (e *Environment) Get(name string) (Object, bool) {
	obj, ok := e.vars[name]
	if !ok && e.outer != nil {
		obj, ok = e.outer.Get(name)
	}
	return obj, ok
}

func (e *Environment) Set(name string, val Object) Object {
	e.vars[name] = val
	return val
}
