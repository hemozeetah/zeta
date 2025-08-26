package object

type HashKey struct {
	Type  ObjectType
	Value uint64
}

type Hasher interface {
	HashKey() HashKey
}
