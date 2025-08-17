.DEFAULT_GOAL := run

run:
	go run main.go

test:
	go test ./lexer
	go test ./ast
