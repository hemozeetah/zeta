.DEFAULT_GOAL := run

run:
	go run main.go

test:
	go test ./lexer
	go test ./ast
	go test ./parser
	go test ./object
	go test ./evaluator
