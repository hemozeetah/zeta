package repl

import (
	"bufio"
	"fmt"
	"io"

	"github.com/hemozeetah/zeta/evaluator"
	"github.com/hemozeetah/zeta/lexer"
	"github.com/hemozeetah/zeta/object"
	"github.com/hemozeetah/zeta/parser"
)

const PROMPT = ">> "

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)
	env := object.NewEnvironment()
	for {
		fmt.Fprintf(out, PROMPT)
		scanned := scanner.Scan()
		if !scanned {
			return
		}
		line := scanner.Text()
		p := parser.New(lexer.New(line))
		program := p.ParseProgram()
		if len(p.Errors()) != 0 {
			printErrors(out, p.Errors())
			continue
		}
		evaluated := evaluator.Eval(program, env)
		if evaluated != nil {
			io.WriteString(out, evaluated.Inspect())
			io.WriteString(out, "\n")
		}
	}
}

func printErrors(out io.Writer, errors []string) {
	for _, msg := range errors {
		io.WriteString(out, "\t"+msg+"\n")
	}
}
