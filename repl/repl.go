package repl

import (
	"bufio"
	"fmt"
	"io"

	"github.com/hemozeetah/zeta/lexer"
	"github.com/hemozeetah/zeta/parser"
)

const PROMPT = ">> "

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)
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
		io.WriteString(out, program.String())
		io.WriteString(out, "\n")
	}
}

func printErrors(out io.Writer, errors []string) {
	for _, msg := range errors {
		io.WriteString(out, "\t"+msg+"\n")
	}
}
