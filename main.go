package main

import (
	"os"

	"github.com/hemozeetah/zeta/repl"
)

func main() {
	repl.Start(os.Stdin, os.Stdout)
}
