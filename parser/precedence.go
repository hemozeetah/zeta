package parser

import "github.com/hemozeetah/zeta/token"

type Precedence int

const (
	LOWEST      Precedence = iota
	EQUALS                 // ==
	LESSGREATER            // >=, >, <=, or <
	SUM                    // +, -
	PRODUCT                // *, /
	PREFIX                 // -X or !X
	CALL                   // myFunction(X)
	INDEX                  // array[index]
)

var precedences = map[token.TokenType]Precedence{
	token.EQ:       EQUALS,
	token.NEQ:      EQUALS,
	token.LT:       LESSGREATER,
	token.LTE:      LESSGREATER,
	token.GT:       LESSGREATER,
	token.GTE:      LESSGREATER,
	token.PLUS:     SUM,
	token.MINUS:    SUM,
	token.ASTERISK: PRODUCT,
	token.SLASH:    PRODUCT,
	token.LPAREN:   CALL,
	token.LBRACKET: INDEX,
}
