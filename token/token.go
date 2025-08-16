package token

type TokenType int

type Token struct {
	Type    TokenType
	Literal string
}

func New(tokenType TokenType, literal string) Token {
	return Token{
		Type:    tokenType,
		Literal: literal,
	}
}

const (
	ILLEGAL TokenType = iota
	EOF
	// Literals
	INT
	STRING
	// Keywords
	VAR
	IF
	ELSE
	FUNCTION
	RETURN
	TRUE
	FALSE
	// Identifiers
	IDENT
	// Operators
	ASSIGN
	PLUS
	MINUS
	ASTERISK
	SLASH
	EQ
	NEQ
	LT
	LTE
	GT
	GTE
	BANG
	// Delimiters
	COMMA
	SEMICOLON
	COLON
	LPAREN
	RPAREN
	LBRACE
	RBRACE
	LBRACKET
	RBRACKET
)

var TokenMap = map[TokenType]string{
	ILLEGAL:   "ILLEGAL",
	EOF:       "EOF",
	INT:       "INT",
	STRING:    "STRING",
	VAR:       "VAR",
	IF:        "IF",
	ELSE:      "ELSE",
	FUNCTION:  "FUNCTION",
	RETURN:    "RETURN",
	TRUE:      "TRUE",
	FALSE:     "FALSE",
	IDENT:     "IDENT",
	ASSIGN:    "ASSIGN",
	PLUS:      "PLUS",
	MINUS:     "MINUS",
	ASTERISK:  "ASTERISK",
	SLASH:     "SLASH",
	EQ:        "EQ",
	NEQ:       "NEQ",
	LT:        "LT",
	LTE:       "LTE",
	GT:        "GT",
	GTE:       "GTE",
	BANG:      "BANG",
	COMMA:     "COMMA",
	SEMICOLON: "SEMICOLON",
	COLON:     "COLON",
	LPAREN:    "LPAREN",
	RPAREN:    "RPAREN",
	LBRACE:    "LBRACE",
	RBRACE:    "RBRACE",
	LBRACKET:  "LBRACKET",
	RBRACKET:  "RBRACKET",
}
