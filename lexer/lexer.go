package lexer

import "github.com/hemozeetah/zeta/token"

type Lexer struct {
	input string
	pos   int // current position in input (points to current char)
}

func New(input string) *Lexer {
	return &Lexer{
		input: input,
		pos:   0,
	}
}

func (l *Lexer) Read() token.Token {
	l.skipWhitespaces()
	if l.pos >= len(l.input) {
		return token.New(token.EOF, "")
	}
	ch := l.read()
	s := string(ch) + string(l.peek())
	if tt, ok := mapping[s]; ok {
		defer l.skipLetter()
		defer l.skipLetter()
		return token.New(tt, s)
	}
	s = string(ch)
	if tt, ok := mapping[s]; ok {
		defer l.skipLetter()
		return token.New(tt, s)
	}
	switch {
	case isDigit(ch):
		return l.readNumber()
	case isQuotes(ch):
		return l.readString()
	case isLetter(ch):
		return l.readIdentifier()
	}
	return token.New(token.ILLEGAL, "")
}

func (l *Lexer) read() byte {
	if l.pos >= len(l.input) {
		return 0
	}
	return l.input[l.pos]
}

func (l *Lexer) peek() byte {
	if l.pos+1 >= len(l.input) {
		return 0
	}
	return l.input[l.pos+1]
}

func (l *Lexer) skipLetter() {
	l.pos += 1
}

func (l *Lexer) readNumber() token.Token {
	start := l.pos
	for isDigit(l.read()) {
		l.skipLetter()
	}
	return token.New(token.INT, l.input[start:l.pos])
}

func (l *Lexer) readString() token.Token {
	l.skipLetter()       // skip "
	defer l.skipLetter() // skip "
	start := l.pos
	for !isQuotes(l.read()) {
		if l.read() == 0 {
			return token.New(token.ILLEGAL, "")
		}
		l.skipLetter()
	}
	return token.New(token.STRING, l.input[start:l.pos])
}

func (l *Lexer) readIdentifier() token.Token {
	start := l.pos
	for isLetter(l.read()) || (l.pos != start && isDigit(l.read())) {
		l.skipLetter()
	}
	ident := l.input[start:l.pos]
	if tt, ok := keywords[ident]; ok {
		return token.New(tt, ident)
	}
	return token.New(token.IDENT, ident)
}

func (l *Lexer) skipWhitespaces() {
	for isWhitespace(l.read()) {
		l.skipLetter()
	}
}

func isWhitespace(ch byte) bool {
	return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
}

func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

func isQuotes(ch byte) bool {
	return ch == '"'
}

var mapping = map[string]token.TokenType{
	"=":  token.ASSIGN,
	"+":  token.PLUS,
	"-":  token.MINUS,
	"*":  token.ASTERISK,
	"/":  token.SLASH,
	"==": token.EQ,
	"!=": token.NEQ,
	"<":  token.LT,
	"<=": token.LTE,
	">":  token.GT,
	">=": token.GTE,
	"!":  token.BANG,
	",":  token.COMMA,
	";":  token.SEMICOLON,
	":":  token.COLON,
	"(":  token.LPAREN,
	")":  token.RPAREN,
	"{":  token.LBRACE,
	"}":  token.RBRACE,
	"[":  token.LBRACKET,
	"]":  token.RBRACKET,
}

var keywords = map[string]token.TokenType{
	"var":    token.VAR,
	"if":     token.IF,
	"else":   token.ELSE,
	"fn":     token.FUNCTION,
	"return": token.RETURN,
	"true":   token.TRUE,
	"false":  token.FALSE,
}
