package parser

import (
	"fmt"
	"strconv"

	"github.com/hemozeetah/zeta/ast"
	"github.com/hemozeetah/zeta/lexer"
	"github.com/hemozeetah/zeta/token"
)

type (
	prefixParseFn func() ast.Expression
	infixParseFn  func(ast.Expression) ast.Expression
)

type Parser struct {
	lexer          *lexer.Lexer
	curToken       token.Token
	peekToken      token.Token
	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn
	errors         []string
}

func New(lexer *lexer.Lexer) *Parser {
	p := &Parser{
		lexer:          lexer,
		curToken:       lexer.Read(),
		peekToken:      lexer.Read(),
		prefixParseFns: make(map[token.TokenType]prefixParseFn),
		infixParseFns:  make(map[token.TokenType]infixParseFn),
	}

	p.registerPrefix(token.INT, p.parseIntegerLiteral)
	p.registerPrefix(token.STRING, p.parseStringLiteral)
	p.registerPrefix(token.TRUE, p.parseBooleanLiteral)
	p.registerPrefix(token.FALSE, p.parseBooleanLiteral)

	p.registerPrefix(token.IDENT, p.parseIdentifier)

	p.registerPrefix(token.PLUS, p.parsePrefixExpression)
	p.registerPrefix(token.MINUS, p.parsePrefixExpression)
	p.registerPrefix(token.BANG, p.parsePrefixExpression)

	p.registerInfix(token.PLUS, p.parseInfixExpression)
	p.registerInfix(token.MINUS, p.parseInfixExpression)
	p.registerInfix(token.SLASH, p.parseInfixExpression)
	p.registerInfix(token.ASTERISK, p.parseInfixExpression)
	p.registerInfix(token.EQ, p.parseInfixExpression)
	p.registerInfix(token.NEQ, p.parseInfixExpression)
	p.registerInfix(token.LT, p.parseInfixExpression)
	p.registerInfix(token.GT, p.parseInfixExpression)

	p.registerPrefix(token.LPAREN, p.parseGroupedExpression)

	p.registerPrefix(token.IF, p.parseIfExpression)

	p.registerPrefix(token.FUNCTION, p.parseFunctionLiteral)
	p.registerInfix(token.LPAREN, p.parseCallExpression)

	p.registerPrefix(token.LBRACKET, p.parseArrayLiteral)
	p.registerPrefix(token.LBRACE, p.parseMapLiteral)

	p.registerInfix(token.LBRACKET, p.parseIndexExpression)

	return p
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) addError(format string, a ...any) {
	p.errors = append(p.errors, fmt.Sprintf(format, a...))
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.lexer.Read()
}

func (p *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}
func (p *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	for p.curToken.Type != token.EOF {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}
	return program
}

func (p *Parser) parseStatement() ast.Statement {
	switch p.curToken.Type {
	case token.VAR:
		return p.parseVarStatement()
	case token.RETURN:
		return p.parseReturnStatement()
	default:
		return p.parseExpressionStatement()
	}
}

func (p *Parser) parseVarStatement() *ast.VarStatement {
	stmt := &ast.VarStatement{Token: p.curToken}
	if !p.expectPeekType(token.IDENT) {
		return nil
	}
	stmt.Name = &ast.Identifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}
	if !p.expectPeekType(token.ASSIGN) {
		return nil
	}
	p.nextToken()
	stmt.Value = p.parseExpression(LOWEST)
	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}
	return stmt
}

func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: p.curToken}
	p.nextToken()
	stmt.ReturnValue = p.parseExpression(LOWEST)
	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}
	return stmt
}

func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: p.curToken}
	stmt.Expression = p.parseExpression(LOWEST)
	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}
	return stmt
}

func (p *Parser) parseExpression(precedence Precedence) ast.Expression {
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.addError("no prefix parse function for %s found", token.TokenMap[p.curToken.Type])
		return nil
	}
	leftRes := prefix()
	for !p.peekTokenIs(token.SEMICOLON) && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftRes
		}
		p.nextToken()
		leftRes = infix(leftRes)
	}
	return leftRes
}

func (p *Parser) parseIntegerLiteral() ast.Expression {
	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		p.addError("could not parse %q as integer", p.curToken.Literal)
		return nil
	}
	return &ast.IntegerLiteral{Token: p.curToken, Value: value}
}

func (p *Parser) parseStringLiteral() ast.Expression {
	return &ast.StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseBooleanLiteral() ast.Expression {
	return &ast.BooleanLiteral{Token: p.curToken, Value: p.curTokenIs(token.TRUE)}
}

func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parsePrefixExpression() ast.Expression {
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}
	p.nextToken()
	expression.Right = p.parseExpression(PREFIX)
	return expression
}

func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	expression := &ast.InfixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}
	precedence := p.curPrecedence()
	p.nextToken()
	expression.Right = p.parseExpression(precedence)
	return expression
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken()
	exp := p.parseExpression(LOWEST)
	if !p.expectPeekType(token.RPAREN) {
		return nil
	}
	return exp
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: p.curToken}
	p.nextToken()
	for !p.curTokenIs(token.RBRACE) {
		if p.curTokenIs(token.EOF) {
			p.addError("missing token %s", token.TokenMap[token.RBRACE])
			return nil
		}
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		p.nextToken()
	}
	return block
}

func (p *Parser) parseIfExpression() ast.Expression {
	expression := &ast.IfExpression{Token: p.curToken}
	if !p.expectPeekType(token.LPAREN) {
		return nil
	}
	p.nextToken()
	expression.Condition = p.parseExpression(LOWEST)
	if !p.expectPeekType(token.RPAREN) {
		return nil
	}
	if !p.expectPeekType(token.LBRACE) {
		return nil
	}
	expression.Consequence = p.parseBlockStatement()
	if p.peekTokenIs(token.ELSE) {
		p.nextToken()
		if !p.expectPeekType(token.LBRACE) {
			return nil
		}
		expression.Alternative = p.parseBlockStatement()
	}
	return expression
}

func (p *Parser) parseFunctionParameters() []*ast.Identifier {
	identifiers := []*ast.Identifier{}
	firstTime := true
	for !p.peekTokenIs(token.RPAREN) {
		if !firstTime && !p.expectPeekType(token.COMMA) {
			return nil
		}
		if !p.expectPeekType(token.IDENT) {
			return nil
		}
		identifiers = append(identifiers, &ast.Identifier{
			Token: p.curToken,
			Value: p.curToken.Literal,
		})
		firstTime = false
	}
	p.nextToken()
	return identifiers
}

func (p *Parser) parseFunctionLiteral() ast.Expression {
	lit := &ast.FunctionLiteral{Token: p.curToken}
	if !p.expectPeekType(token.LPAREN) {
		return nil
	}
	lit.Parameters = p.parseFunctionParameters()
	if !p.expectPeekType(token.LBRACE) {
		return nil
	}
	lit.Body = p.parseBlockStatement()
	return lit
}

func (p *Parser) parseExpressionList(end token.TokenType) []ast.Expression {
	list := []ast.Expression{}
	firstTime := true
	for !p.peekTokenIs(end) {
		if !firstTime && !p.expectPeekType(token.COMMA) {
			return nil
		}
		p.nextToken()
		list = append(list, p.parseExpression(LOWEST))
		firstTime = false
	}
	p.nextToken()
	return list
}

func (p *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	exp := &ast.CallExpression{Token: p.curToken, Function: function}
	exp.Arguments = p.parseExpressionList(token.RPAREN)
	return exp
}

func (p *Parser) parseArrayLiteral() ast.Expression {
	array := &ast.ArrayLiteral{Token: p.curToken}
	array.Elements = p.parseExpressionList(token.RBRACKET)
	return array
}

func (p *Parser) parseMapLiteral() ast.Expression {
	ml := &ast.MapLiteral{
		Token: p.curToken,
		Pairs: make(map[ast.Expression]ast.Expression),
	}
	firstTime := true
	for !p.peekTokenIs(token.RBRACE) {
		if !firstTime && !p.expectPeekType(token.COMMA) {
			return nil
		}
		p.nextToken()
		key := p.parseExpression(LOWEST)
		if !p.expectPeekType(token.COLON) {
			return nil
		}
		p.nextToken()
		value := p.parseExpression(LOWEST)
		ml.Pairs[key] = value
		firstTime = false
	}
	p.nextToken()
	return ml
}

func (p *Parser) parseIndexExpression(left ast.Expression) ast.Expression {
	exp := &ast.IndexExpression{Token: p.curToken, Left: left}
	p.nextToken()
	exp.Index = p.parseExpression(LOWEST)
	if !p.expectPeekType(token.RBRACKET) {
		return nil
	}
	return exp
}

func (p *Parser) curTokenIs(t token.TokenType) bool {
	return p.curToken.Type == t
}

func (p *Parser) peekTokenIs(t token.TokenType) bool {
	return p.peekToken.Type == t
}

func (p *Parser) expectPeekType(t token.TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	}
	p.addError("expected next token to be %s, got %s instead", token.TokenMap[t], token.TokenMap[p.peekToken.Type])
	return false
}

func (p *Parser) peekPrecedence() Precedence {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) curPrecedence() Precedence {
	if p, ok := precedences[p.curToken.Type]; ok {
		return p
	}
	return LOWEST
}
