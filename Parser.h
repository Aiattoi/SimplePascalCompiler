#pragma once

#include "Lexer.h"
#include "AST.h"

class InvalidTokenException : public std::exception {
public:
    InvalidTokenException(std::vector<Token> expected, Token got);

    std::string message() const;

    const std::vector<Token> expected;
    const Token got;
private:
    const char *tokenTable[N_OF_TOKENS] = {
            "kwPROGRAM", "kwCONST", "kwVAR", "kwPROCEDURE", "kwFUNCTION", "kwBEGIN", "kwEND", "kwEXIT", "kwFORWARD",
            "kwIF", "kwTHEN", "kwELSE",
            "kwWHILE", "kwDO", "kwBREAK",
            "kwFOR", "kwTO", "kwDOWNTO",
            "kwINTEGER", "kwREAL", "kwSTRING",
            "kwDIV", "kwMOD", "kwNOT", "kwAND", "kwOR", "kwARRAY", "kwOF",
            "eof", "semicolon", "ident", "number", "rnumber", "string",
            "assigment", "colon", "quote", "dot", "comma",
            "equal", "notequal", "less", "lessequal", "more", "moreequal",
            "plus", "minus", "bitOr", "mult", "rDiv", "bitAnd", "bitNot",
            "openParenthesis", "closeParenthesis",
            "openArray", "closeArray",
            "err"
    }; // same order as in token enum
};

class TokenIterator {
public:
    bool init(char *filename);

    void match(Token tok);

    Token peek();

    int getPrevTokVal() const;

    const std::string &getPrevTokName() const;

    const std::string &getPrevTokTextString() const;

private:
    void getNextToken();

    Lexer lexer;

    Token curTok;
    int prevTokVal;
    std::string prevTokName;
    std::string prevTokTextString;
};

class Parser {
public:
    bool init(char *filename);

    std::unique_ptr<ProgramNode> parseProgram(); // parse

    std::unique_ptr<BodyNode> parseBody();

    std::unique_ptr<StatementNode> parseStatement();

    std::unique_ptr<ProcedureCallStatementNode> parseProcedureCall(const std::string &name);

    std::vector<std::unique_ptr<ExpressionNode>> parseCallParams();

    std::unique_ptr<StatementNode> parseIdentifierStatement();

    std::shared_ptr<Type> parseType();

    // lowest precedence
    std::unique_ptr<ExpressionNode> parseExpression();

    std::unique_ptr<ExpressionNode> parseFirstLevelExpression();

    std::unique_ptr<ExpressionNode> parseSecondLevelExpression();

    // highest precedence
    std::unique_ptr<ExpressionNode> parseThirdLevelExpression();

    std::unique_ptr<NumberExpressionNode> parseNumber();

    std::unique_ptr<ExpressionNode> parseIdentifierExpression();

    std::vector<std::unique_ptr<ConstDeclaration>> parseConstantsDeclaration();

    std::vector<std::unique_ptr<VarDeclaration>> parseVariablesDeclaration();

    std::unique_ptr<VarReferenceNode> parseVariableReference(const std::string &name);

    std::unique_ptr<ExpressionNode> parseLiteral();

    std::unique_ptr<AssignStatementNode> parseAssign();

    std::unique_ptr<IfStatementNode> parseIf();

    std::unique_ptr<WhileStatementNode> parseWhile();

    std::unique_ptr<BreakStatementNode> parseBreak();

    std::unique_ptr<ForStatementNode> parseFor();

    std::vector<std::unique_ptr<VarDeclaration>> parseParamsDeclaration();

    std::unique_ptr<ProcedurePrototype> parseProcedurePrototype();

    std::unique_ptr<ProcedureDeclaration> parseProcedure();

    std::unique_ptr<FunctionCallExpressionNode> parseFunctionCall(const std::string &name);

    std::unique_ptr<FunctionPrototype> parseFunctionPrototype();

    std::unique_ptr<FunctionDeclaration> parseFunction();

    std::unique_ptr<ExitStatementNode> parseExit();

    std::unique_ptr<ExpressionNode> parseString();

    std::unique_ptr<ArrayItemReferenceNode> parseArrayReference(const std::string &name);

private:
    TokenIterator tokenizer;
};