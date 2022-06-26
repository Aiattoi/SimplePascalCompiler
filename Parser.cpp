#include "Parser.h"

#include <utility>

InvalidTokenException::InvalidTokenException(std::vector<Token> expected, Token got) : expected(std::move(expected)),
                                                                                       got(got) {
}

std::string InvalidTokenException::message() const {
    std::string response = "Expected: <";
    size_t i = 0;
    response.append(tokenTable[expected[i++]]);
    while (i < expected.size()) {
        response.append(", ");
        response.append(tokenTable[expected[i++]]);
    }
    response.append(">. Got: <");
    response.append(tokenTable[got]);
    response.append(">.");
    return response;
}

bool TokenIterator::init(char *filename) {
    if (!lexer.init(filename))
        return false;
    getNextToken();
    return true;
}

void TokenIterator::getNextToken() {
    curTok = lexer.gettok();
}

void TokenIterator::match(Token tok) {
    if (tok != curTok)
        throw InvalidTokenException({tok}, curTok);
    prevTokName.assign(lexer.getIdentifierName().c_str()); // c_str to hardcopy
    prevTokVal = lexer.getNumVal();
    prevTokTextString.assign(lexer.getTextString().c_str()); // c_str to hardcopy
    getNextToken();
}

Token TokenIterator::peek() {
    return curTok;
}

int TokenIterator::getPrevTokVal() const {
    return prevTokVal;
}

const std::string &TokenIterator::getPrevTokName() const {
    return prevTokName;
}

const std::string &TokenIterator::getPrevTokTextString() const {
    return prevTokTextString;
}

bool Parser::init(char *filename) {
    return tokenizer.init(filename);
}

std::unique_ptr<ProgramNode> Parser::parseProgram() {
    //parse program statement
    tokenizer.match(kwPROGRAM);
    tokenizer.match(ident);
    std::string name = tokenizer.getPrevTokName();
    tokenizer.match(semicolon);
    //parse declarations
    std::vector<std::unique_ptr<Declaration>> declarations;
    Token curTok = tokenizer.peek();
    // add procedures and functions declarations !
    while (curTok == kwCONST || curTok == kwVAR || curTok == kwPROCEDURE || curTok == kwFUNCTION) {
        if (curTok == kwCONST) {
            auto consts = parseConstantsDeclaration();
            for (auto &constant: consts)
                declarations.push_back(std::move(constant));
        } else if (curTok == kwVAR) {
            auto vars = parseVariablesDeclaration();
            for (auto &var: vars)
                declarations.push_back(std::move(var));
        } else if (curTok == kwPROCEDURE)
            declarations.push_back(std::move(parseProcedure()));
        else
            declarations.push_back(std::move(parseFunction()));
        curTok = tokenizer.peek();
    }
    //parse body
    std::unique_ptr<BodyNode> body = parseBody();
    tokenizer.match(dot);
    return std::make_unique<ProgramNode>(name, std::move(declarations), std::move(body));

}

std::unique_ptr<BodyNode> Parser::parseBody() {
    std::vector<std::unique_ptr<StatementNode>> statements;
    // parse one statement
    if (tokenizer.peek() != kwBEGIN) {
        statements.push_back(parseStatement());
        return std::make_unique<BodyNode>(std::move(statements));
    }
    // parse compound statement
    tokenizer.match(kwBEGIN);
    // empty compound statement
    if (tokenizer.peek() == kwEND)
        return std::make_unique<BodyNode>(std::move(statements));

    // 1st statement
    statements.push_back(parseStatement());

    // next statements
    while (tokenizer.peek() == semicolon) {
        tokenizer.match(semicolon);
        if (tokenizer.peek() == kwEND)
            break;
        statements.push_back(parseStatement());
    }

    tokenizer.match(kwEND);
    return std::make_unique<BodyNode>(std::move(statements));
}

std::unique_ptr<StatementNode> Parser::parseStatement() {
    switch (tokenizer.peek()) {
        case kwIF:
            return parseIf();
        case kwWHILE:
            return parseWhile();
        case kwFOR:
            return parseFor();
        case kwEXIT:
            return parseExit();
        case kwBREAK:
            return parseBreak();
        case ident:
            return parseIdentifierStatement();
        default:
            throw InvalidTokenException({kwIF, kwWHILE, kwFOR, kwBREAK, ident}, tokenizer.peek());
    }
}

std::unique_ptr<ProcedureCallStatementNode> Parser::parseProcedureCall(const std::string &name) {
    tokenizer.match(openParenthesis);
    auto params = parseCallParams();
    tokenizer.match(closeParenthesis);
    return std::make_unique<ProcedureCallStatementNode>(name, std::move(params));
}

std::vector<std::unique_ptr<ExpressionNode>> Parser::parseCallParams() {
    std::vector<std::unique_ptr<ExpressionNode>> ret;
    if (tokenizer.peek() != closeParenthesis)
        ret.push_back(std::move(parseExpression()));
    while (tokenizer.peek() != closeParenthesis) {
        tokenizer.match(comma);
        ret.push_back(std::move(parseExpression()));
    }
    return ret;
}

std::unique_ptr<StatementNode> Parser::parseIdentifierStatement() {
    tokenizer.match(ident);

    if (tokenizer.peek() == openParenthesis)
        return parseProcedureCall(tokenizer.getPrevTokName().c_str());

    return parseAssign();
}

std::shared_ptr<Type> Parser::parseType() {
    Token curTok = tokenizer.peek();
    if (curTok == kwARRAY) {
        // ARRAY[1..10] OF INTEGER;
        tokenizer.match(kwARRAY);
        tokenizer.match(openArray);
        std::unique_ptr<NumberExpressionNode> from = parseNumber();
        tokenizer.match(dot);
        tokenizer.match(dot);
        std::unique_ptr<NumberExpressionNode> to = parseNumber();
        tokenizer.match(closeArray);
        tokenizer.match(kwOF);
        return std::make_shared<Array>(std::move(from), std::move(to), parseType());
    }
    if (curTok == kwINTEGER) {
        tokenizer.match(kwINTEGER);
        return std::make_shared<Integer>();
    }

    throw InvalidTokenException({kwARRAY, kwINTEGER}, curTok);
}

std::unique_ptr<ExpressionNode> Parser::parseExpression() {
    std::unique_ptr<ExpressionNode> res = parseFirstLevelExpression();

    while (true) {
        switch (tokenizer.peek()) {
            case equal:
                tokenizer.match(equal);
                res = std::make_unique<BinOpNode>(std::move(res), parseFirstLevelExpression(),
                                                  BinOpNode::binOperation::bo_equal);
                continue;
            case notequal:
                tokenizer.match(notequal);
                res = std::make_unique<BinOpNode>(std::move(res), parseFirstLevelExpression(),
                                                  BinOpNode::binOperation::bo_notequal);
                continue;
            case less:
                tokenizer.match(less);
                res = std::make_unique<BinOpNode>(std::move(res), parseFirstLevelExpression(),
                                                  BinOpNode::binOperation::bo_less);
                continue;
            case lessequal:
                tokenizer.match(lessequal);
                res = std::make_unique<BinOpNode>(std::move(res), parseFirstLevelExpression(),
                                                  BinOpNode::binOperation::bo_lessequal);
                continue;
            case more:
                tokenizer.match(more);
                res = std::make_unique<BinOpNode>(std::move(res), parseFirstLevelExpression(),
                                                  BinOpNode::binOperation::bo_more);
                continue;
            case moreequal:
                tokenizer.match(moreequal);
                res = std::make_unique<BinOpNode>(std::move(res), parseFirstLevelExpression(),
                                                  BinOpNode::binOperation::bo_moreequal);
                continue;
            default:
                break;
        }
        break;
    }

    return res;
}

std::unique_ptr<ExpressionNode> Parser::parseFirstLevelExpression() {
    std::unique_ptr<ExpressionNode> res = parseSecondLevelExpression();

    while (true) {
        switch (tokenizer.peek()) {
            case plus:
                tokenizer.match(plus);
                res = std::make_unique<BinOpNode>(std::move(res), parseSecondLevelExpression(),
                                                  BinOpNode::binOperation::bo_plus);
                continue;
            case minus:
                tokenizer.match(minus);
                res = std::make_unique<BinOpNode>(std::move(res), parseSecondLevelExpression(),
                                                  BinOpNode::binOperation::bo_minus);
                continue;
            case kwOR:
                tokenizer.match(kwOR);
                res = std::make_unique<BinOpNode>(std::move(res), parseSecondLevelExpression(),
                                                  BinOpNode::binOperation::bo_or);
                continue;
            case bitOr:
                tokenizer.match(bitOr);
                res = std::make_unique<BinOpNode>(std::move(res), parseSecondLevelExpression(),
                                                  BinOpNode::binOperation::bo_bitOr);
                continue;
            default:
                break;
        }
        break;
    }

    return res;
}

std::unique_ptr<ExpressionNode> Parser::parseSecondLevelExpression() {
    std::unique_ptr<ExpressionNode> res = parseThirdLevelExpression();

    while (true) {
        switch (tokenizer.peek()) {
            case mult:
                tokenizer.match(mult);
                res = std::make_unique<BinOpNode>(std::move(res), parseThirdLevelExpression(),
                                                  BinOpNode::binOperation::bo_mult);
                continue;
            case rDiv:
                tokenizer.match(rDiv);
                res = std::make_unique<BinOpNode>(std::move(res), parseThirdLevelExpression(),
                                                  BinOpNode::binOperation::bo_rDiv);
                continue;
            case kwDIV:
                tokenizer.match(kwDIV);
                res = std::make_unique<BinOpNode>(std::move(res), parseThirdLevelExpression(),
                                                  BinOpNode::binOperation::bo_div);
                continue;
            case kwMOD:
                tokenizer.match(kwMOD);
                res = std::make_unique<BinOpNode>(std::move(res), parseThirdLevelExpression(),
                                                  BinOpNode::binOperation::bo_mod);
                continue;
            case kwAND:
                tokenizer.match(kwAND);
                res = std::make_unique<BinOpNode>(std::move(res), parseThirdLevelExpression(),
                                                  BinOpNode::binOperation::bo_and);
                continue;
            case bitAnd:
                tokenizer.match(bitAnd);
                res = std::make_unique<BinOpNode>(std::move(res), parseThirdLevelExpression(),
                                                  BinOpNode::binOperation::bo_bitAnd);
                continue;
            default:
                break;
        }
        break;
    }

    return res;
}

std::unique_ptr<ExpressionNode> Parser::parseThirdLevelExpression() {
    switch (tokenizer.peek()) {
        case bitNot:
            tokenizer.match(bitNot);
            return std::make_unique<UnOpNode>(parseLiteral(),
                                              UnOpNode::unOperation::uo_bitNot);
        case kwNOT:
            tokenizer.match(kwNOT);
            return std::make_unique<UnOpNode>(parseLiteral(),
                                              UnOpNode::unOperation::uo_not);
        default:
            return parseLiteral();
    }
}

std::unique_ptr<NumberExpressionNode> Parser::parseNumber() {
    bool isNegative = false;
    if (tokenizer.peek() == minus) {
        isNegative = true;
        tokenizer.match(minus);
    }
    tokenizer.match(number);

    return std::make_unique<NumberExpressionNode>(isNegative ? -tokenizer.getPrevTokVal() : tokenizer.getPrevTokVal());
}

std::unique_ptr<ExpressionNode> Parser::parseIdentifierExpression() {
    tokenizer.match(ident);
    std::string identName = tokenizer.getPrevTokName();
    if (tokenizer.peek() == openParenthesis) {
        return parseFunctionCall(identName);
    }
    return parseVariableReference(identName);
}

std::vector<std::unique_ptr<ConstDeclaration>> Parser::parseConstantsDeclaration() {
    std::vector<std::unique_ptr<ConstDeclaration>> res;

    tokenizer.match(kwCONST);
    do {
        tokenizer.match(ident);
        std::string identName = tokenizer.getPrevTokName();
        tokenizer.match(equal);
        res.push_back(std::move(std::make_unique<ConstDeclaration>(identName, parseNumber()->getValue())));
        tokenizer.match(semicolon);
    } while (tokenizer.peek() == ident);

    return res;
}

std::vector<std::unique_ptr<VarDeclaration>> Parser::parseVariablesDeclaration() {
    std::vector<std::unique_ptr<VarDeclaration>> res;

    // VAR firstIdent
    tokenizer.match(kwVAR);
    do {
        std::vector<std::string> varNames;
        tokenizer.match(ident);
        varNames.push_back(tokenizer.getPrevTokName());

        // , secondIdent, thirdIdent ...
        while (tokenizer.peek() == comma) {
            tokenizer.match(comma);
            tokenizer.match(ident);
            varNames.push_back(tokenizer.getPrevTokName());
        }

        // : TYPE;
        tokenizer.match(colon);
        auto type = parseType();
        tokenizer.match(semicolon);

        for (auto &name: varNames)
            res.push_back(std::move(std::make_unique<VarDeclaration>(name, type)));
    } while (tokenizer.peek() == ident);

    return res;
}

std::unique_ptr<VarReferenceNode> Parser::parseVariableReference(const std::string &name) {
    if (tokenizer.peek() == openArray)
        return parseArrayReference(name);
    return std::make_unique<SimpleVarReferenceNode>(name.c_str());
}

std::unique_ptr<ExpressionNode> Parser::parseLiteral() {
    Token curTok = tokenizer.peek();
    // number
    if (curTok == number || curTok == minus)
        return parseNumber();
    // ident
    if (curTok == ident)
        return parseIdentifierExpression();
    // string
    if (curTok == string)
        return parseString();
    // brackets
    tokenizer.match(openParenthesis);
    auto res = parseExpression();
    tokenizer.match(closeParenthesis);
    return res;
}

std::unique_ptr<AssignStatementNode> Parser::parseAssign() {
    std::string identName = tokenizer.getPrevTokName();
    std::unique_ptr<VarReferenceNode> ref = parseVariableReference(identName);
    tokenizer.match(assigment);
    return std::make_unique<AssignStatementNode>(std::move(ref), parseExpression());
}

std::unique_ptr<IfStatementNode> Parser::parseIf() {
    tokenizer.match(kwIF);
    std::unique_ptr<ExpressionNode> condition = parseExpression();

    tokenizer.match(kwTHEN);
    std::unique_ptr<BodyNode> thenBody = parseBody();

    std::unique_ptr<BodyNode> elseBody = nullptr;

    if (tokenizer.peek() == kwELSE) {
        tokenizer.match(kwELSE);
        elseBody = parseBody();
    }
    return std::make_unique<IfStatementNode>(std::move(condition), std::move(thenBody), std::move(elseBody));
}

std::unique_ptr<WhileStatementNode> Parser::parseWhile() {
    tokenizer.match(kwWHILE);

    std::unique_ptr<ExpressionNode> cond = parseExpression();

    tokenizer.match(kwDO);

    std::unique_ptr<BodyNode> whileBody = parseBody();
    return std::make_unique<WhileStatementNode>(std::move(cond), std::move(whileBody));
}

std::unique_ptr<BreakStatementNode> Parser::parseBreak() {
    tokenizer.match(kwBREAK);
    return std::make_unique<BreakStatementNode>();
}

std::unique_ptr<ForStatementNode> Parser::parseFor() {
    // for
    tokenizer.match(kwFOR);
    // i
    tokenizer.match(ident);
    std::string identName = tokenizer.getPrevTokName();
    // :=
    tokenizer.match(assigment);
    // 0
    std::unique_ptr<ExpressionNode> from = parseExpression();
    // to/downto
    bool isAsc = true;
    Token curTok = tokenizer.peek();
    if (curTok == kwTO) {
        tokenizer.match(kwTO);
        isAsc = true;
    } else if (curTok == kwDOWNTO) {
        tokenizer.match(kwDOWNTO);
        isAsc = false;
    } else
        throw InvalidTokenException({kwTO, kwDOWNTO}, curTok);

    // n
    std::unique_ptr<ExpressionNode> to = parseExpression();
    // do
    tokenizer.match(kwDO);
    // body
    std::unique_ptr<BodyNode> forBody = parseBody();

    return std::make_unique<ForStatementNode>(identName, std::move(from), isAsc, std::move(to), std::move(forBody));
}

std::vector<std::unique_ptr<VarDeclaration>> Parser::parseParamsDeclaration() {
    std::vector<std::unique_ptr<VarDeclaration>> res;
    std::string paramName;

    // firstParam : type
    tokenizer.match(ident);
    paramName = tokenizer.getPrevTokName();
    tokenizer.match(colon);
    res.push_back(std::move(std::make_unique<VarDeclaration>(paramName, std::move(parseType()))));
    // ; secondParam : type; thirdParam : type ...
    while (tokenizer.peek() == semicolon) {
        tokenizer.match(semicolon);
        tokenizer.match(ident);
        paramName = tokenizer.getPrevTokName();
        tokenizer.match(colon);
        res.push_back(std::move(std::make_unique<VarDeclaration>(paramName, std::move(parseType()))));
    }

    return res;
}

std::unique_ptr<ProcedurePrototype> Parser::parseProcedurePrototype() {
    // procedure procName(params);
    tokenizer.match(kwPROCEDURE);
    tokenizer.match(ident);
    std::string procName = tokenizer.getPrevTokName();
    tokenizer.match(openParenthesis);
    std::vector<std::unique_ptr<VarDeclaration>> params = std::move(parseParamsDeclaration());
    tokenizer.match(closeParenthesis);
    tokenizer.match(semicolon);

    return std::make_unique<ProcedurePrototype>(procName, std::move(params));
}

std::unique_ptr<ProcedureDeclaration> Parser::parseProcedure() {
    std::unique_ptr<ProcedurePrototype> prototype = parseProcedurePrototype();
    std::vector<std::unique_ptr<Declaration>> declarations;
    std::unique_ptr<BodyNode> body = nullptr;

    if (tokenizer.peek() == kwFORWARD) {
        tokenizer.match(kwFORWARD);
        tokenizer.match(semicolon);
        return std::make_unique<ProcedureDeclaration>(std::move(prototype), std::move(declarations),
                                                      std::move(body));
    }

    Token curTok = tokenizer.peek();
    while (curTok == kwVAR || curTok == kwCONST) {
        if (curTok == kwVAR) {
            auto varDeclarations = parseVariablesDeclaration();
            for (auto &varDeclaration: varDeclarations)
                declarations.push_back(std::move(varDeclaration));
        } else {
            auto constDeclarations = parseConstantsDeclaration();
            for (auto &constDeclaration: constDeclarations)
                declarations.push_back(std::move(constDeclaration));
        }
        curTok = tokenizer.peek();
    }

    body = parseBody();
    tokenizer.match(semicolon);

    return std::make_unique<ProcedureDeclaration>(std::move(prototype), std::move(declarations), std::move(body));
}

std::unique_ptr<FunctionCallExpressionNode> Parser::parseFunctionCall(const std::string &name) {
    tokenizer.match(openParenthesis);
    auto params = parseCallParams();
    tokenizer.match(closeParenthesis);
    return std::make_unique<FunctionCallExpressionNode>(name, std::move(params));
}

std::unique_ptr<FunctionPrototype> Parser::parseFunctionPrototype() {
    // function funcName(params) : returnType;
    tokenizer.match(kwFUNCTION);
    tokenizer.match(ident);
    std::string funcName = tokenizer.getPrevTokName();
    tokenizer.match(openParenthesis);
    std::vector<std::unique_ptr<VarDeclaration>> params = std::move(parseParamsDeclaration());
    tokenizer.match(closeParenthesis);
    tokenizer.match(colon);
    std::shared_ptr<Type> type = parseType();
    tokenizer.match(semicolon);

    return std::make_unique<FunctionPrototype>(funcName, std::move(params), std::move(type));
}

std::unique_ptr<FunctionDeclaration> Parser::parseFunction() {
    std::unique_ptr<FunctionPrototype> prototype = parseFunctionPrototype();
    std::vector<std::unique_ptr<Declaration>> declarations;
    std::unique_ptr<BodyNode> body = nullptr;

    if (tokenizer.peek() == kwFORWARD) {
        tokenizer.match(kwFORWARD);
        tokenizer.match(semicolon);
        return std::make_unique<FunctionDeclaration>(std::move(prototype), std::move(declarations),
                                                     std::move(body));
    }

    Token curTok = tokenizer.peek();
    while (curTok == kwVAR || curTok == kwCONST) {
        if (curTok == kwVAR) {
            auto varDeclarations = parseVariablesDeclaration();
            for (auto &varDeclaration: varDeclarations)
                declarations.push_back(std::move(varDeclaration));
        } else {
            auto constDeclarations = parseConstantsDeclaration();
            for (auto &constDeclaration: constDeclarations)
                declarations.push_back(std::move(constDeclaration));
        }
        curTok = tokenizer.peek();
    }

    body = parseBody();
    tokenizer.match(semicolon);

    return std::make_unique<FunctionDeclaration>(std::move(prototype), std::move(declarations), std::move(body));
}

std::unique_ptr<ExitStatementNode> Parser::parseExit() {
    tokenizer.match(kwEXIT);
    return std::make_unique<ExitStatementNode>();
}

std::unique_ptr<ExpressionNode> Parser::parseString() {
    tokenizer.match(string);
    return std::make_unique<StringExpressionNode>(tokenizer.getPrevTokTextString());
}

std::unique_ptr<ArrayItemReferenceNode> Parser::parseArrayReference(const std::string &name) {
    std::vector<std::unique_ptr<ExpressionNode>> indices;
    tokenizer.match(openArray);
    indices.push_back(std::move(parseExpression()));
    tokenizer.match(closeArray);
    while (tokenizer.peek() == openArray) {
        tokenizer.match(openArray);
        indices.push_back(std::move(parseExpression()));
        tokenizer.match(closeArray);
    }
    return std::make_unique<ArrayItemReferenceNode>(name.c_str(), std::move(indices));
}
