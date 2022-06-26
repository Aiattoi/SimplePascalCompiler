#pragma once

#include <iostream>
#include <cstring>

#define MAX_LINE_LENGTH 257
#define N_OF_TOKENS 57
#define N_OF_KEYWORDS 29

const int MAX_IDENT_LEN = 32;

typedef enum {
    kwPROGRAM, kwCONST, kwVAR, kwPROCEDURE, kwFUNCTION, kwBEGIN, kwEND, kwEXIT, kwFORWARD,
    kwIF, kwTHEN, kwELSE,
    kwWHILE, kwDO, kwBREAK,
    kwFOR, kwTO, kwDOWNTO,
    kwINTEGER, kwREAL, kwSTRING,
    kwDIV, kwMOD, kwNOT, kwAND, kwOR, kwARRAY, kwOF,
    eof, semicolon, ident, number, rnumber, string,
    assigment, colon, quote, dot, comma,
    equal, notequal, less, lessequal, more, moreequal,
    plus, minus, bitOr, mult, rDiv, bitAnd, bitNot,
    openParenthesis, closeParenthesis,
    openArray, closeArray,
    err
} Token;

typedef enum lookAheadType {
    LETTER, NUMBER, WHITE_SPACE, END, UNDERSCORE, NO_TYPE
} LookAheadType;

class Input {
public:

    ~Input();

    bool init(char *filename);

    int readSymbol();

private:
    char line[MAX_LINE_LENGTH];
    int lineNumber = 0;
    char *linePointer = line;
    FILE *inputFile;
    int extendedLine = 0;
};

class Lexer {
public:
    bool init(char *filename);

    Token gettok();

    const std::string &getIdentifierName() const { return this->identifierName; }

    int getNumVal() const { return this->numVal; }

    const std::string &getTextString() const { return this->textString; }

private:
    void readInput();

    Token keyword(const std::string &ident);

    std::string identifierName; /* used to store a name if the token is identifier */
    int numVal; /* used to store a value, if the token is number */
    std::string textString; /* used to store a string constant */

    int lookahead;
    LookAheadType lookaheadType;
    Input input;

    const struct {
        const char *kw;
        Token symb;
    } keyWordTable[N_OF_KEYWORDS] = {
            {"program",   kwPROGRAM},
            {"const",     kwCONST},
            {"var",       kwVAR},
            {"procedure", kwPROCEDURE},
            {"function",  kwFUNCTION},
            {"begin",     kwBEGIN},
            {"end",       kwEND},
            {"exit",      kwEXIT},
            {"forward",   kwFORWARD},
            {"if",        kwIF},
            {"then",      kwTHEN},
            {"else",      kwELSE},
            {"while",     kwWHILE},
            {"do",        kwDO},
            {"break",     kwBREAK},
            {"for",       kwFOR},
            {"to",        kwTO},
            {"downto",    kwDOWNTO},
            {"integer",   kwINTEGER},
            {"real",      kwREAL},
            {"string",    kwSTRING},
            {"div",       kwDIV},
            {"mod",       kwMOD},
            {"not",       kwNOT},
            {"and",       kwAND},
            {"or",        kwOR},
            {"array",     kwARRAY},
            {"of",        kwOF},
            {nullptr,     (Token) 0}
    };
};