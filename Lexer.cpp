#include "Lexer.h"

Input::~Input() {
    fclose(inputFile);
}

bool Input::init(char *filename) {
    line[0] = '\0';
    if (!filename) {
        inputFile = stdin;
    } else {
        inputFile = fopen(filename, "r");
        if (!inputFile) {
            std::cerr << "Input file " << filename << " couldn't be opened." << std::endl;
            return false;
        }
    }
    return true;
}

int Input::readSymbol() {
    if (!*linePointer) {
        if (!fgets(line, MAX_LINE_LENGTH, inputFile)) return EOF;

        linePointer = line;
        lineNumber++;

        int lineLength = strlen(line);
        if (extendedLine) {
            lineNumber--;
        }

        extendedLine = line[lineLength - 1] != '\n';
    }
    return *linePointer++;

}

bool Lexer::init(char *filename) {
    if (!input.init(filename))
        return false;
    readInput();
    return true;
}

Token Lexer::gettok() {
    int base = 10;
    Token outToken;
    int identLen;
    int digit;

    start:
    switch (lookahead) {
        case '=':
            outToken = equal;
            readInput();
            return outToken;
        case '<':
            readInput();
            goto less;
        case '>':
            readInput();
            goto more;
        case '+':
            outToken = plus;
            readInput();
            return outToken;
        case '-':
            outToken = minus;
            readInput();
            return outToken;
        case '|':
            outToken = bitOr;
            readInput();
            return outToken;
        case '*':
            outToken = mult;
            readInput();
            return outToken;
        case '/':
            outToken = rDiv;
            readInput();
            return outToken;
        case '&':
            readInput();
            goto ampersand;
        case '$':
            outToken = number;
            base = 16;
            readInput();
            goto xnumber;
        case '~':
            outToken = bitNot;
            readInput();
            return outToken;
        case ':':
            readInput();
            goto colon;
        case '{':
            readInput();
            goto comment;
        case ';':
            outToken = semicolon;
            readInput();
            return outToken;
        case '(':
            outToken = openParenthesis;
            readInput();
            return outToken;
        case ')':
            outToken = closeParenthesis;
            readInput();
            return outToken;
        case '[':
            outToken = openArray;
            readInput();
            return outToken;
        case ']':
            outToken = closeArray;
            readInput();
            return outToken;
        case '\'':
            readInput();
            goto xtext;
        case ',':
            outToken = comma;
            readInput();
            return outToken;
        case '.':
            outToken = dot;
            readInput();
            return outToken;
        default:;
    }
    switch (lookaheadType) {
        case LETTER:
            identLen = 1;
            identifierName[0] = (char) lookahead;
            identifierName[1] = '\0';
            readInput();
            goto identifier;
        case NUMBER:
            outToken = number;
            goto xnumber;
        case WHITE_SPACE:
            readInput();
            goto start;
        case END:
            outToken = eof;
            return outToken;
        default:
            outToken = err;
            std::cerr << "Invalid symbol: '" << lookahead << "'." << std::endl;
            readInput();
            return outToken;
    }

    less:
    switch (lookahead) {
        case '>':
            outToken = notequal;
            readInput();
            return outToken;
        case '=':
            outToken = lessequal;
            readInput();
            return outToken;
        default:
            outToken = less;
            return outToken;
    }

    more:
    switch (lookahead) {
        case '=':
            outToken = moreequal;
            readInput();
            return outToken;
        default:
            outToken = more;
            return outToken;
    }

    colon:
    switch (lookahead) {
        case '=':
            outToken = assigment;
            readInput();
            return outToken;
        default:
            outToken = colon;
            return outToken;
    }

    comment:
    switch (lookahead) {
        case '}':
            readInput();
            goto start;
        default:;
    }
    switch (lookaheadType) {
        case END:
            outToken = err;
            std::cerr << "Unexpected end of file in a comment." << std::endl;
            return outToken;
        default:
            readInput();
            goto comment;
    }

    identifier:
    switch (lookaheadType) {
        case LETTER:
        case NUMBER:
        case UNDERSCORE:
            identifierName[identLen++] = (char) lookahead;
            readInput();
            goto identifier;
        default:
            identifierName[identLen++] = '\0';
            outToken = keyword(identifierName);
            return outToken;
    }

    ampersand:
    switch (lookaheadType) {
        case NUMBER:
            outToken = number;
            base = 8;
            break;
        default:
            outToken = bitAnd;
            return outToken;
    }

    xnumber:
    numVal = lookahead - '0';
    readInput();
    if (numVal == 0 && lookaheadType == NUMBER) {
        outToken = err;
        std::cerr << "Number can't start with zero." << std::endl;
        readInput();
        return outToken;
    }

    number:
    switch (lookaheadType) {
        case LETTER:
        case NUMBER:
            if (lookahead >= 'a')
                digit = (lookahead - 'a') + 10;
            else if (lookahead >= 'A')
                digit = (lookahead - 'A') + 10;
            else
                digit = (lookahead - '0');
            if (digit >= base) {
                outToken = err;
                std::cerr << "Digit not allowed in this base." << std::endl;
                return outToken;
            }
            numVal *= base;
            numVal += digit;
            readInput();
            goto number;
        default:
            return outToken;

    }

    xtext:
    textString.clear();
    text:
    switch (lookahead) {
        case '\'':
            readInput();
            return string;
        case '\\':
            readInput();
            if (lookahead == '\'') {
                textString += '\'';
                readInput();
                goto text;
            }
            textString += '\\';
        default:;
    }
    switch (lookaheadType) {
        case END:
            outToken = err;
            std::cerr << "Unexpected end of file in a string." << std::endl;
            return outToken;
        default:
            textString += (char) lookahead;
            readInput();
            goto text;
    }

    std::cerr << "Lexer error: out of states." << std::endl;
    return err;
}

void Lexer::readInput() {
    lookahead = input.readSymbol();

    if (isalpha(lookahead))
        lookaheadType = LETTER;
    else if (isdigit(lookahead))
        lookaheadType = NUMBER;
    else if (lookahead == EOF)
        lookaheadType = END;
    else if (isspace(lookahead))
        lookaheadType = WHITE_SPACE;
    else if (lookahead == '_')
        lookaheadType = UNDERSCORE;
    else
        lookaheadType = NO_TYPE;
}

Token Lexer::keyword(const std::string &word) {
    int i = 0;
    while (keyWordTable[i].kw) {
        if (!strncmp(word.c_str(), keyWordTable[i].kw, MAX_IDENT_LEN))
            return keyWordTable[i].symb;
        i++;
    }
    return ident;
}
