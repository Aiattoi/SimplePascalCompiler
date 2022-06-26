#include "Parser.h"

int main(int argc, char *argv[]) {
    char *fileName;
    Parser parser;

    fileName = argv[1];

    if (!parser.init(fileName)) {
        std::cerr << "[main] Error creating parser." << std::endl;
        return 2;
    }

    try {
        auto res = parser.parseProgram();
        res->translate();
        res->printLLVM();
    }
    catch (InvalidTokenException &e) {
        std::cerr << e.message() << std::endl;
        return 3;
    }
    catch (UnknownVariableException &e) {
        std::cerr << e.message() << std::endl;
        return 4;
    }
    catch (ConstReferenceException &e) {
        std::cerr << e.message() << std::endl;
        return 5;
    }
    catch (MissingBinaryOperandException &e) {
        std::cerr << e.message() << std::endl;
        return 6;
    }
    catch (MissingUnaryOperandException &e) {
        std::cerr << e.message() << std::endl;
        return 6;
    }
    catch (const char *e) {
        std::cerr << e << std::endl;
        return 7;
    }

    return 0;
}
