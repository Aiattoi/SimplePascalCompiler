#pragma once

#include <iostream>
#include <vector>

#include <llvm/IR/Value.h>
#include <llvm/IR/IRBuilder.h>

#define N_OF_BINARY_OPERATIONS 16
#define N_OF_UNARY_OPERATIONS 2

#define FN_MAIN_NAME "main"

static llvm::LLVMContext MilaContext;   // llvm context
static llvm::IRBuilder<> MilaBuilder(MilaContext);   // llvm builder
static std::unique_ptr<llvm::Module> MilaModule;         // llvm module

static std::map<std::string, llvm::Value *> GlobalConstValues;
static std::map<std::string, llvm::GlobalVariable *> GlobalValues;

// LocalConstValues[functionName][variableName] gets the desired value
static std::map<std::string, std::map<std::string, llvm::Value *>> LocalConstValues;
static std::map<std::string, std::map<std::string, llvm::AllocaInst *>> LocalValues;

static llvm::BasicBlock *toBreakIn;
static bool isBreakedInBlock = false;

static bool isExitedInBlock = false;

static std::map<std::string, std::vector<llvm::Value *>> GlobalArraysBeginning;
static std::map<std::string, std::map<std::string, std::vector<llvm::Value *>>> LocalArraysBeginning;

class UnknownVariableException : public std::exception {
public:
    explicit UnknownVariableException(const std::string &ident);

    std::string message() const;

    const std::string ident;
};

class ConstReferenceException : public std::exception {
public:
    explicit ConstReferenceException(const std::string &ident);

    std::string message() const;

    const std::string ident;
};

class ASTNode {
public:
    virtual ~ASTNode() = default;
};

class ExpressionNode : public ASTNode {
public:
    virtual llvm::Value *generate() = 0;
};

class NumberExpressionNode : public ExpressionNode {
public:
    explicit NumberExpressionNode(int value);

    llvm::Value *generate() override;

    int getValue() const;

private:
    const int value;
};

class StatementNode : public ASTNode {
public:
    virtual void translate() = 0;
};

class Declaration {
public:
    virtual ~Declaration() = default;

    virtual void translate(bool isGlobal) = 0;

};

class ProcedureCallStatementNode : public StatementNode {
public:
    ProcedureCallStatementNode(std::string name, std::vector<std::unique_ptr<ExpressionNode>> params);

    void translate() override;

private:
    std::string name;
    std::vector<std::unique_ptr<ExpressionNode>> params;
};

class BodyNode : public ASTNode {
public:
    explicit BodyNode(std::vector<std::unique_ptr<StatementNode>> statements);

    void translate();

private:
    const std::vector<std::unique_ptr<StatementNode>> statements;
};

class ProgramNode : public ASTNode {
public:
    ProgramNode(std::string name, std::vector<std::unique_ptr<Declaration>> declarations,
                std::unique_ptr<BodyNode> body);

    void translate();

    static void printLLVM();

private:
    const std::string name;
    const std::vector<std::unique_ptr<Declaration>> declarations;
    const std::unique_ptr<BodyNode> body;
};

class Type {
public:
    virtual llvm::Type *getLLVMType() = 0;

    virtual llvm::Constant *getInitValue() = 0;
};

class Integer : public Type {
    llvm::Type *getLLVMType() override;

    llvm::Constant *getInitValue() override;
};

class VarDeclaration : public Declaration {
public:
    VarDeclaration(std::string ident, std::shared_ptr<Type> type);

    void translate(bool isGlobal) override;

    const std::string &getIdent() const;

    const std::shared_ptr<Type> &getType() const;

private:
    const std::string ident;
    const std::shared_ptr<Type> type;
};

class ConstDeclaration : public Declaration {
public:
    ConstDeclaration(std::string ident, int value);

    void translate(bool isGlobal) override;

private:
    const std::string ident;
    const int value;
};

class VarReferenceNode : public ExpressionNode {
public:
    llvm::Value *generate() override = 0;

    virtual llvm::Value *getLLVMAddress() = 0;
};

class SimpleVarReferenceNode : public VarReferenceNode {
public:
    explicit SimpleVarReferenceNode(std::string ident);

    llvm::Value *generate() override;

    llvm::Value *getLLVMAddress() override;

private:
    const std::string ident;
};

class BinOpNode : public ExpressionNode {
public:
    enum binOperation {
        bo_div, bo_mod, bo_and, bo_or, bo_equal, bo_notequal, bo_less, bo_lessequal, bo_more, bo_moreequal,
        bo_plus, bo_minus, bo_bitOr, bo_mult, bo_rDiv, bo_bitAnd
    };

    BinOpNode(std::unique_ptr<ExpressionNode> lhs, std::unique_ptr<ExpressionNode> rhs,
              binOperation op);

    llvm::Value *generate() override;

private:
    const std::unique_ptr<ExpressionNode> lhs, rhs;
    const binOperation op;
};

class UnOpNode : public ExpressionNode {
public:
    enum unOperation {
        uo_bitNot, uo_not
    };

    UnOpNode(std::unique_ptr<ExpressionNode> expr, unOperation op);

    llvm::Value *generate() override;

private:
    const std::unique_ptr<ExpressionNode> expr;
    const unOperation op;
};

class MissingBinaryOperandException : public std::exception {
public:
    MissingBinaryOperandException(BinOpNode::binOperation op, bool isLeft);

    std::string message() const;

    const BinOpNode::binOperation op;
    const bool isLeft;
private:
    const char *binOperations[N_OF_BINARY_OPERATIONS] = {
            "bo_div", "bo_mod", "bo_and", "bo_or", "bo_equal", "bo_notequal", "bo_less",
            "bo_lessequal", "bo_more", "bo_moreequal",
            "bo_plus", "bo_minus", "bo_bitOr", "bo_mult", "bo_rDiv", "bo_bitAnd"
    }; // same order as in binOperation enum
};

class MissingUnaryOperandException : public std::exception {
public:
    explicit MissingUnaryOperandException(UnOpNode::unOperation op);

    std::string message() const;

    const UnOpNode::unOperation op;
private:
    const char *unOperations[N_OF_UNARY_OPERATIONS] = {
            "uo_bitNot", "uo_not"
    }; // same order as in unOperation enum
};

class AssignStatementNode : public StatementNode {
public:
    AssignStatementNode(std::unique_ptr<VarReferenceNode> var, std::unique_ptr<ExpressionNode> val);

    void translate() override;

private:
    const std::unique_ptr<VarReferenceNode> var;
    const std::unique_ptr<ExpressionNode> val;
};

class IfStatementNode : public StatementNode {
public:
    IfStatementNode(std::unique_ptr<ExpressionNode> cond, std::unique_ptr<BodyNode> thenBody,
                    std::unique_ptr<BodyNode> elseBody);

    void translate() override;

private:
    const std::unique_ptr<ExpressionNode> cond;
    const std::unique_ptr<BodyNode> thenBody;
    const std::unique_ptr<BodyNode> elseBody;
};

class WhileStatementNode : public StatementNode {
public:
    WhileStatementNode(std::unique_ptr<ExpressionNode> cond, std::unique_ptr<BodyNode> body);

    void translate() override;

private:
    const std::unique_ptr<ExpressionNode> cond;
    const std::unique_ptr<BodyNode> body;
};

class BreakStatementNode : public StatementNode {
public:
    void translate() override;
};

class ForStatementNode : public StatementNode {
public:
    ForStatementNode(std::string ident, std::unique_ptr<ExpressionNode> from, bool isAsc,
                     std::unique_ptr<ExpressionNode> to, std::unique_ptr<BodyNode> body);

    void translate() override;

private:
    const std::string ident;
    const std::unique_ptr<ExpressionNode> from;
    const bool isAsc;
    const std::unique_ptr<ExpressionNode> to;
    const std::unique_ptr<BodyNode> body;
};

class ProcedurePrototype {
public:
    ProcedurePrototype(std::string name,
                       std::vector<std::unique_ptr<VarDeclaration>> params);

    llvm::Function *generate();

    const std::string &getName() const;

private:
    const std::string name;
    const std::vector<std::unique_ptr<VarDeclaration>> params;
};

class ProcedureDeclaration : public Declaration {
public:
    ProcedureDeclaration(std::unique_ptr<ProcedurePrototype> prototype,
                         std::vector<std::unique_ptr<Declaration>> declarations,
                         std::unique_ptr<BodyNode> body);

    void translate(bool isGlobal) override;

private:
    const std::unique_ptr<ProcedurePrototype> prototype;
    const std::vector<std::unique_ptr<Declaration>> declarations;
    const std::unique_ptr<BodyNode> body;
};

class FunctionCallExpressionNode : public ExpressionNode {
public:
    FunctionCallExpressionNode(std::string name, std::vector<std::unique_ptr<ExpressionNode>> params);

    llvm::Value *generate() override;

private:
    std::string name;
    std::vector<std::unique_ptr<ExpressionNode>> params;
};

class FunctionPrototype {
public:
    FunctionPrototype(std::string name,
                      std::vector<std::unique_ptr<VarDeclaration>> params, std::shared_ptr<Type> returnType);

    llvm::Function *generate();

    const std::string &getName() const;

    const std::shared_ptr<Type> &getReturnType() const;

private:
    const std::string name;
    const std::vector<std::unique_ptr<VarDeclaration>> params;
    const std::shared_ptr<Type> returnType;
};

class FunctionDeclaration : public Declaration {
public:
    FunctionDeclaration(std::unique_ptr<FunctionPrototype> prototype,
                        std::vector<std::unique_ptr<Declaration>> declarations,
                        std::unique_ptr<BodyNode> body);

    void translate(bool isGlobal) override;

private:
    const std::unique_ptr<FunctionPrototype> prototype;
    const std::vector<std::unique_ptr<Declaration>> declarations;
    const std::unique_ptr<BodyNode> body;
};

class ExitStatementNode : public StatementNode {
public:
    void translate() override;
};

class StringExpressionNode : public ExpressionNode {
public:
    explicit StringExpressionNode(std::string text);

    llvm::Value *generate() override;

private:
    const std::string text;
};

class Array : public Type {
public:
    Array(std::unique_ptr<NumberExpressionNode> idxFrom, std::unique_ptr<NumberExpressionNode> idxTo,
          std::shared_ptr<Type> elemType);

    llvm::Type *getLLVMType() override;

    llvm::Constant *getInitValue() override;

    const std::unique_ptr<NumberExpressionNode> &getIdxFrom() const;

    const std::shared_ptr<Type> &getElemType() const;

private:
    const std::unique_ptr<NumberExpressionNode> idxFrom;
    const std::unique_ptr<NumberExpressionNode> idxTo;
    const std::shared_ptr<Type> elemType;
};

class ArrayItemReferenceNode : public VarReferenceNode {
public:
    ArrayItemReferenceNode(std::string arrayName,
                           std::vector<std::unique_ptr<ExpressionNode>> indices);

    llvm::Value *generate() override;

    llvm::Value *getLLVMAddress() override;

private:
    const std::string arrayName;
    const std::vector<std::unique_ptr<ExpressionNode>> indices;
};