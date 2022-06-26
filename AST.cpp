#include "AST.h"

UnknownVariableException::UnknownVariableException(const std::string &ident)
        : ident(ident) {
}

std::string UnknownVariableException::message() const {
    std::string response = "Unknown variable: '";
    response.append(ident);
    response.append("'.");
    return response;
}

ConstReferenceException::ConstReferenceException(const std::string &ident) : ident(ident) {}

std::string ConstReferenceException::message() const {
    std::string response = "Cannot get address of a constant: '";
    response.append(ident);
    response.append("'.");
    return response;
}

NumberExpressionNode::NumberExpressionNode(int value)
        : value(value) {
}

llvm::Value *NumberExpressionNode::generate() {
    return llvm::ConstantInt::get(MilaContext, llvm::APInt(32, value, true));
}

int NumberExpressionNode::getValue() const {
    return value;
}

ProcedureCallStatementNode::ProcedureCallStatementNode(std::string name,
                                                       std::vector<std::unique_ptr<ExpressionNode>> params)
        : name(std::move(name)), params(std::move(params)) {
}

void ProcedureCallStatementNode::translate() {
    if (name == "write") {
        auto param = params[0]->generate();
        // int
        if (param->getType() == llvm::Type::getInt32Ty(MilaContext)) {
            MilaBuilder.CreateCall(MilaModule->getFunction("write"),
                                   {param});
            isBreakedInBlock = false;
            isExitedInBlock = false;
            return;
        }
        // string
        if (param->getType() == llvm::ArrayType::getInt8PtrTy(MilaContext)) {
            MilaBuilder.CreateCall(MilaModule->getFunction("writeS"),
                                   {param});
            isBreakedInBlock = false;
            isExitedInBlock = false;
            return;
        }
        // real ?
        std::cerr << "Parameter of procedure: 'write' can be int or string only." << std::endl;
        return;
    }

    if (name == "writeln") {
        auto param = params[0]->generate();
        if (param->getType() == llvm::Type::getInt32Ty(MilaContext)) {
            MilaBuilder.CreateCall(MilaModule->getFunction("writeln"),
                                   {param});
            isBreakedInBlock = false;
            isExitedInBlock = false;
            return;
        }
        // string
        if (param->getType() == llvm::ArrayType::getInt8PtrTy(MilaContext)) {
            MilaBuilder.CreateCall(MilaModule->getFunction("writelnS"),
                                   {param});
            isBreakedInBlock = false;
            isExitedInBlock = false;
            return;
        }
        // real ?
        std::cerr << "Parameter of procedure: 'writeln' can be int or string only." << std::endl;
        return;
    }

    if (name == "readln") {
        // retrieve paramAddresss (*int)
        llvm::Value *paramAddress = ((VarReferenceNode *) (params[0].get()))->getLLVMAddress();
        MilaBuilder.CreateCall(MilaModule->getFunction("readln"), {paramAddress});
        isBreakedInBlock = false;
        isExitedInBlock = false;
        return;
    }

    if (name == "dec") {
        // retrieve paramAddresss (*int)
        llvm::Value *paramAddress = ((VarReferenceNode *) (params[0].get()))->getLLVMAddress();
        // retrieve paramValue (int)
        llvm::Value *paramValue = params[0]->generate();
        MilaBuilder.CreateStore(
                MilaBuilder.CreateSub(paramValue, llvm::ConstantInt::get(llvm::Type::getInt32Ty(MilaContext), 1, true)),
                paramAddress);
        isBreakedInBlock = false;
        isExitedInBlock = false;
        return;
    }

    if (name == "inc") {
        // retrieve paramAddresss (*int)
        llvm::Value *paramAddress = ((VarReferenceNode *) (params[0].get()))->getLLVMAddress();
        // retrieve paramValue (int)
        llvm::Value *paramValue = params[0]->generate();
        MilaBuilder.CreateStore(
                MilaBuilder.CreateAdd(paramValue, llvm::ConstantInt::get(llvm::Type::getInt32Ty(MilaContext), 1, true)),
                paramAddress);
        isBreakedInBlock = false;
        isExitedInBlock = false;
        return;
    }

    llvm::Function *foo = MilaModule->getFunction(name);
    if (!foo) {
        std::cerr << "Procedure " << name << " not found." << std::endl;
        return;
    }
    if (foo->arg_size() != params.size()) {
        std::cerr << "Procedure expected " << foo->arg_size() << " parameters, got " << params.size() << "."
                  << std::endl;
        return;
    }
    std::vector<llvm::Value *> paramsV;
    for (auto &param: params)
        paramsV.push_back(param->generate());

    MilaBuilder.CreateCall(foo, paramsV);

    isBreakedInBlock = false;
    isExitedInBlock = false;
}

BodyNode::BodyNode(std::vector<std::unique_ptr<StatementNode>> statements)
        : statements(std::move(statements)) {
}

void BodyNode::translate() {
    for (auto &statement: statements) {
        statement->translate();
    }
}

ProgramNode::ProgramNode(std::string name, std::vector<std::unique_ptr<Declaration>> declarations,
                         std::unique_ptr<BodyNode> body)
        : name(std::move(name)), declarations(std::move(declarations)), body(std::move(body)) {
}

void ProgramNode::translate() {
    MilaModule = std::make_unique<llvm::Module>("mila", MilaContext);

    // create write
    llvm::Function *fwrite = llvm::Function::Create(llvm::FunctionType::get(
                                                            llvm::Type::getInt32Ty(MilaContext),
                                                            {llvm::Type::getInt32Ty(MilaContext)}, false),
                                                    llvm::Function::ExternalLinkage,
                                                    llvm::Twine("write"),
                                                    MilaModule.get());
    fwrite->setCallingConv(llvm::CallingConv::C);

    // create writeln
    llvm::Function *fwriteln = llvm::Function::Create(llvm::FunctionType::get(
                                                              llvm::Type::getInt32Ty(MilaContext),
                                                              {llvm::Type::getInt32Ty(MilaContext)}, false),
                                                      llvm::Function::ExternalLinkage,
                                                      llvm::Twine("writeln"),
                                                      MilaModule.get());
    fwriteln->setCallingConv(llvm::CallingConv::C);

    // create readln
    llvm::Function *freadln = llvm::Function::Create(llvm::FunctionType::get(
                                                             llvm::Type::getInt32Ty(MilaContext),
                                                             {llvm::Type::getInt32PtrTy(MilaContext)}, false),
                                                     llvm::Function::ExternalLinkage,
                                                     llvm::Twine("readln"),
                                                     MilaModule.get());
    freadln->setCallingConv(llvm::CallingConv::C);

    // create writeS
    llvm::Function *fwriteS = llvm::Function::Create(llvm::FunctionType::get(
                                                             llvm::Type::getInt32Ty(MilaContext),
                                                             {llvm::ArrayType::getInt8PtrTy(MilaContext)}, false),
                                                     llvm::Function::ExternalLinkage,
                                                     llvm::Twine("writeS"),
                                                     MilaModule.get());
    fwriteS->setCallingConv(llvm::CallingConv::C);

    // create writelnS
    llvm::Function *fwritelnS = llvm::Function::Create(llvm::FunctionType::get(
                                                               llvm::Type::getInt32Ty(MilaContext),
                                                               {llvm::ArrayType::getInt8PtrTy(MilaContext)}, false),
                                                       llvm::Function::ExternalLinkage,
                                                       llvm::Twine("writelnS"),
                                                       MilaModule.get());
    fwritelnS->setCallingConv(llvm::CallingConv::C);

    //create main
    llvm::Function *main = llvm::Function::Create(
            llvm::FunctionType::get(llvm::Type::getInt32Ty(MilaContext), {}, false),
            llvm::GlobalValue::ExternalLinkage, FN_MAIN_NAME, MilaModule.get());
    main->setCallingConv(llvm::CallingConv::C);

    // create main block
    llvm::BasicBlock *mainBlock = llvm::BasicBlock::Create(MilaContext, "mainBlock", main);
    MilaBuilder.SetInsertPoint(mainBlock);

    // generate declarations
    for (auto &decl: declarations)
        decl->translate(true);

    // generate body code
    body->translate();

    // set return
    MilaBuilder.CreateRet(llvm::ConstantInt::get(MilaContext, llvm::APInt(32, 0)));
}

void ProgramNode::printLLVM() {
    MilaModule->print(llvm::outs(), nullptr);
}

llvm::Type *Integer::getLLVMType() {
    return llvm::Type::getInt32Ty(MilaContext);
}

llvm::Constant *Integer::getInitValue() {
    return llvm::ConstantInt::get(MilaContext, llvm::APInt(32, 0, true));
}

VarDeclaration::VarDeclaration(std::string ident, std::shared_ptr<Type> type) : ident(std::move(ident)),
                                                                                type(std::move(type)) {}

void VarDeclaration::translate(bool isGlobal) {
    llvm::Type *lType = type->getLLVMType();
    std::string fooName = MilaBuilder.GetInsertBlock()->getParent()->getName().str();
    if (!isGlobal) {
        if (lType->isArrayTy()) {
            int idxFrom = ((Array *) type.get())->getIdxFrom()->getValue();
            // store idxFrom for first dimension of the array
            LocalArraysBeginning[fooName][ident].push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(MilaContext),
                                                                                  idxFrom,
                                                                                  true));
            Array *arr = ((Array *) type.get());
            while (arr->getElemType()->getLLVMType()->isArrayTy()) {
                arr = ((Array *) arr->getElemType().get());
                idxFrom = arr->getIdxFrom()->getValue();
                // store idxFrom for nth dimension of the array
                LocalArraysBeginning[fooName][ident].push_back(llvm::ConstantInt::get(
                        llvm::Type::getInt32Ty(MilaContext), idxFrom,
                        true));
            }
        }
        LocalValues[fooName][ident] = MilaBuilder.CreateAlloca(lType, nullptr, ident);
        return;
    }
    if (lType->isArrayTy()) {
        int idxFrom = ((Array *) type.get())->getIdxFrom()->getValue();
        // store idxFrom for first dimension of the array
        GlobalArraysBeginning[ident].push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(MilaContext), idxFrom,
                                                                      true));

        Array *arr = ((Array *) type.get());
        while (arr->getElemType()->getLLVMType()->isArrayTy()) {
            arr = ((Array *) arr->getElemType().get());
            idxFrom = arr->getIdxFrom()->getValue();
            // store idxFrom for nth dimension of the array
            GlobalArraysBeginning[ident].push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(MilaContext), idxFrom,
                                                                          true));
        }
    }
    MilaModule->getOrInsertGlobal(ident, lType);
    llvm::GlobalVariable *gVar = MilaModule->getNamedGlobal(ident);
    gVar->setLinkage(llvm::GlobalValue::CommonLinkage);
    gVar->setAlignment(llvm::MaybeAlign(lType->isArrayTy() ? 16 : 4));
    gVar->setInitializer(type->getInitValue());
    GlobalValues.insert(std::make_pair(ident, gVar));
}

const std::shared_ptr<Type> &VarDeclaration::getType() const {
    return type;
}

const std::string &VarDeclaration::getIdent() const {
    return ident;
}

ConstDeclaration::ConstDeclaration(std::string ident, const int value) : ident(std::move(ident)),
                                                                         value(value) {}

void ConstDeclaration::translate(bool isGlobal) {
    std::string fooName = MilaBuilder.GetInsertBlock()->getParent()->getName().str();
    llvm::ConstantInt *lVal = llvm::ConstantInt::get(MilaContext, llvm::APInt(32, value, true));
    if (!isGlobal)
        LocalConstValues[fooName][ident] = lVal;
    else
        GlobalConstValues.insert(std::make_pair(ident, lVal));
}

SimpleVarReferenceNode::SimpleVarReferenceNode(std::string ident) : ident(std::move(ident)) {}

llvm::Value *SimpleVarReferenceNode::generate() {
    std::string fooName = MilaBuilder.GetInsertBlock()->getParent()->getName().str();

    if (LocalConstValues[fooName].find(ident) != LocalConstValues[fooName].end())
        return LocalConstValues[fooName].at(ident);
    if (LocalValues[fooName].find(ident) != LocalValues[fooName].end())
        return MilaBuilder.CreateLoad(LocalValues[fooName].at(ident), ident);
    if (GlobalConstValues.find(ident) != GlobalConstValues.end())
        return GlobalConstValues.at(ident);
    if (GlobalValues.find(ident) != GlobalValues.end())
        return MilaBuilder.CreateLoad(GlobalValues.at(ident), ident);
    throw UnknownVariableException(ident);
}

llvm::Value *SimpleVarReferenceNode::getLLVMAddress() {
    std::string fooName = MilaBuilder.GetInsertBlock()->getParent()->getName().str();

    if (LocalValues[fooName].find(ident) != LocalValues[fooName].end())
        return LocalValues[fooName].at(ident);
    if (GlobalValues.find(ident) != GlobalValues.end())
        return GlobalValues.at(ident);

    if (GlobalConstValues.find(ident) != GlobalConstValues.end())
        throw ConstReferenceException(ident);
    throw UnknownVariableException(ident);
}

BinOpNode::BinOpNode(std::unique_ptr<ExpressionNode> lhs, std::unique_ptr<ExpressionNode> rhs,
                     BinOpNode::binOperation op) : lhs(std::move(lhs)), rhs(std::move(rhs)), op(op) {}

llvm::Value *BinOpNode::generate() {
    llvm::Value *left = lhs->generate();
    llvm::Value *right = rhs->generate();

    if (!left)
        throw MissingBinaryOperandException(op, true);
    if (!right)
        throw MissingBinaryOperandException(op, false);

    llvm::Value *ret = nullptr;
    llvm::Value *Div = nullptr;
    switch (op) {
        case bo_div:
            Div = MilaBuilder.CreateSDiv(left, right);
            ret = MilaBuilder.CreateTrunc(Div, llvm::Type::getInt32Ty(MilaContext));
            break;
        case bo_mod:
            ret = MilaBuilder.CreateSRem(left, right);
            break;
        case bo_and:
            ret = MilaBuilder.CreateAnd(left, right);
            break;
        case bo_or:
            ret = MilaBuilder.CreateOr(left, right);
            break;
        case bo_equal:
            ret = MilaBuilder.CreateICmpEQ(left, right);
            break;
        case bo_notequal:
            ret = MilaBuilder.CreateICmpNE(left, right);
            break;
        case bo_less:
            ret = MilaBuilder.CreateICmpSLT(left, right);
            break;
        case bo_lessequal:
            ret = MilaBuilder.CreateICmpSLE(left, right);
            break;
        case bo_more:
            ret = MilaBuilder.CreateICmpSGT(left, right);
            break;
        case bo_moreequal:
            ret = MilaBuilder.CreateICmpSGE(left, right);
            break;
        case bo_plus:
            ret = MilaBuilder.CreateAdd(left, right);
            break;
        case bo_minus:
            ret = MilaBuilder.CreateSub(left, right);
            break;
        case bo_bitOr:
            ret = MilaBuilder.CreateOr(left, right);
            break;
        case bo_mult:
            ret = MilaBuilder.CreateMul(left, right);
            break;
        case bo_rDiv:
            ret = MilaBuilder.CreateFDiv(left, right);
            break;
        case bo_bitAnd:
            ret = MilaBuilder.CreateAnd(left, right);
            break;
    }
    // result must be a 32 bit number, comparison returns 1 bit number
    return MilaBuilder.CreateIntCast(ret, llvm::Type::getInt32Ty(MilaContext), true);
}

UnOpNode::UnOpNode(std::unique_ptr<ExpressionNode> expr, UnOpNode::unOperation op)
        : expr(std::move(expr)), op(op) {}

llvm::Value *UnOpNode::generate() {
    llvm::Value *exprVal = expr->generate();

    if (!exprVal)
        throw MissingUnaryOperandException(op);

    llvm::Value *ret = nullptr;
    switch (op) {
        case uo_bitNot:
            ret = MilaBuilder.CreateNot(exprVal);
            break;
        case uo_not:
            ret = MilaBuilder.CreateNot(exprVal);
            break;
    }
    return MilaBuilder.CreateIntCast(ret, llvm::Type::getInt32Ty(MilaContext), true);
}

MissingBinaryOperandException::MissingBinaryOperandException(BinOpNode::binOperation op, bool isLeft)
        : op(op), isLeft(isLeft) {}

std::string MissingBinaryOperandException::message() const {
    std::string response = "Operator '";
    response.append(binOperations[op]);
    response.append("' misses the ");
    if (isLeft)
        response.append("left");
    else
        response.append("right");
    response.append(" hand side operand.");
    return response;
}

MissingUnaryOperandException::MissingUnaryOperandException(UnOpNode::unOperation op)
        : op(op) {}

std::string MissingUnaryOperandException::message() const {
    std::string response = "Operator '";
    response.append(unOperations[op]);
    response.append("' misses the operand.");
    return response;
}

AssignStatementNode::AssignStatementNode(std::unique_ptr<VarReferenceNode> var, std::unique_ptr<ExpressionNode> val)
        : var(std::move(var)), val(std::move(val)) {}

void AssignStatementNode::translate() {
    llvm::Value *varRef = var->getLLVMAddress();
    MilaBuilder.CreateStore(val->generate(), varRef);

    isBreakedInBlock = false;
}

IfStatementNode::IfStatementNode(std::unique_ptr<ExpressionNode> cond, std::unique_ptr<BodyNode> thenBody,
                                 std::unique_ptr<BodyNode> elseBody)
        : cond(std::move(cond)), thenBody(std::move(thenBody)), elseBody(std::move(elseBody)) {}

void IfStatementNode::translate() {
    llvm::Value *condition = MilaBuilder.CreateICmpNE(cond->generate(),
                                                      llvm::ConstantInt::get(MilaContext, llvm::APInt(32, 0, true)));
    llvm::Function *foo = MilaBuilder.GetInsertBlock()->getParent();
    llvm::BasicBlock *if_thenBB = llvm::BasicBlock::Create(MilaContext, "if_then", foo);
    llvm::BasicBlock *if_elseBB = llvm::BasicBlock::Create(MilaContext, "if_else");
    llvm::BasicBlock *if_afterBB = llvm::BasicBlock::Create(MilaContext, "if_after");

    // condition
    MilaBuilder.CreateCondBr(condition, if_thenBB, if_elseBB);

    // then
    MilaBuilder.SetInsertPoint(if_thenBB);
    isBreakedInBlock = false;
    thenBody->translate();
    if (!isBreakedInBlock)
        MilaBuilder.CreateBr(if_afterBB);

    // else
    foo->getBasicBlockList().push_back(if_elseBB);
    MilaBuilder.SetInsertPoint(if_elseBB);
    isBreakedInBlock = false;
    if (elseBody)
        elseBody->translate();
    if (!isBreakedInBlock)
        MilaBuilder.CreateBr(if_afterBB);

    // after if
    foo->getBasicBlockList().push_back(if_afterBB);
    MilaBuilder.SetInsertPoint(if_afterBB);

    isBreakedInBlock = false;
    isExitedInBlock = false;
}

WhileStatementNode::WhileStatementNode(std::unique_ptr<ExpressionNode> cond,
                                       std::unique_ptr<BodyNode> body) : cond(std::move(cond)), body(std::move(body)) {}

void WhileStatementNode::translate() {
    llvm::Function *foo = MilaBuilder.GetInsertBlock()->getParent();
    llvm::BasicBlock *while_loopBB = llvm::BasicBlock::Create(MilaContext, "while_loop");
    llvm::BasicBlock *while_condBB = llvm::BasicBlock::Create(MilaContext, "while_cond", foo);
    llvm::BasicBlock *while_afterBB = llvm::BasicBlock::Create(MilaContext, "while_after");

    // condition
    MilaBuilder.CreateBr(while_condBB);
    MilaBuilder.SetInsertPoint(while_condBB);
    llvm::Value *condition = cond->generate();
    condition = MilaBuilder.CreateICmpNE(condition, llvm::ConstantInt::get(MilaContext, llvm::APInt(32, 0, true)));
    MilaBuilder.CreateCondBr(condition, while_loopBB, while_afterBB);

    // body
    foo->getBasicBlockList().push_back(while_loopBB);
    MilaBuilder.SetInsertPoint(while_loopBB);
    llvm::BasicBlock *oldToBreakIn = toBreakIn;
    toBreakIn = while_afterBB;
    isBreakedInBlock = false;
    body->translate();
    toBreakIn = oldToBreakIn;
    if (!isBreakedInBlock)
        MilaBuilder.CreateBr(while_condBB);

    // after while
    foo->getBasicBlockList().push_back(while_afterBB);
    MilaBuilder.SetInsertPoint(while_afterBB);

    isBreakedInBlock = false;
    isExitedInBlock = false;
}

void BreakStatementNode::translate() {
    MilaBuilder.CreateBr(toBreakIn);

    isBreakedInBlock = true;
    isExitedInBlock = false;
}

ForStatementNode::ForStatementNode(std::string ident, std::unique_ptr<ExpressionNode> from,
                                   bool isAsc, std::unique_ptr<ExpressionNode> to,
                                   std::unique_ptr<BodyNode> body)
        : ident(std::move(ident)), from(std::move(from)),
          isAsc(isAsc),
          to(std::move(to)), body(std::move(body)) {}

void ForStatementNode::translate() {
    llvm::Function *foo = MilaBuilder.GetInsertBlock()->getParent();
    std::string fooName = foo->getName().str();
    // alloca for the variable in for declaration
    llvm::IRBuilder<> tmpBuilder(&foo->getEntryBlock(), foo->getEntryBlock().begin());
    llvm::AllocaInst *fromAlloca = tmpBuilder.CreateAlloca(llvm::Type::getInt32Ty(MilaContext),
                                                           llvm::ConstantInt::get(llvm::Type::getInt32Ty(MilaContext),
                                                                                  0, false), ident.c_str());

    // compute the from expression
    llvm::Value *fromVal = from->generate();
    if (!fromVal) {
        std::cerr << "No from value in for." << std::endl;
        return;
    }
    MilaBuilder.CreateStore(fromVal, fromAlloca);

    // create necessary basic blocks
    llvm::BasicBlock *for_loopBB = llvm::BasicBlock::Create(MilaContext, "for_loop", foo);
    llvm::BasicBlock *for_afterBB = llvm::BasicBlock::Create(MilaContext, "for_after");


    // body
    MilaBuilder.CreateBr(for_loopBB);
    MilaBuilder.SetInsertPoint(for_loopBB);
    // sort variable shadowing
    llvm::AllocaInst *oldVal = LocalValues[fooName][ident];
    // make the variable from for declaration usable
    LocalValues[fooName][ident] = fromAlloca;
    llvm::BasicBlock *oldToBreakIn = toBreakIn;
    toBreakIn = for_afterBB;
    body->translate();

    // inc/dec
    toBreakIn = oldToBreakIn;
    llvm::Value *stepVal = isAsc
                           ? llvm::ConstantInt::get(MilaContext, llvm::APInt(32, 1, true))
                           : llvm::ConstantInt::get(MilaContext, llvm::APInt(32, -1, true));
    llvm::Value *curVal = MilaBuilder.CreateLoad(fromAlloca, ident.c_str());
    llvm::Value *nextVal = MilaBuilder.CreateAdd(curVal, stepVal, "nextVal");
    MilaBuilder.CreateStore(nextVal, fromAlloca);

    // evaluate the cond
    llvm::Value *toVal = to->generate();
    if (!toVal) {
        std::cerr << "No to value in for." << std::endl;
        return;
    }
    llvm::Value *cond = MilaBuilder.CreateICmpNE(toVal, curVal);
    MilaBuilder.CreateCondBr(cond, for_loopBB, for_afterBB);

    // after for
    foo->getBasicBlockList().push_back(for_afterBB);
    MilaBuilder.SetInsertPoint(for_afterBB);

    isBreakedInBlock = false;
    isExitedInBlock = false;

    if (oldVal)
        // restore the shadowed variable
        LocalValues[fooName][ident] = oldVal;
    else
        // clear the variable from the for declaration
        LocalValues.erase(ident);
}

ProcedurePrototype::ProcedurePrototype(std::string name,
                                       std::vector<std::unique_ptr<VarDeclaration>> params)
        : name(std::move(name)), params(std::move(params)) {}

llvm::Function *ProcedurePrototype::generate() {
    std::vector<llvm::Type *> paramsTypes;
    for (auto &param: params)
        paramsTypes.push_back(param->getType()->getLLVMType());

    llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getVoidTy(MilaContext), paramsTypes, false);
    llvm::Function *f = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, name, MilaModule.get());

    size_t i = 0;
    for (auto &arg: f->args())
        arg.setName(params[i++]->getIdent());

    return f;
}

const std::string &ProcedurePrototype::getName() const {
    return name;
}


ProcedureDeclaration::ProcedureDeclaration(std::unique_ptr<ProcedurePrototype> prototype,
                                           std::vector<std::unique_ptr<Declaration>> declarations,
                                           std::unique_ptr<BodyNode> body) : prototype(std::move(prototype)),
                                                                             declarations(
                                                                                     std::move(declarations)),
                                                                             body(std::move(body)) {}

void ProcedureDeclaration::translate(bool isGlobal) {
    llvm::Function *f = MilaModule->getFunction(prototype->getName());

    if (!f)
        f = prototype->generate();

    if (!f) {
        std::cerr << "Procedure: '" << prototype->getName() << "' couldn't be generated." << std::endl;
        return;
    }
    if (!body) {
        // not an error, because of forward declaration
        return;
    }

    // create a basic block for this procedure
    llvm::BasicBlock *BB = llvm::BasicBlock::Create(MilaContext, "procedure:" + prototype->getName() + ":block", f);
    llvm::BasicBlock *prevBB = MilaBuilder.GetInsertBlock();
    MilaBuilder.SetInsertPoint(BB);

    // handle procedure's arguments
    llvm::IRBuilder<> tmpBuilder(&f->getEntryBlock(), f->getEntryBlock().begin());
    for (auto &arg: f->args()) {
        llvm::AllocaInst *alloca = tmpBuilder.CreateAlloca(arg.getType(),
                                                           llvm::ConstantInt::get(llvm::Type::getInt32Ty(MilaContext),
                                                                                  0, false), arg.getName());
        MilaBuilder.CreateStore(&arg, alloca);

        LocalValues[prototype->getName()][arg.getName().str()] = alloca;
    }

    // generate declarations
    for (auto &declaration: declarations)
        declaration->translate(false);

    // generate body
    body->translate();

    // return
    MilaBuilder.CreateRetVoid();
    MilaBuilder.SetInsertPoint(prevBB);
}

FunctionCallExpressionNode::FunctionCallExpressionNode(std::string name,
                                                       std::vector<std::unique_ptr<ExpressionNode>> params)
        : name(std::move(name)), params(std::move(params)) {}

llvm::Value *FunctionCallExpressionNode::generate() {
    if (name == "dec") {
        auto ret = params[0]->generate();
        ProcedureCallStatementNode(name, std::move(params)).translate();
        return ret;
    }

    if (name == "inc") {
        auto ret = params[0]->generate();
        ProcedureCallStatementNode(name, std::move(params)).translate();
        return ret;
    }

    llvm::Function *foo = MilaModule->getFunction(name);
    if (!foo) {
        std::cerr << "Function " << name << " not found." << std::endl;
        return nullptr;
    }
    if (foo->arg_size() != params.size()) {
        std::cerr << "Function expected " << foo->arg_size() << " parameters, got " << params.size() << "."
                  << std::endl;
        return nullptr;
    }
    std::vector<llvm::Value *> paramsV;
    for (auto &param: params)
        paramsV.push_back(param->generate());

    return MilaBuilder.CreateCall(foo, paramsV);
}

FunctionPrototype::FunctionPrototype(std::string name, std::vector<std::unique_ptr<VarDeclaration>> params,
                                     std::shared_ptr<Type> returnType)
        : name(std::move(name)), params(std::move(params)), returnType(std::move(returnType)) {}

llvm::Function *FunctionPrototype::generate() {
    std::vector<llvm::Type *> paramsTypes;
    for (auto &param: params)
        paramsTypes.push_back(param->getType()->getLLVMType());

    llvm::FunctionType *FT = llvm::FunctionType::get(returnType->getLLVMType(), paramsTypes, false);
    llvm::Function *f = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, name, MilaModule.get());

    size_t i = 0;
    for (auto &arg: f->args())
        arg.setName(params[i++]->getIdent());

    return f;
}

const std::string &FunctionPrototype::getName() const {
    return name;
}

const std::shared_ptr<Type> &FunctionPrototype::getReturnType() const {
    return returnType;
}

FunctionDeclaration::FunctionDeclaration(std::unique_ptr<FunctionPrototype> prototype,
                                         std::vector<std::unique_ptr<Declaration>> declarations,
                                         std::unique_ptr<BodyNode> body)
        : prototype(std::move(prototype)), declarations(std::move(declarations)), body(std::move(body)) {}

void FunctionDeclaration::translate(bool isGlobal) {
    llvm::Function *f = MilaModule->getFunction(prototype->getName());

    if (!f)
        f = prototype->generate();

    if (!f) {
        std::cerr << "Function: '" << prototype->getName() << "' couldn't be generated." << std::endl;
        return;
    }
    if (!body) {
        // not an error, because of forward declaration
        return;
    }

    // create a basic block for this function
    llvm::BasicBlock *BB = llvm::BasicBlock::Create(MilaContext, "function:" + prototype->getName() + ":block", f);
    llvm::BasicBlock *prevBB = MilaBuilder.GetInsertBlock();
    MilaBuilder.SetInsertPoint(BB);

    // handle function return value and function's arguments
    llvm::IRBuilder<> tmpBuilder(&f->getEntryBlock(), f->getEntryBlock().begin());
    LocalValues[prototype->getName()][prototype->getName()] = tmpBuilder.CreateAlloca(
            prototype->getReturnType()->getLLVMType(),
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(MilaContext),
                                   0, false), prototype->getName());
    for (auto &arg: f->args()) {
        llvm::AllocaInst *alloca = tmpBuilder.CreateAlloca(arg.getType(),
                                                           llvm::ConstantInt::get(llvm::Type::getInt32Ty(MilaContext),
                                                                                  0, false), arg.getName());
        MilaBuilder.CreateStore(&arg, alloca);

        LocalValues[prototype->getName()][arg.getName().str()] = alloca;
    }

    // generate declarations
    for (auto &declaration: declarations)
        declaration->translate(false);

    isExitedInBlock = false;
    // generate body
    body->translate();

    // return
    llvm::Value *res = nullptr;
    if (!isExitedInBlock)
        res = MilaBuilder.CreateRet(MilaBuilder.CreateLoad(LocalValues[prototype->getName()][prototype->getName()]));
    MilaBuilder.SetInsertPoint(prevBB);
}

void ExitStatementNode::translate() {
    llvm::Function *foo = MilaBuilder.GetInsertBlock()->getParent();
    std::string fooName = foo->getName().str();
    if (fooName == FN_MAIN_NAME)
        MilaBuilder.CreateRet(llvm::ConstantInt::get(MilaContext, llvm::APInt(32, 1)));
    else if (foo->getReturnType() != llvm::Type::getVoidTy(MilaContext)) {
        MilaBuilder.CreateRet(MilaBuilder.CreateLoad(LocalValues[fooName][fooName]));
    } else
        MilaBuilder.CreateRetVoid();

    isBreakedInBlock = true;
    isExitedInBlock = true;
}

StringExpressionNode::StringExpressionNode(std::string text) : text(std::move(text)) {}

llvm::Value *StringExpressionNode::generate() {
    return MilaBuilder.CreateGlobalStringPtr(llvm::StringRef(text));
}

Array::Array(std::unique_ptr<NumberExpressionNode> idxFrom, std::unique_ptr<NumberExpressionNode> idxTo,
             std::shared_ptr<Type> elemType)
        : idxFrom(std::move(idxFrom)), idxTo(std::move(idxTo)), elemType(std::move(elemType)) {}

llvm::Type *Array::getLLVMType() {
    return llvm::ArrayType::get(elemType->getLLVMType(), idxTo->getValue() - idxFrom->getValue() + 1);
}

llvm::Constant *Array::getInitValue() {
    return llvm::ConstantArray::get((llvm::ArrayType *) getLLVMType(), elemType->getInitValue());
}

const std::unique_ptr<NumberExpressionNode> &Array::getIdxFrom() const {
    return idxFrom;
}

const std::shared_ptr<Type> &Array::getElemType() const {
    return elemType;
}

ArrayItemReferenceNode::ArrayItemReferenceNode(std::string arrayName,
                                               std::vector<std::unique_ptr<ExpressionNode>> indices)
        : arrayName(std::move(arrayName)), indices(std::move(indices)) {}

llvm::Value *ArrayItemReferenceNode::generate() {
    return MilaBuilder.CreateLoad(getLLVMAddress());
}

llvm::Value *ArrayItemReferenceNode::getLLVMAddress() {
    std::string fooName = MilaBuilder.GetInsertBlock()->getParent()->getName().str();
    if (LocalValues.find(arrayName) != LocalValues.end()) {
        std::vector<llvm::Value *> lIndices;
        lIndices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(MilaContext), 0));
        size_t depth = 0;
        lIndices.push_back(
                MilaBuilder.CreateSub(indices[depth]->generate(), LocalArraysBeginning[fooName][arrayName][depth]));
        for (depth = 1; depth < indices.size(); depth++)
            lIndices.push_back(
                    MilaBuilder.CreateSub(indices[depth]->generate(), LocalArraysBeginning[fooName][arrayName][depth]));

        return MilaBuilder.CreateGEP(LocalValues[fooName][arrayName], lIndices);
    }

    if (GlobalValues.find(arrayName) != GlobalValues.end()) {
        std::vector<llvm::Value *> lIndices;
        lIndices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(MilaContext), 0));
        size_t depth = 0;
        // load indices for all the desired dimensions of the array reference
        lIndices.push_back(MilaBuilder.CreateSub(indices[depth]->generate(), GlobalArraysBeginning[arrayName][depth]));
        for (depth = 1; depth < indices.size(); depth++)
            lIndices.push_back(
                    MilaBuilder.CreateSub(indices[depth]->generate(), GlobalArraysBeginning[arrayName][depth]));

        return MilaBuilder.CreateGEP(GlobalValues[arrayName], lIndices);
    }

    throw UnknownVariableException(arrayName);
}
