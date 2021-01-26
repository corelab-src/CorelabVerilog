#include "GetMemOper.h"

using namespace llvm;

const Value *corelab::getMemOper(const Instruction *inst) {
  return getMemOper(const_cast<Instruction *>(inst));
}

Value *corelab::getMemOper(Instruction *inst) {

  if(LoadInst *load = dyn_cast<LoadInst>(inst))
	{
		Value *op = load->getPointerOperand();
		Type *ty = op->getType();
		if ( isa<PointerType>(ty) )
			return op;

		if ( User *userI = dyn_cast<User>(op) )
			op = userI->getOperand(0);
    return op;
	}

  if(StoreInst *store = dyn_cast<StoreInst>(inst))
	{		
		Value *op = store->getPointerOperand();
		Type *ty = op->getType();
		if ( isa<PointerType>(ty) )
			return op;

		if ( User *userI = dyn_cast<User>(op) )
			op = userI->getOperand(0);
    return op;
	}

  if(isa<MemIntrinsic>(inst))
    return NULL;

  if(isa<VAArgInst>(inst))
    assert(false && "Variadic arguments not supported");

  return NULL;
}


void corelab::setMemOper(Instruction *inst, Value *value) {

  if(isa<LoadInst>(inst)) {
    inst->setOperand(0, value);
    return;
  }

  if(isa<StoreInst>(inst)) {
    inst->setOperand(1, value);
    return;
  }

  if(isa<MemIntrinsic>(inst) || isa<VAArgInst>(inst))
    assert(false && "Unimplemented");

  assert(false && "No memory operator");
}
