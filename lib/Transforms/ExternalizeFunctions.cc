#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Demangle/Demangle.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Regex.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

static llvm::cl::list<std::string>
ExternalizeFunctionNames("clam-externalize-function",
			  llvm::cl::desc("Set the linkage to external"),
			  llvm::cl::ZeroOrMore, llvm::cl::CommaSeparated);

static llvm::cl::opt<bool>
RemoveBodies("clam-externalize-functions-delete",
	     llvm::cl::desc("Delete bodies of externalized functions"),
	     llvm::cl::init(true),
	     llvm::cl::Hidden);

namespace clam {

class ExternalizeFunctions : public ModulePass {

  struct MatchRegex : public std::unary_function<Function *, bool> {
    llvm::Optional<llvm::Regex> m_re;
    MatchRegex(std::string s) {
      if (s != "") {
        m_re = llvm::Regex(s);
        std::string Error;
        if (!m_re->isValid(Error)) {
          m_re = llvm::None;
        }
      }
    }
    bool operator()(Function *F) {
      return m_re && m_re->match(F->getName());
    }
  };

public:
  static char ID;

  ExternalizeFunctions() : ModulePass(ID) {}

  virtual bool runOnModule(Module &M) override {
    if (ExternalizeFunctionNames.begin() == ExternalizeFunctionNames.end())
      return false;

    SmallPtrSet<Function *, 16> externalizeFunctions;
    for (auto name : ExternalizeFunctionNames) {    
      MatchRegex filter(name);
      for (auto &F : M) {
	if (filter(&F)) {
	  externalizeFunctions.insert(&F);
	}
      }
    }
    

    bool Change = false;
    // Delete function bodies and set external linkage
    for (Function &F : M) {
      if (F.isDeclaration() || externalizeFunctions.count(&F) == 0) {
        continue;
      }
      Change = true;
      
      unsigned numMemInsts = 0;
      SmallPtrSet<Function*, 32> skippedFunctions;
      for (auto &B: F) {
	for (auto &I: B) {
	  if (isa<LoadInst>(I) || isa<StoreInst>(I)) {
	    numMemInsts++;
	  }
	  if (CallBase *CB = dyn_cast<CallBase>(&I)) {
	    if (Function* calleeF = CB->getCalledFunction()) {
	      if (!calleeF->empty()) {
		skippedFunctions.insert(calleeF);
	      }
	    }
	  }
	}
      }
      errs() << "EXTERNALIZING " << llvm::demangle(F.getName().str()) << " with "
	     << numMemInsts << " memory instructions and ";
      if (skippedFunctions.empty()) {
	errs() << " no calls\n";
      } else {
	errs() << " calls to:\n";
	for (Function* fn: skippedFunctions) {
	  errs() << "\t" << fn->getName() << "\n";
	}
      }
      
      if (!RemoveBodies && F.hasLocalLinkage()) {
        // in C++ local names are mangled with _ZL prefix, as we make functions
        // external, also rename
        auto name = F.getName();
        if (name.startswith("_ZL") && name.size() > 3) {
          F.setName("_Z" + name.substr(3));
        }
        // -- change linkage to external
        F.setLinkage(GlobalValue::ExternalLinkage);
      } else {
        F.deleteBody();
        F.setComdat(nullptr);
      }
    }

    // get rid of aliases, even if the function has not been removed

    // Delete global aliases: aliases cannot point to a function
    // declaration so if there is an alias to an external function
    // we also need to remove all its aliases.
    std::vector<GlobalAlias *> aliases;
    for (GlobalAlias &GA : M.aliases()) {
      aliases.push_back(&GA);
    }

    for (GlobalAlias *GA : aliases) {
      if (Function *aliasee = dyn_cast<Function>(GA->getAliasee())) {
        if (externalizeFunctions.count(aliasee) > 0) {
          GA->replaceAllUsesWith(aliasee);
          GA->eraseFromParent();
          Change = true;
        }
      }
    }

    llvm::verifyModule(M, &(errs()), nullptr);
    return Change;
  }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesAll();
  }

  virtual StringRef getPassName() const override {
    return "Externalize all selected functions";
  }
};

char ExternalizeFunctions::ID = 0;

Pass *createExternalizeFunctionsPass() { return new ExternalizeFunctions(); }

} // namespace clam
