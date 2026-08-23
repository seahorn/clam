//== InsertTaintIntrinsic.cc - Insert taint intrinsics based on YAML config ==//
//
// A ModulePass reads a YAML taint configuration file and
// inserts taint intrinsics (e.g., __CRAB_intrinsic_add_tag,
// __CRAB_intrinsic_check_does_not_have_tag)
// at appropriate program points based on the configuration.
//
// The YAML format follows Clang Static Analyzer's taint configuration format.
// But not exactly the same.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/SmallSet.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"

#include "clam/NewPmPasses.hh"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/raw_ostream.h"

#include <climits>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

#include "clam/Support/Debug.hh"

using namespace llvm;

static cl::opt<std::string>
    TaintConfigFile("clam-taint-config",
                    cl::desc("Path to YAML taint configuration file"),
                    cl::init(""), cl::value_desc("filename"));

static cl::opt<bool> PrintTaintInfo("clam-print-taint-info",
                                    cl::desc("Print taint info at sink calls"),
                                    cl::init(false));

#define DEFAULT_TAINT_TAG 1
#define ADD_TAINT_INTRINSIC "add_tag"
#define CHECK_HAS_TAINT_INTRINSIC "check_has_tag"
#define CHECK_DOES_NOT_HAVE_TAINT_INTRINSIC "check_does_not_have_tag"
#define SINK_INTRINSIC CHECK_DOES_NOT_HAVE_TAINT_INTRINSIC
#define PROPAGATE_TAINT_INTRINSIC "move_tag"
#define REMOVE_TAINT_INTRINSIC "remove_tag"
#define DEBUG_TAINT_INTRINSIC "print_tags"

#define SEA_DSA_SET_MODIFIED "sea_dsa_set_modified"

namespace {
int safe_unsigned_to_int(unsigned int value) {
  if (value > INT_MAX) {
    // Handle overflow - return error code or clamp
    return INT_MAX; // or return -1 to indicate error
  }
  return (int)value;
}

static std::unordered_map<std::string, std::string>
buildHiddenFunctionMap(const Module &M) {
  std::unordered_map<std::string, std::string> hiddenNames;

  StringRef triple = M.getTargetTriple();

  // Check if this is a GNU/Linux system with glibc >= 2.7
  if (triple.contains("linux") && triple.contains("gnu")) {
    // Add glibc hidden names
    hiddenNames["fscanf"] = "__isoc99_fscanf";
    hiddenNames["scanf"] = "__isoc99_scanf";
    hiddenNames["sscanf"] = "__isoc99_sscanf";
    hiddenNames["vscanf"] = "__isoc99_vscanf";
    hiddenNames["vfscanf"] = "__isoc99_vfscanf";
    hiddenNames["vsscanf"] = "__isoc99_vsscanf";
  }

  // Add other platform-specific mappings
  if (triple.contains("darwin") || triple.contains("macos")) {
    // macOS might have different hidden names
    // Add macOS-specific mappings if needed
  }

  if (triple.contains("windows") || triple.contains("msvc")) {
    // Windows/MSVC has different naming conventions
    // Add Windows-specific mappings if needed
  }

  return hiddenNames;
}

struct TaintConfig { // now just for C
  using ArgsVecTy = llvm::SmallVector<int, 2>;
  using VariadicIndexTy = std::optional<unsigned>;
  enum class VariadicType { None, Src, Dst };

  struct Common {
    std::string Name;
    VariadicIndexTy VarIndex; // variadic argument starts from this index
  };

  struct Sink : Common {
    ArgsVecTy SinkArgs;
  };

  struct Filter : Common {
    ArgsVecTy FilterArgs;
  };

  struct Propagation : Common {
    ArgsVecTy SrcArgs;
    ArgsVecTy DstArgs;
    VariadicType VarType; // None, Src, Dst
                          // For Src, check VarIndex or default 0
                          // For Dst, since function has only no variadic output
  };

  std::vector<Propagation> Propagations;
  std::vector<Filter> Filters;
  std::vector<Sink> Sinks;

  TaintConfig() = default;
  TaintConfig(const TaintConfig &) = default;
  TaintConfig(TaintConfig &&) = default;
  TaintConfig &operator=(const TaintConfig &) = default;
  TaintConfig &operator=(TaintConfig &&) = default;
};
} // end of anonymous namespace

/// YAML serialization mapping.
LLVM_YAML_IS_SEQUENCE_VECTOR(TaintConfig::Sink)
LLVM_YAML_IS_SEQUENCE_VECTOR(TaintConfig::Filter)
LLVM_YAML_IS_SEQUENCE_VECTOR(TaintConfig::Propagation)

namespace llvm {
namespace yaml {

template <> struct MappingTraits<TaintConfig> {
  static void mapping(IO &IO, TaintConfig &Config) {
    IO.mapOptional("Propagations", Config.Propagations);
    IO.mapOptional("Filters", Config.Filters);
    IO.mapOptional("Sinks", Config.Sinks);
  }
};

template <> struct MappingTraits<TaintConfig::Sink> {
  static void mapping(IO &IO, TaintConfig::Sink &Sink) {
    IO.mapRequired("Name", Sink.Name);
    IO.mapRequired("Args", Sink.SinkArgs);
    IO.mapOptional("VariadicIndex", Sink.VarIndex, std::nullopt);
  }
};

template <> struct MappingTraits<TaintConfig::Filter> {
  static void mapping(IO &IO, TaintConfig::Filter &Filter) {
    IO.mapRequired("Name", Filter.Name);
    IO.mapRequired("Args", Filter.FilterArgs);
    IO.mapOptional("VariadicIndex", Filter.VarIndex, std::nullopt);
  }
};

template <> struct MappingTraits<TaintConfig::Propagation> {
  static void mapping(IO &IO, TaintConfig::Propagation &Propagation) {
    IO.mapRequired("Name", Propagation.Name);
    IO.mapOptional("SrcArgs", Propagation.SrcArgs);
    IO.mapOptional("DstArgs", Propagation.DstArgs);
    IO.mapOptional("VariadicType", Propagation.VarType);
    IO.mapOptional("VariadicIndex", Propagation.VarIndex, std::nullopt);
  }
};

template <> struct ScalarEnumerationTraits<TaintConfig::VariadicType> {
  static void enumeration(IO &IO, TaintConfig::VariadicType &Value) {
    IO.enumCase(Value, "None", TaintConfig::VariadicType::None);
    IO.enumCase(Value, "Src", TaintConfig::VariadicType::Src);
    IO.enumCase(Value, "Dst", TaintConfig::VariadicType::Dst);
  }
};
} // namespace yaml
} // namespace llvm

namespace {
using namespace llvm::yaml;

class TaintConfigParser {
public:
  // Expected like Optional to handle errors
  static Expected<TaintConfig> parseFromFile(const std::string &filename) {
    if (filename.empty()) {
      return make_error<StringError>("No taint config file specified",
                                     inconvertibleErrorCode());
    }

    auto bufferOrErr = MemoryBuffer::getFile(filename);
    if (auto ec = bufferOrErr.getError()) {
      return make_error<StringError>("Cannot open file: " + filename, ec);
    }

    TaintConfig config;
    yaml::Input yin((*bufferOrErr)->getBuffer());
    yin >> config;

    if (auto ec = yin.error()) {
      return make_error<StringError>("YAML parsing error in " + filename, ec);
    }

    return config;
  }
  static void dump(raw_ostream &out, const TaintConfig &config) {
    yaml::Output yout(out);
    TaintConfig configCopy = config;
    yout << configCopy;
  }
};

struct InsertTaintIntrinsic : public ModulePass {
  static char ID;
  TaintConfig m_config;
  bool m_configLoaded = false;
  Module *m_M = nullptr;
  std::unordered_map<std::string, FunctionCallee> m_functionDecls;
  std::unordered_map<std::string, std::string> m_hiddenFunctionNames;
  std::unordered_map<Value *, Value *> m_callRetProcessed_map;

  void setCallRetProcessed(CallInst &CI, Value *inst) {
    m_callRetProcessed_map[&CI] = inst;
  }

  Value *getCallRetProcessed(CallInst &CI) {
    auto it = m_callRetProcessed_map.find(&CI);
    return it != m_callRetProcessed_map.end() ? it->second : nullptr;
  }

  InsertTaintIntrinsic() : ModulePass(ID) {}

  bool runOnModule(Module &M) override {
    m_M = &M;
    m_hiddenFunctionNames = buildHiddenFunctionMap(*m_M);
    // Load configuration file if specified
    if (!TaintConfigFile.empty() && !m_configLoaded) {
      auto configOrErr = TaintConfigParser::parseFromFile(TaintConfigFile);
      // Check if it succeeded
      if (!configOrErr) {
        CLAM_ERROR("[InsertTaintIntrinsic] Error loading config: "
                   << toString(configOrErr.takeError()) << "\n");
        return false;
      }
      m_config = std::move(*configOrErr);
      m_configLoaded = true;

      CRAB_LOG("taint-intrinsic",
               errs() << "[InsertTaintIntrinsic] Loaded taint config from "
                      << TaintConfigFile << "\n");
      // TaintConfigParser::dump(errs(), m_config);
    }

    if (!m_configLoaded) {
      CRAB_LOG(
          "taint-intrinsic",
          errs() << "[InsertTaintIntrinsic] No config loaded, pass through\n");
      return false;
    }

    auto isFunctionNameMatched = [this](const StringRef &FuncName,
                                        const std::string &TargetName) {
      if (FuncName == TargetName)
        return true;
      auto it = m_hiddenFunctionNames.find(TargetName);
      if (it != m_hiddenFunctionNames.end() && FuncName == it->second)
        return true;
      return false;
    };

    bool Changed = false;

    // Declare intrinsics we might need
    findFunctionDeclaration(ADD_TAINT_INTRINSIC);
    findFunctionDeclaration(CHECK_DOES_NOT_HAVE_TAINT_INTRINSIC);
    findFunctionDeclaration(CHECK_HAS_TAINT_INTRINSIC);
    findFunctionDeclaration(PROPAGATE_TAINT_INTRINSIC);
    findFunctionDeclaration(DEBUG_TAINT_INTRINSIC);
    if (!m_config.Filters.empty()) {
      findFunctionDeclaration(REMOVE_TAINT_INTRINSIC);
    }

    // Process each function in the module
    for (Function &F : *m_M) {
      if (F.isDeclaration())
        continue;

      for (BasicBlock &BB : F) {
        for (Instruction &I : BB) {
          if (auto *CI = dyn_cast<CallInst>(&I)) {
            bool IsTaintedUse = false;
            Function *Callee = CI->getCalledFunction();
            if (!Callee)
              continue; // skip indirect calls

            // process Propagations
            for (const auto &Propagation : m_config.Propagations) {
              bool isVariadicDst =
                  (Propagation.VarType == TaintConfig::VariadicType::Dst);
              bool isVariadicSrc =
                  (Propagation.VarType == TaintConfig::VariadicType::Src);
              bool hasSrc = (Propagation.SrcArgs.size() > 0 || isVariadicSrc);
              bool hasDst = (Propagation.DstArgs.size() > 0 || isVariadicDst);
              if (isFunctionNameMatched(Callee->getName(), Propagation.Name) &&
                  hasDst) {
                // Found matching propagation
                if (!hasSrc) {
                  IsTaintedUse = insertAddTaintIntrinsics(
                      *CI, Propagation, m_functionDecls[ADD_TAINT_INTRINSIC]);
                  Changed |= IsTaintedUse;
                } else {
                  IsTaintedUse = insertPropagateTaintIntrinsics(
                      *CI, Propagation,
                      m_functionDecls[PROPAGATE_TAINT_INTRINSIC]);
                  Changed |= IsTaintedUse;
                }
              }
            }

            // process Sinks
            for (const auto &Sink : m_config.Sinks) {
              if (isFunctionNameMatched(Callee->getName(), Sink.Name) &&
                  (Sink.SinkArgs.size() > 0 || Sink.VarIndex.has_value())) {
                // Found matching sink
                IsTaintedUse = insertSinkIntrinsics(
                    *CI, Sink, m_functionDecls[SINK_INTRINSIC],
                    m_functionDecls[DEBUG_TAINT_INTRINSIC]);
                Changed |= IsTaintedUse;
              }
            }

            // process Filters
            for (const auto &Filter : m_config.Filters) {
              if (isFunctionNameMatched(Callee->getName(), Filter.Name) &&
                  (Filter.FilterArgs.size() > 0 || Filter.VarIndex.has_value()) &&
                  m_functionDecls.count(REMOVE_TAINT_INTRINSIC)) {
                // Found matching filter (sanitizer)
                IsTaintedUse = insertFilterIntrinsics(
                    *CI, Filter, m_functionDecls[REMOVE_TAINT_INTRINSIC]);
                Changed |= IsTaintedUse;
              }
            }

            // delete its function body if it has one
            // Like propagation and sink functions are treated as no-ops
            if (Callee && !Callee->isDeclaration() && IsTaintedUse) {
              Callee->deleteBody();
            }
          }
        }
      }
    }

    if (Changed) {
      CRAB_LOG(
          "taint-intrinsic",
          errs() << "[InsertTaintIntrinsic] Inserted all taint intrinsics\n");
    }

    return Changed;
  }

  void findFunctionDeclaration(std::string name) {
    std::string intrinsicName = "__CRAB_intrinsic_" + name;
    for (Function &F : *m_M) {
      if (F.isDeclaration() && F.getName() == intrinsicName) {
        FunctionCallee func =
            m_M->getOrInsertFunction(intrinsicName, F.getFunctionType());
        m_functionDecls.insert({name, func});
        // Get the intrinsic and mark it as no side effects
        // FIXME: side effects free will cause taint propagation wrong.
        // if (Function *Fn = dyn_cast<Function>(func.getCallee())) {
        //   Fn->addFnAttr(Attribute::ReadNone);
        //   // Mark pointer parameters as nocapture
        //   for (unsigned i = 0; i < Fn->arg_size(); ++i) {
        //       if (Fn->getArg(i)->getType()->isPointerTy()) {
        //           Fn->addParamAttr(i, Attribute::NoCapture);
        //       }
        //   }
        // }
        return;
      }
    }
    CLAM_WARNING("[InsertTaintIntrinsic]: Function declaration for "
                 << intrinsicName << " not found in module\n");
  }

  /// @brief Insert sink intrinsics for a call instruction
  /// @param CI The call instruction
  /// @param rules The taint sink rules
  /// @param sinkIntrinsic The sink intrinsic function
  /// @param debugTaintIntrinsic The debug taint intrinsic function
  /// @return True if any changes were made
  bool insertSinkIntrinsics(CallInst &CI, const TaintConfig::Sink &rules,
                            FunctionCallee &sinkIntrinsic,
                            FunctionCallee &debugTaintIntrinsic) {
    // Create IRBuilder for inserting instructions before call instruction
    // The conversion is like this:
    // Before:
    //  sink(arg1, arg2, ...)
    // - Name: sink
    //  Args: [0, 1]
    // After:
    //  SINK_INTRINSIC(arg1)   // for 0 index
    //  SINK_INTRINSIC(arg2)   // for 1 index
    //  sink(arg1, arg2, ...)
    // Alternative, if VariadicIndex is set to N:
    // Before:
    //  sink(arg1, arg2, ..., argN)
    // - Name: sink
    //  Args: [0, 6]
    //  VariadicIndex: 2 # Indicates variadic args starts from index 2
    //          # if it specifies, it means the rest of args need to be checked
    //          # if you want to let specific args checked and others ignored.
    //          # do not use VariadicIndex.
    // After:
    //  SINK_INTRINSIC(arg1)   // for 0 index
    //  SINK_INTRINSIC(arg3)   // for variadic args starting from index 2
    //  ...
    //  SINK_INTRINSIC(argN)
    IRBuilder<> Builder(&CI);
    bool Changed = false;
    CRAB_LOG("taint-intrinsic", errs()
                                    << "[Sink] visit CallInst " << CI << "\n");
    int VariadicIndex =
        safe_unsigned_to_int(rules.VarIndex.value_or(UINT_MAX));
    int NumArgs = safe_unsigned_to_int(CI.arg_size());

    // Lambda to handle sink checking with complete operation
    auto checkSinkArg = [&](Value *arg, int argIdx) -> bool {
      Value *argPtr = nullptr;

      if (arg->getType()->isPointerTy()) {
        argPtr = (arg->getType() == Builder.getPtrTy())
                     ? arg
                     : Builder.CreateBitCast(arg, Builder.getPtrTy(),
                                             "taint.cast");
      } else {
        // Create alloca for non-pointer value
        BasicBlock &EntryBB = CI.getFunction()->getEntryBlock();
        IRBuilder<> AllocaBuilder(&EntryBB, EntryBB.getFirstInsertionPt());
        AllocaInst *Alloca =
            AllocaBuilder.CreateAlloca(arg->getType(), nullptr, "tmp");
        Builder.CreateStore(arg, Alloca);
        argPtr =
            Builder.CreateBitCast(Alloca, Builder.getPtrTy(), "taint.cast");
      }

      // Create the intrinsic call
      if (PrintTaintInfo) {
        // Insert debug intrinsic call
        CallInst *debugCall = Builder.CreateCall(debugTaintIntrinsic, {argPtr});
        debugCall->setDebugLoc(CI.getDebugLoc());
      }
      Value *Tag = Builder.getInt64(DEFAULT_TAINT_TAG);
      // FIXME: incorrect handler
      // if (ConstantExpr *CE = dyn_cast<ConstantExpr>(argPtr)) {
      //   Instruction *Inst = CE->getAsInstruction(&CI);
      //   argPtr = Inst;
      // }
      CallInst *sinkCall = Builder.CreateCall(sinkIntrinsic, {argPtr, Tag});
      sinkCall->setDebugLoc(CI.getDebugLoc());

      // Log the insertion
      CRAB_LOG("taint-intrinsic", errs() << "[Sink] Inserted " << SINK_INTRINSIC
                                         << " from "
                                         << CI.getCalledFunction()->getName()
                                         << " arg " << argIdx << "\n");

      return true;
    };

    // Insert check_has_tag intrinsics for each sink argument
    for (int ArgIdx : rules.SinkArgs) {
      assert(ArgIdx >= 0);
      if (ArgIdx >= 0 && ArgIdx < NumArgs && ArgIdx < VariadicIndex) {
        Value *Arg = CI.getArgOperand(ArgIdx);
        Changed |= checkSinkArg(Arg, ArgIdx);
      }
    }

    for (int ArgIdx = 0; ArgIdx < NumArgs; ArgIdx++) {
      assert(ArgIdx >= 0);
      if (ArgIdx >= VariadicIndex) {
        Value *Arg = CI.getArgOperand(ArgIdx);
        Changed |= checkSinkArg(Arg, ArgIdx);
      }
    }

    return Changed;
  }

  /// @brief Insert remove taint intrinsics after a sanitizer call
  /// @param CI The call instruction
  /// @param rules The taint filter rules
  /// @param removeTaintIntrinsic The remove taint intrinsic function
  /// @return True if any changes were made
  bool insertFilterIntrinsics(CallInst &CI, const TaintConfig::Filter &rules,
                              FunctionCallee &removeTaintIntrinsic) {
    // The conversion is like this:
    // Before:
    //  sanitize(arg1, arg2, ...)
    // - Name: sanitize
    //  Args: [0]
    // After:
    //  sanitize(arg1, arg2, ...)
    //  REMOVE_TAINT_INTRINSIC(arg1)   // for 0 index
    bool Changed = false;
    Instruction *InsertPoint = CI.getNextNonDebugInstruction();
    IRBuilder<> Builder(InsertPoint);
    int VariadicIndex =
        safe_unsigned_to_int(rules.VarIndex.value_or(UINT_MAX));
    int NumArgs = safe_unsigned_to_int(CI.arg_size());
    CRAB_LOG("taint-intrinsic",
             errs() << "[Filter] visit CallInst " << CI << "\n");
    auto removeTaintArg = [&](Value *arg, int argIdx) -> bool {
      if (!arg || !arg->getType()->isPointerTy()) {
        // skip, only pointed-to memory can be sanitized
        return false;
      }
      Value *argPtr = (arg->getType() == Builder.getPtrTy())
                          ? arg
                          : Builder.CreateBitCast(arg, Builder.getPtrTy(),
                                                  "taint.cast");
      Value *Tag = Builder.getInt64(DEFAULT_TAINT_TAG);
      CallInst *removeCall =
          Builder.CreateCall(removeTaintIntrinsic, {argPtr, Tag});
      removeCall->setDebugLoc(CI.getDebugLoc());
      CRAB_LOG("taint-intrinsic", errs() << "[Filter] Inserted "
                                         << REMOVE_TAINT_INTRINSIC
                                         << " for arg " << argIdx << "\n");
      return true;
    };

    for (int ArgIdx : rules.FilterArgs) {
      if (ArgIdx >= 0 && ArgIdx < NumArgs && ArgIdx < VariadicIndex) {
        Changed |= removeTaintArg(CI.getArgOperand(ArgIdx), ArgIdx);
      }
    }

    for (int ArgIdx = 0; ArgIdx < NumArgs; ArgIdx++) {
      if (ArgIdx >= VariadicIndex) {
        Changed |= removeTaintArg(CI.getArgOperand(ArgIdx), ArgIdx);
      }
    }

    return Changed;
  }

  /// @brief Handle tainting of return values for non-pointer types
  /// @param CI The call instruction whose return value needs tainting
  /// @param TaintIntrinsic The taint intrinsic function
  /// @param Builder IRBuilder positioned after the call instruction
  /// @param Src The source value to propagate taint from (if any)
  /// @param srcIdx The index of the source argument
  /// @return True if taint intrinsic was inserted
  bool insertInstructionsForReturnValue(CallInst &CI,
                                        FunctionCallee &TaintIntrinsic,
                                        IRBuilder<> &Builder, Value *Src,
                                        int srcIdx) {

    Type *RetType = CI.getType();
    std::string idx = std::to_string(srcIdx);

    // Early exit for void return
    if (RetType->isVoidTy()) {
      return false;
    }

    Value *RetValPtr = nullptr;
    StoreInst *Store = nullptr;
    bool NeedsLoadReplacement = false;

    // Check if already processed
    if (Value *Processed = getCallRetProcessed(CI)) {
      RetValPtr = Processed;
      // A previous rule already boxed this return value, and this rule's
      // builder starts right after the call, which is BEFORE the cached
      // definition; using it there would break dominance. Move the
      // insertion point past the cached definition, but only FORWARD:
      // moving back would jump over values this rule inserted meanwhile
      // (e.g. boxed source arguments) and break their uses instead.
      if (auto *ProcessedI = dyn_cast<Instruction>(Processed)) {
        auto InsertPt = Builder.GetInsertPoint();
        bool insertPtAfterDef =
            InsertPt != ProcessedI->getParent()->end() &&
            InsertPt->getParent() == ProcessedI->getParent() &&
            ProcessedI->comesBefore(&*InsertPt);
        if (!insertPtAfterDef) {
          Builder.SetInsertPoint(ProcessedI->getParent(),
                                 std::next(ProcessedI->getIterator()));
        }
      }
    } else {
      if (RetType->isPointerTy()) {
        // Pointer return type: just cast if needed
        RetValPtr = &CI;
        if (RetValPtr->getType() != Builder.getPtrTy()) {
          RetValPtr = Builder.CreateBitCast(RetValPtr, Builder.getPtrTy(),
                                            "taint.cast");
        }
      } else {
        // Before:
        //   %tmp11 = call i32 (...) @taint_int()
        //  ||
        //  V
        // After:
        //   %tmp = alloca i32
        //   ...
        //   %tmp3 = call i32 (...) @taint_int()
        //   store i32 %tmp3, i32* %tmp
        //   %tmp1 = bitcast i32* %tmp to i8*
        //   TAINT_INTRINSIC(%tmp1)
        //   %tmp11 = load i32, i32* %tmp
        // Non-pointer return type: need alloca+store+load pattern
        BasicBlock &EntryBB = CI.getFunction()->getEntryBlock();
        IRBuilder<> AllocaBuilder(&EntryBB, EntryBB.getFirstInsertionPt());

        // Step 1: Create alloca
        AllocaInst *Alloca =
            AllocaBuilder.CreateAlloca(RetType, nullptr, "tmp");
        // Step 2: Store call result
        Store = Builder.CreateStore(&CI, Alloca);
        // Cast alloca to i8*
        RetValPtr = Alloca;
        if (RetValPtr->getType() != Builder.getPtrTy()) {
          RetValPtr = Builder.CreateBitCast(RetValPtr, Builder.getPtrTy(),
                                            "taint.cast");
        }
        NeedsLoadReplacement = true;
      }

      // Cache for future use
      setCallRetProcessed(CI, RetValPtr);
    }

    // Step 3: Insert taint intrinsic (always executed)
    CallInst *TaintCall;
    if (Src) {
      TaintCall = Builder.CreateCall(TaintIntrinsic, {Src, RetValPtr});
    } else {
      Value *Tag = Builder.getInt64(DEFAULT_TAINT_TAG);
      TaintCall = Builder.CreateCall(TaintIntrinsic, {RetValPtr, Tag});
    }
    TaintCall->setDebugLoc(CI.getDebugLoc());
    CRAB_LOG("taint-intrinsic",
             errs() << "[" << (Src ? "Prop" : "Add") << "] Inserted "
                    << (Src ? PROPAGATE_TAINT_INTRINSIC : ADD_TAINT_INTRINSIC)
                    << (Src ? " from arg " + idx + " to" : " for")
                    << " return value\n");

    // Step 4: Insert load and fix uses (only if we created alloca+store)
    if (NeedsLoadReplacement) {
      Value *Source;
      if (auto *BC = dyn_cast<BitCastInst>(RetValPtr)) {
        Source = BC->getOperand(0);
      } else {
        // It is NOT a bitcast (might be an Alloca, Argument, GEP, etc.)
        Source = RetValPtr;
      }
      LoadInst *Load =
          Builder.CreateLoad(RetType, cast<AllocaInst>(Source), CI.getName());
      // Replace all uses of the original call with the load
      CI.replaceAllUsesWith(Load);
      // Fix the store to reference the original call instruction
      Store->setOperand(0, &CI);
    }

    return true;
  }

  /// @brief Insert propagate taint intrinsics for a call instruction
  /// @param CI The call instruction
  /// @param rules The taint propagation rules
  /// @param propagateTaintIntrinsic The propagate taint intrinsic function
  /// @return True if any changes were made
  bool insertPropagateTaintIntrinsics(CallInst &CI,
                                      const TaintConfig::Propagation &rules,
                                      FunctionCallee &propagateTaintIntrinsic) {
    // Create IRBuilder for inserting instructions before call instruction
    // Add DstArgs
    // The conversion is like this:
    // Before:
    //  ret = func(arg1, arg2, ...)
    // - Name: func
    //  SrcArgs: [0]     # first arg is the source to trigger
    //  DstArgs: [-1, 1] # prop taint to return value and second arg
    // After:
    //  ret = func(arg1, arg2, ...)
    //  PROPAGATE_TAINT_INTRINSIC(arg1, arg2)   // move 0 to 1
    //  PROPAGATE_TAINT_INTRINSIC(arg1, ret)    // move 0 to ret
    bool changed = false;
    Instruction *InsertPoint = CI.getNextNonDebugInstruction();
    IRBuilder<> Builder(InsertPoint);
    bool isSrcVariadic = (rules.VarType == TaintConfig::VariadicType::Src);
    bool isDstVariadic = (rules.VarType == TaintConfig::VariadicType::Dst);
    int VariadicIndex =
        (isSrcVariadic || isDstVariadic)
            ? safe_unsigned_to_int(rules.VarIndex.value_or(UINT_MAX))
            : INT_MAX;
    int NumArgs = safe_unsigned_to_int(CI.arg_size());
    CRAB_LOG("taint-intrinsic", errs()
                                    << "[Prop] visit CallInst " << CI << "\n");
    std::unordered_map<Value *, std::pair<Value *, bool>> processedSrc;
    using IndxSetTy = SmallSet<int, 4>;

    // Lambda to get pointer for taint propagation (handles both pointer and
    // non-pointer values)
    auto getOfInsertIfPointerForTaint = [&](Value *val,
                                            IRBuilder<> &builder) -> Value * {
      if (processedSrc.find(val) != processedSrc.end()) {
        return processedSrc[val].first;
      }
      Value *ret = nullptr;
      if (val->getType()->isPointerTy()) {
        // Cast to i8* if needed
        if (val->getType() != builder.getPtrTy()) {
          ret =
              builder.CreateBitCast(val, builder.getPtrTy(), "taint.cast");
        } else {
          ret = val;
        }
      } else {
        // Create alloca for non-pointer value
        BasicBlock &EntryBB = CI.getFunction()->getEntryBlock();
        IRBuilder<> AllocaBuilder(&EntryBB, EntryBB.getFirstInsertionPt());
        AllocaInst *a =
            AllocaBuilder.CreateAlloca(val->getType(), nullptr, "tmp");
        // Store the value
        builder.CreateStore(val, a);
        // Return the alloca pointer (cast to i8*)
        ret = builder.CreateBitCast(a, builder.getPtrTy(), "taint.cast");
      }
      processedSrc[val] = {ret, true};
      return ret;
    };

    auto insertPropArgs = [&](Value *arg, int argIdx,
                              IndxSetTy &dstArgIndices) -> bool {
      Value *src = getOfInsertIfPointerForTaint(arg, Builder);
      for (int dstArg : dstArgIndices) {
        Value *dst = CI.getArgOperand(dstArg);
        assert(dst);
        if (dst->getType()->isPointerTy()) {
          // Cast to i8* if needed
          if (dst->getType() != Builder.getPtrTy()) {
            dst = Builder.CreateBitCast(dst, Builder.getPtrTy(),
                                        "taint.cast");
          }
        } else {
          // skip, only pointer args can be tainted
          // temp registers don't need tainting since their values are
          // immutable
          continue;
        }
        // Create call to propagate_taint intrinsic
        Builder.CreateCall(propagateTaintIntrinsic, {src, dst});
        CRAB_LOG("taint-intrinsic", errs() << "[Prop] Inserted "
                                           << PROPAGATE_TAINT_INTRINSIC
                                           << " for arg " << argIdx
                                           << " to arg " << dstArg << "\n");
      }
      return true;
    };

    bool addRet = false;
    IndxSetTy srcArgIndices, dstArgIndices;

    // Collect source set
    for (int SrcIdx : rules.SrcArgs) {
      if (SrcIdx >= 0 && SrcIdx < NumArgs && SrcIdx < VariadicIndex) {
        srcArgIndices.insert(SrcIdx);
      }
    }
    // Add variadic source indices
    if (isSrcVariadic) {
      for (int ArgIdx = VariadicIndex; ArgIdx < NumArgs; ArgIdx++) {
        srcArgIndices.insert(ArgIdx);
      }
    }

    // If no source args found, nothing to propagate
    if (srcArgIndices.empty()) {
      return changed;
    }

    // Build destination set
    for (int DstIdx : rules.DstArgs) {
      if (DstIdx == -1) {
        addRet = true;
      } else if (DstIdx >= 0 && DstIdx < NumArgs &&
                 !srcArgIndices.contains(DstIdx)) {
        Value *dstArg = CI.getArgOperand(DstIdx);
        if (dstArg) {
          dstArgIndices.insert(DstIdx);
        }
      }
    }
    // Add variadic destination indices
    if (isDstVariadic) {
      for (int ArgIdx = VariadicIndex; ArgIdx < NumArgs; ArgIdx++) {
        if (!srcArgIndices.contains(ArgIdx)) {
          Value *dstArg = CI.getArgOperand(ArgIdx);
          if (dstArg) {
            dstArgIndices.insert(ArgIdx);
          }
        }
      }
    }

    // Process each source arg -> propagate to all destinations
    for (int SrcIdx : srcArgIndices) {
      Value *srcArg = CI.getArgOperand(SrcIdx);
      if (!srcArg)
        continue;

      // Propagate to destination args
      if (!dstArgIndices.empty()) {
        changed |= insertPropArgs(srcArg, SrcIdx, dstArgIndices);
      }

      // Propagate to return value
      if (addRet) {
        Value *src = getOfInsertIfPointerForTaint(srcArg, Builder);
        changed |= insertInstructionsForReturnValue(CI, propagateTaintIntrinsic,
                                                    Builder, src, SrcIdx);
      }
    }
    return changed;
  }

  /// @brief Insert add taint intrinsics for a call instruction
  /// @param CI The call instruction
  /// @param rules The taint addition rules
  /// @param addTaintIntrinsic The add taint intrinsic function
  /// @return True if any changes were made
  bool insertAddTaintIntrinsics(CallInst &CI,
                                const TaintConfig::Propagation &rules,
                                FunctionCallee &addTaintIntrinsic) {
    // Add DstArgs
    // The conversion is like this:
    // Before:
    //  ret = func(arg1, arg2, ...)
    // - Name: func
    //  DstArgs: [0, -1]
    // After:
    //  ret = func(arg1, arg2, ...)
    //  ADD_TAINT_INTRINSIC(arg1)   // for 0 index
    //  ADD_TAINT_INTRINSIC(ret)  // for -1 index
    bool Changed = false;
    Instruction *InsertPoint = CI.getNextNonDebugInstruction();
    bool addRet = false;
    IRBuilder<> Builder(InsertPoint);
    bool isDstVariadic = (rules.VarType == TaintConfig::VariadicType::Dst);
    int VariadicIndex =
        isDstVariadic
            ? safe_unsigned_to_int(rules.VarIndex.value_or(UINT_MAX))
            : INT_MAX;
    int NumArgs = safe_unsigned_to_int(CI.arg_size());
    CRAB_LOG("taint-intrinsic", errs()
                                    << "[Add] visit CallInst " << CI << "\n");
    auto insertTaintArg = [&](Value *arg, int argIdx) -> bool {
      Value *argPtr = nullptr;
      if (!arg || !arg->getType()->isPointerTy()) {
        // skip, only pointer args can be tainted
        // temp registers don't need tainting since their values are immutable
        return false;
      }
      argPtr = (arg->getType() == Builder.getPtrTy())
                   ? arg
                   : Builder.CreateBitCast(arg, Builder.getPtrTy(),
                                           "taint.cast");
      // make a constant 1 as second argument
      Value *Tag = Builder.getInt64(DEFAULT_TAINT_TAG);
      CallInst *addCall = Builder.CreateCall(addTaintIntrinsic, {argPtr, Tag});
      addCall->setDebugLoc(CI.getDebugLoc());
      CRAB_LOG("taint-intrinsic", errs() << "[Add] Inserted "
                                         << ADD_TAINT_INTRINSIC << " for arg "
                                         << argIdx << "\n");
      return true;
    };

    for (int ArgIdx : rules.DstArgs) {
      if (ArgIdx >= 0 && ArgIdx < NumArgs && ArgIdx < VariadicIndex) {
        Value *Arg = CI.getArgOperand(ArgIdx);
        Changed |= insertTaintArg(Arg, ArgIdx);
      } else if (ArgIdx == -1) {
        addRet = true;
      } else {
        continue;
      }
    }

    for (int ArgIdx = 0; ArgIdx < NumArgs; ArgIdx++) {
      assert(ArgIdx >= 0);
      if (ArgIdx >= VariadicIndex) {
        Value *Arg = CI.getArgOperand(ArgIdx);
        Changed |= insertTaintArg(Arg, ArgIdx);
      }
    }

    if (addRet) {
      Changed |= insertInstructionsForReturnValue(
          CI, addTaintIntrinsic, Builder, nullptr, 0 /*unused*/);
    }
    return Changed;
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    // This pass modifies the IR by inserting calls
    // It does not preserve analyses
  }

  StringRef getPassName() const override {
    return "Insert Taint Intrinsics based on YAML configuration";
  }
};

char InsertTaintIntrinsic::ID = 0;

} // end anonymous namespace

namespace clam {
llvm::Pass *createInsertTaintIntrinsicPass() {
  return new InsertTaintIntrinsic();
}

llvm::PreservedAnalyses
InsertTaintIntrinsicPass::run(llvm::Module &M, llvm::ModuleAnalysisManager &) {
  // The legacy pass uses no analyses; share its implementation directly.
  InsertTaintIntrinsic P;
  if (!P.runOnModule(M)) {
    return llvm::PreservedAnalyses::all();
  }
  return llvm::PreservedAnalyses::none();
}
} // namespace clam

// Register the pass
static RegisterPass<InsertTaintIntrinsic>
    X("insert-taint-intrinsic",
      "Insert taint intrinsics based on YAML configuration", false, false);
