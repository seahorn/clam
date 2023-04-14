#include "llvm/Analysis/CFGPrinter.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/ADT/Optional.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/DOTGraphTraits.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/GraphWriter.h"

#include "clam/Clam.hh"

/*
 * Print the analyzed CFG with extra information extracted from Clam
 * invariants.
 */

namespace clam {

/**  A wrapper for a function with clam invariants **/
struct ClamFunction {
  // the functions
  const llvm::Function &m_F;
  // loop information
  const llvm::LoopInfo &m_LI;
  // clam pass
  const ClamPass &m_Clam;

  ClamFunction(const llvm::Function &F, const llvm::LoopInfo &LI,
               const ClamPass &Clam)
      : m_F(F), m_LI(LI), m_Clam(Clam) {}
};
} // namespace clam

namespace llvm {

template <>
struct GraphTraits<const clam::ClamFunction *>
    : public GraphTraits<const BasicBlock *> {
  static NodeRef getEntryNode(const clam::ClamFunction *FW) {
    return &FW->m_F.getEntryBlock();
  }
  // nodes_iterator/begin/end - Allow iteration over all nodes in the graph
  using nodes_iterator = pointer_iterator<Function::const_iterator>;
  static nodes_iterator nodes_begin(const clam::ClamFunction *FW) {
    return nodes_iterator(FW->m_F.begin());
  }
  static nodes_iterator nodes_end(const clam::ClamFunction *FW) {
    return nodes_iterator(FW->m_F.end());
  }
  static size_t size(const clam::ClamFunction *FW) { return FW->m_F.size(); }
};

template <>
struct DOTGraphTraits<const clam::ClamFunction *>
    : public DefaultDOTGraphTraits {

  DOTGraphTraits(bool isSimple = false) : DefaultDOTGraphTraits(isSimple) {}

  static std::string getNodeAttributes(const BasicBlock *Node,
                                       const clam::ClamFunction *FW) {

    if (FW->m_LI.isLoopHeader(const_cast<BasicBlock *>(Node)))
      return "color=blue";
    else {
      llvm::Optional<clam::clam_abstract_domain> domOpt =
          FW->m_Clam.getPre(Node);
      if (domOpt.hasValue() && !domOpt.getValue().is_bottom()) {
        return "";
      } else {
        return "color=red";
      }
    }
  }

  static std::string getGraphName(const clam::ClamFunction *F) {
    return "CFG for '" + F->m_F.getName().str() + "' function";
  }

  static std::string getSimpleNodeLabel(const BasicBlock *Node,
                                        const clam::ClamFunction *FW) {
    if (!Node->getName().empty())
      return Node->getName().str();

    std::string Str;
    llvm::Optional<clam::clam_abstract_domain> domOpt = FW->m_Clam.getPre(Node);
    if (domOpt.hasValue() && domOpt.getValue().is_bottom()) {
      Str += "UNREACHABLE\n";
    }
    raw_string_ostream OS(Str);

    Node->printAsOperand(OS, false);
    return OS.str();
  }

  static std::string getCompleteNodeLabel(const BasicBlock *Node,
                                          const clam::ClamFunction *) {
    enum { MaxColumns = 80 };
    std::string Str;
    raw_string_ostream OS(Str);

    if (Node->getName().empty()) {
      Node->printAsOperand(OS, false);
      OS << ":";
    }

    OS << *Node;
    std::string OutStr = OS.str();
    if (OutStr[0] == '\n')
      OutStr.erase(OutStr.begin());

    // Process string output to make it nicer...
    unsigned ColNum = 0;
    unsigned LastSpace = 0;
    for (unsigned i = 0; i != OutStr.length(); ++i) {
      if (OutStr[i] == '\n') { // Left justify
        OutStr[i] = '\\';
        OutStr.insert(OutStr.begin() + i + 1, 'l');
        ColNum = 0;
        LastSpace = 0;
      } else if (OutStr[i] == ';') {             // Delete comments!
        unsigned Idx = OutStr.find('\n', i + 1); // Find end of line
        OutStr.erase(OutStr.begin() + i, OutStr.begin() + Idx);
        --i;
      } else if (ColNum == MaxColumns) { // Wrap lines.
        // Wrap very long names even though we can't find a space.
        if (!LastSpace)
          LastSpace = i;
        OutStr.insert(LastSpace, "\\l...");
        ColNum = i - LastSpace;
        LastSpace = 0;
        i += 3; // The loop will advance 'i' again.
      } else
        ++ColNum;
      if (OutStr[i] == ' ')
        LastSpace = i;
    }
    return OutStr;
  }

  std::string getNodeLabel(const BasicBlock *Node,
                           const clam::ClamFunction *Graph) {
    if (isSimple())
      return getSimpleNodeLabel(Node, Graph);
    else
      return getCompleteNodeLabel(Node, Graph);
  }

  static std::string getEdgeSourceLabel(const BasicBlock *Node,
                                        const_succ_iterator I) {
    // Label source of conditional branches with "T" or "F"
    if (const BranchInst *BI = dyn_cast<BranchInst>(Node->getTerminator()))
      if (BI->isConditional())
        return (I == succ_begin(Node)) ? "T" : "F";

    // Label source of switch edges with the associated value.
    if (const SwitchInst *SI = dyn_cast<SwitchInst>(Node->getTerminator())) {
      unsigned SuccNo = I.getSuccessorIndex();

      if (SuccNo == 0)
        return "def";

      std::string Str;
      raw_string_ostream OS(Str);
      SwitchInst::ConstCaseIt Case =
          SwitchInst::ConstCaseIt::fromSuccessorIndex(SI, SuccNo);
      OS << Case->getCaseValue()->getValue();
      return OS.str();
    }
    return "";
  }
};
} // namespace llvm

namespace clam {

using namespace llvm;

class CFGPrinter : public FunctionPass {
public:
  static char ID; // Pass identification, replacement for typeid
  CFGPrinter() : FunctionPass(ID) {}

  bool runOnFunction(Function &F) override {
    auto &LI = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
    auto &Clam = getAnalysis<ClamPass>();
    std::string Filename = F.getName().str() + ".llvm.cfg.dot";
    writeGraph(F, LI, Clam, Filename);
    return false;
  }

  void print(raw_ostream &OS, const Module * = nullptr) const override {}

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesAll();
    AU.addRequired<LoopInfoWrapperPass>();
    AU.addRequired<ClamPass>();
  }

private:
  static bool writeGraph(Function &F, const LoopInfo &LI, const ClamPass &Clam,
                         std::string Filename) {
    std::error_code EC;
    raw_fd_ostream File(Filename, EC, sys::fs::OF_Text);
    ClamFunction FW(F, LI, Clam);
    if (!EC) {
      errs() << "Writing '" << Filename << "'...";
      llvm::WriteGraph(File, (const ClamFunction *)&FW);
      errs() << "\n";
      return true;
    }
    return false;
  }
};

char CFGPrinter::ID = 0;
Pass *createAnnotatedCFGPrinterPass() { return new CFGPrinter(); }
} // end namespace clam
