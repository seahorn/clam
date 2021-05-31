#include "clam/config.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"

#include "seadsa/Graph.hh"

#include "SeaDsaHeapAbstractionUtils.hh"

#include <algorithm>

namespace clam {

using namespace llvm;
using namespace seadsa;

namespace seadsa_heap_abs_impl {

bool NodeOrdering::operator()(const seadsa::Node *n1,
                              const seadsa::Node *n2) const {
  return n1->getId() < n2->getId();
}

void set_difference(NodeSet &s1, NodeSet &s2) {
  NodeSet s3;
  std::set_difference(s1.begin(), s1.end(), s2.begin(), s2.end(),
                      std::inserter(s3, s3.end()));
  std::swap(s3, s1);
}

void set_union(NodeSet &s1, NodeSet &s2) {
  NodeSet s3;
  std::set_union(s1.begin(), s1.end(), s2.begin(), s2.end(),
                 std::inserter(s3, s3.end()));
  std::swap(s3, s1);
}

bool isInteger::operator()(const llvm::Type *t) {
  bool is_int = (t->isIntegerTy() && !t->isIntegerTy(1));
  if (is_int) {
    // XXX: We use bitwidth for overflow purposes so taking the
    // minimum is the most conservative choice.
    m_bitwidth =
        (m_bitwidth == 0 ? t->getIntegerBitWidth()
                         : std::min(m_bitwidth, t->getIntegerBitWidth()));
  }
  return is_int;
}

bool isBool::operator()(const llvm::Type *t) const { return t->isIntegerTy(1); }

bool isIntegerOrBool::operator()(const llvm::Type *t) const {
  return t->isIntegerTy();
}

bool isPointer::operator()(const llvm::Type *t) const {
  return t->isPointerTy();
}

void markReachableNodes(const Node *n, NodeSet &set) {
  if (!n)
    return;
  assert(!n->isForwarding() && "Cannot mark a forwarded node");

  if (set.insert(n).second)
    for (auto const &edg : n->links())
      markReachableNodes(edg.second->getNode(), set);
}

/// Computes the set of all nodes reachable by fn in graph g.
/// Sets inputReach to nodes reachable from arguments or globals
/// Sets retReach to nodes from return value
void reachableNodes(const Function &fn, Graph &g, NodeSet &inputReach,
                    NodeSet &retReach) {
  // formal parameters
  for (Function::const_arg_iterator I = fn.arg_begin(), E = fn.arg_end();
       I != E; ++I) {
    const Value &arg = *I;
    if (g.hasCell(arg)) {
      Cell &c = g.mkCell(arg, Cell());
      markReachableNodes(c.getNode(), inputReach);
    }
  }

  // globals
  for (auto &kv : llvm::make_range(g.globals_begin(), g.globals_end())) {
    markReachableNodes(kv.second->getNode(), inputReach);
  }

  // return value
  if (g.hasRetCell(fn)) {
    markReachableNodes(g.getRetCell(fn).getNode(), retReach);
  }
}

/// Computes Node reachable from the call arguments in the graph.
/// reach - all reachable nodes
/// outReach - subset of reach that is only reachable from return (and
/// not reachable from the arguments or globals).
void argReachableNodes(const llvm::Function &fn, Graph &G, NodeSet &reach,
                       NodeSet &outReach) {
  reachableNodes(fn, G, reach, outReach);
  set_difference(outReach, reach);
  set_union(reach, outReach);
}

} // end namespace seadsa_heap_abs_impl
} // namespace clam
