#pragma once
#include "clam/config.h"

#include <set>

namespace llvm {
class Type;
class Function;
} // namespace llvm

namespace seadsa {
class Graph;
class Node;
} // namespace seadsa

namespace clam {
namespace seadsa_heap_abs_impl {

struct NodeOrdering {
  bool operator()(const seadsa::Node *n1, const seadsa::Node *n2) const;
};

using NodeSet = std::set<const seadsa::Node *, NodeOrdering>;

void set_difference(NodeSet &s1, NodeSet &s2);

void set_union(NodeSet &s1, NodeSet &s2);

void markReachableNodes(const seadsa::Node *n, NodeSet &set);

void reachableNodes(const llvm::Function &fn, seadsa::Graph &g,
                    NodeSet &inputReach, NodeSet &retReach);

/// Computes Node reachable from the call arguments in the graph.
/// reach - all reachable nodes
/// outReach - subset of reach that is only reachable from the return node
void argReachableNodes(const llvm::Function &fn, seadsa::Graph &G,
                       NodeSet &reach, NodeSet &outReach);

struct isInteger {
  unsigned m_bitwidth; // bits
  isInteger() : m_bitwidth(0) {}
  bool operator()(const llvm::Type *t);
};

struct isBool {
  bool operator()(const llvm::Type *t) const;
};

struct isIntegerOrBool {
  bool operator()(const llvm::Type *t) const;
};

struct isPointer {
  bool operator()(const llvm::Type *t) const;
};

} // end namespace seadsa_heap_abs_impl
} // end namespace clam
