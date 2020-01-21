#pragma once
#include "clam/config.h"

#include <set>

namespace llvm {
class Type;
class Function;
} // namespace llvm

namespace sea_dsa {
class Graph;
class Node;
} // namespace sea_dsa

namespace clam {
namespace seadsa_heap_abs_impl {

struct NodeOrdering {
  bool operator()(const sea_dsa::Node *n1, const sea_dsa::Node *n2) const;
};

using NodeSet = std::set<const sea_dsa::Node *, NodeOrdering>;

void set_difference(NodeSet &s1, NodeSet &s2);

void set_union(NodeSet &s1, NodeSet &s2);

void markReachableNodes(const sea_dsa::Node *n, NodeSet &set);

void reachableNodes(const llvm::Function &fn, sea_dsa::Graph &g,
                    NodeSet &inputReach, NodeSet &retReach);

/// Computes Node reachable from the call arguments in the graph.
/// reach - all reachable nodes
/// outReach - subset of reach that is only reachable from the return node
void argReachableNodes(const llvm::Function &fn, sea_dsa::Graph &G,
                       NodeSet &reach, NodeSet &outReach);

struct isInteger : std::unary_function<const llvm::Type *, bool> {
  unsigned m_bitwidth;
  isInteger() : m_bitwidth(0) {}
  bool operator()(const llvm::Type *t);
};

struct isBool : std::unary_function<const llvm::Type *, bool> {
  bool operator()(const llvm::Type *t) const;
};

struct isIntegerOrBool : std::unary_function<const llvm::Type *, bool> {
  bool operator()(const llvm::Type *t) const;
};

} // end namespace seadsa_heap_abs_impl
} // end namespace clam
