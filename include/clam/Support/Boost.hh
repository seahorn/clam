#pragma once

#ifdef BOOST_NO_EXCEPTIONS
#include "llvm/Support/ErrorHandling.h"
namespace boost {
template <class E> void throw_exception(E const &e) {
  llvm::report_fatal_error("boost threw an exception", false);
}
} // namespace boost
#endif
