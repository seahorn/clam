#pragma once

/** Define whether llvm-dsa library is available */
#cmakedefine HAVE_DSA ${HAVE_DSA}

/** Define whether llvm-seahorn is available */
#cmakedefine HAVE_LLVM_SEAHORN ${HAVE_LLVM_SEAHORN}

/** Include all abstract domains */
#cmakedefine HAVE_ALL_DOMAINS ${HAVE_ALL_DOMAINS}

/** Include inter-procedural analysis */
#cmakedefine HAVE_INTER ${HAVE_INTER}

/** Whether to use big numbers for representing weights in DBM-based domains **/
#cmakedefine USE_DBM_BIGNUM ${USE_DBM_BIGNUM}

/** Whether to use safe or unsafe for representing weights
 ** in DBM-based domains. Only if USE_DBM_BIGNUM is disabled.  **/
#cmakedefine USE_DBM_SAFEINT ${USE_DBM_SAFEINT}

/** Use new top-down inter-procedural analysis.  Otherwise, it will
    use the old bottom-up analysis */
#cmakedefine TOP_DOWN_INTER_ANALYSIS ${TOP_DOWN_INTER_ANALYSIS}

/** Choose the array adaptive domain (default array smashing) */
#cmakedefine HAVE_ARRAY_ADAPT ${HAVE_ARRAY_ADAPT}
