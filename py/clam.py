#!/usr/bin/env python3

"""
Entry point to Clam Abstract Interpreter
"""

import argparse as a
import atexit
#from datetime import datetime
import errno
import io
import os
import os.path
import platform
import resource
import shutil
import subprocess as sub
import signal
import stats
import sys
import tempfile
import threading


root = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
verbose = True
running_process = None

####### SPECIAL ERROR CODES USEFUL FOR DEBUGGING ############
# Exit codes are between 0 and 255.
# Do not use 1, 2, 126, 127, 128 and negative integers.
## special error codes for the frontend (clang + opt + pp)
FRONTEND_TIMEOUT=20
FRONTEND_MEMORY_OUT=21
#### specific errors for each frontend component
CLANG_ERROR = 22
OPT_ERROR = 23
PP_ERROR = 24
### special error codes for crab
CRAB_ERROR = 25    ## errors caught by crab
CRAB_TIMEOUT = 26
CRAB_MEMORY_OUT = 27
CRAB_SEGFAULT = 28 ## unexpected segfaults
#############################################################

llvm_version = "14.0"

def isexec(fpath):
    if fpath is None:
        return False
    return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

def which(program):
    if isinstance(program, str):
        choices = [program]
    else:
        choices = program

    for p in choices:
        fpath, _ = os.path.split(p)
        if fpath:
            if isexec(p):
                return p
        else:
            for path in os.environ["PATH"].split(os.pathsep):
                exe_file = os.path.join(path, p)
                if isexec(exe_file):
                    return exe_file
    return None

# Return a tuple (returnvalue:int, timeout:bool, out_of_memory:bool, segfault:bool, unknown:bool)
#   - Only one boolean flag can be enabled at any time.
#   - If all flags are false then returnvalue cannot be None.
def run_command_with_limits(cmd, cpu, mem, out = None):
    timeout = False
    out_of_memory = False
    segfault = False
    unknown_error = False
    returnvalue = 0

    def set_limits():
        if mem > 0:
            mem_bytes = mem * 1024 * 1024
            resource.setrlimit(resource.RLIMIT_AS, [mem_bytes, mem_bytes])
    def kill(proc):
        try:
            proc.terminate()
            proc.kill()
            proc.wait()
            global running_process
            running_process = None
        except OSError:
            pass

    if out is not None:
        p = sub.Popen(cmd, stdout = out, preexec_fn=set_limits)
    else:
        p = sub.Popen(cmd, preexec_fn=set_limits)

    global running_process
    running_process = p
    timer = threading.Timer(cpu, kill, [p])
    if cpu > 0:
        timer.start()

    try:
        (_, status, _) = os.wait4(p.pid, 0)
        signalvalue = status & 0xff
        returnvalue = status >> 8
        if signalvalue != 0:
            returnvalue = None
            if signalvalue > 127:
                segfault = True
            else:
                print("** Killed by signal " + str(signalvalue))
                # 9: 'SIGKILL', 14: 'SIGALRM', 15: 'SIGTERM'
                if signalvalue in (9, 14, 15):
                    ## kill sends SIGTERM by default.
                    ## The timer set above uses kill to stop the process.
                    timeout = True
                else:
                    unknown_error = True
        running_process = None
    except OSError as e:
        returnvalue = None
        print("** OS Error: " + str(e))
        if errno.errorcode[e.errno] == 'ECHILD':
            ## The children has been killed. We assume it has been killed by the timer.
            ## But I think it can be killed by others
            timeout = True
        elif errno.errorcode[e.errno] == 'ENOMEM':
            out_of_memory = True
        else:
            unknown_error = True
    finally:
        ## kill the timer if the process has terminated already
        if timer.is_alive():
            timer.cancel()

    return (returnvalue, timeout, out_of_memory, segfault, unknown_error)

def loadEnv(filename):
    if not os.path.isfile(filename): return

    f = open(filename)
    for line in f:
        sl = line.split('=', 1)
        # skip lines without equality
        if len(sl) != 2:
            continue
        (key, val) = sl
        os.environ [key] = os.path.expandvars(val.rstrip())


def parseArgs(argv):
    def str2bool(v):
        if isinstance(v, bool):
            return v
        if v.lower() in ('yes', 'true', 't', 'y', '1'):
            return True
        if v.lower() in ('no', 'false', 'f', 'n', '0'):
            return False
        raise a.ArgumentTypeError('Boolean value expected.')

    def add_bool_argument(parser, name, default,
                          help=None, dest=None, **kwargs):
        """
        Add boolean option that can be turned on and off
        """
        dest_name = dest if dest else name
        mutex_group = parser.add_mutually_exclusive_group(required=False)
        mutex_group.add_argument('--' + name, dest=dest_name, type=str2bool,
                                 nargs='?', const=True, help=help,
                                 metavar='BOOL', **kwargs)
        mutex_group.add_argument('--no-' + name, dest=dest_name,
                                 type=lambda v: not(str2bool(v)),
                                 nargs='?', const=False,
                                 help=a.SUPPRESS, **kwargs)
        default_value = {dest_name : default}
        parser.set_defaults(**default_value)

    p = a.ArgumentParser(description='Abstract Interpretation-based Analyzer for LLVM bitecode',
                         formatter_class=a.RawTextHelpFormatter)
    p.add_argument ('-oll', '--oll', dest='asm_out_name', metavar='FILE',
                    help='Output analyzed bitecode')
    p.add_argument ('-ocrab', '--ocrab', dest='crabir_out_name', metavar='FILE',
                    help='Output analyzed CrabIR with (optionally) annotations')
    p.add_argument ('-ojson', '--ojson', dest='json_out_name', metavar='FILE',
                    help='Output invariants to JSON format')
    p.add_argument('--log', dest='log', default=None,
                    metavar='STR', help='Log level for clam')
    p.add_argument('-o', dest='out_name', metavar='FILE',
                    help='Output file name')
    p.add_argument("--save-temps", dest="save_temps",
                    help="Do not delete temporary files",
                    action="store_true",
                    default=False)
    p.add_argument("--temp-dir", dest="temp_dir", metavar='DIR',
                    help="Temporary directory",
                    default=None)
    p.add_argument('-g', default=False, action='store_true', dest='debug_info',
                    help='Compile with debug information')
    p.add_argument('-m', type=int, dest='machine',
                    help='Machine architecture MACHINE:[32,64]', default=32)
    p.add_argument ('-I', default=None, dest='include_dir', help='Include')
    p.add_argument("--no-preprocess", dest="preprocess",
                    help='Skip compilation and preprocessing', action='store_false',
                    default=True)
    p.add_argument("--only-preprocess", dest="only_preprocess",
                    help='Run only the preprocessor', action='store_true',
                    default=False)
    p.add_argument('-O', type=int, dest='L', metavar='INT',
                    help='Optimization level L:[0,1,2,3]', default=0)
    p.add_argument('--cpu', type=int, dest='cpu', metavar='SEC',
                    help='CPU time limit (seconds)', default=-1)
    p.add_argument('--mem', type=int, dest='mem', metavar='MB',
                    help='MEM limit (MB)', default=-1)
    p.add_argument('--llvm-version',
                    help='Print llvm version', dest='llvm_version',
                    default=False, action='store_true')
    p.add_argument('--clang-version',
                    help='Print clang version', dest='clang_version',
                    default=False, action='store_true')
    p.add_argument('--llvm-dot-cfg',
                    help='Print LLVM CFG of function to dot file',
                    dest='dot_cfg', default=False, action='store_true')
    ##---------------------------------------------------------------------##
    ##   Optimizations/transformations that take place at LLVM IR level
    ##---------------------------------------------------------------------##    
    p.add_argument('--llvm-inline-threshold', dest='inline_threshold',
                    type=int, metavar='NUM',
                    help='Inline threshold (default = 255)')
    p.add_argument('--llvm-pp-loops',
                    help='Optimizing loops',
                    dest='pp_loops', default=False, action='store_true')
    p.add_argument('--llvm-peel-loops', dest='peel_loops',
                    type=int, metavar='NUM', default=0,
                    help='Number of iterations to peel (default = 0)')
    # p.add_argument('--llvm-unroll-threshold', type=int,
    #                 help='Unrolling threshold (default = 150)',
    #                 dest='unroll_threshold',
    #                 default=150, metavar='NUM')
    p.add_argument('--inline', dest='inline', help='Inline all functions',
                    default=False, action='store_true')
    p.add_argument('--turn-undef-nondet',
                    help='Turn undefined behaviour into non-determinism',
                    dest='undef_nondet', default=False, action='store_true')
    add_bool_argument(p, 'promote-malloc',
                    help='Promote top-level malloc to alloca',
                      dest='promote_malloc', default=True)
    p.add_argument('--lower-select',
                    help='Lower select instructions',
                    dest='lower_select', default=False, action='store_true')
    p.add_argument('--lower-unsigned-icmp',
                    help='Lower ULT and ULE instructions',
                    dest='lower_unsigned_icmp', default=False, action='store_true')
    p.add_argument('--disable-scalarize',
                    help='Disable lowering of vector operations into scalar ones',
                    dest='disable_scalarize', default=False, action='store_true')
    p.add_argument('--disable-lower-constant-expr',
                    help='Disable lowering of constant expressions to instructions',
                    dest='disable_lower_cst_expr', default=False, action='store_true')
    p.add_argument('--disable-lower-switch',
                    help='Disable lowering of switch instructions',
                    dest='disable_lower_switch', default=False, action='store_true')
    p.add_argument('--devirt-functions',
                    help="Resolve indirect calls (needed for soundness):\n"
                    "- none : do not resolve indirect calls (default)\n"
                    "- types: select all functions with same type signature\n"
                    "- sea-dsa: use sea-dsa analysis to select the callees\n",
                    dest='devirt',
                    choices=['none','types','sea-dsa'],
                    default='none')
    p.add_argument ('--entry', dest='entry', help='Make entry point if main does not exist',
                    default=None, metavar='str')
    p.add_argument ('--externalize-functions',
                    help='Externalize these functions',
                    dest='extern_funcs', type=str, metavar='str,...')    
    p.add_argument('--externalize-addr-taken-functions',
                    help='Externalize uses of address-taken functions (potentially unsound)',
                    dest='extern_addr_taken_funcs', default=False,
                    action='store_true')
    p.add_argument('--print-after-all',
                    help='Print IR after each pass (for debugging)',
                    dest='print_after_all', default=False,
                    action='store_true')
    p.add_argument('--debug-pass',
                    help='Print all LLVM passes executed (--debug-pass=Structure)',
                    dest='debug_pass', default=False,
                    action='store_true')
    p.add_argument('file', metavar='FILE', help='Input file')
    ##---------------------------------------------------------------------##    
    ### BEGIN CRAB
    ## Here options that are passed to Crab or transformations that
    ## take place at CrabIR level
    ##---------------------------------------------------------------------##
    p.add_argument('--crab-verbose', type=int,
                    help='Enable verbose messages',
                    dest='crab_verbose',
                    default=0, metavar='UINT')
    p.add_argument("--crab-only-cfg", dest="crab_only_cfg",
                    help='Build only the Crab CFG', action='store_true',
                    default=False)
    p.add_argument('--crab-cfg-simplify',
                    help='Perform some crab CFG transformations',
                    dest='crab_cfg_simplify', default=False, action='store_true')
    p.add_argument('--crab-dom',
                    help="Choose abstract domain:\n"
                          "- int: intervals\n"
                          "- sign-const: reduced product of sign and constant domains\n"
                          "- ric: reduced product of intervals and congruences\n"
                          "- term-int: int with uninterpreted functions\n"
                          "- dis-int: disjunctive intervals based on Clousot's DisInt domain\n"
                          "- term-dis-int: dis-int with uninterpreted functions\n"
                          "- boxes: disjunctive intervals based on LDDs\n"
                          "- zones: zones domain using DBMs in Split Normal Form\n"
                          "- non-unit-zones: zones domain with a small set of non-unit coefficients\n"                   
                          "- soct: octagons domain using DBMs in Split Normal Form\n"
                          "- oct: octagons domain from Apron or Elina\n"
                          "- pk: polyhedra domain from Apron or Elina\n"
                          "- pk-pplite: polyhedra domain from PPLite\n"                   
                          "- rtz: reduced product of term-dis-int with zones\n"
                          "- w-int: wrapped intervals\n",
                    choices=['int', 'sign-const', 'ric', 'term-int',
                             'dis-int', 'term-dis-int', 'boxes',
                             'zones', 'non-unit-zones', 'soct', 'oct', 'pk', 'pk-pplite', 'rtz',
                             'w-int'],
                    dest='crab_dom', default='zones')
    p.add_argument('--crab-dom-params', dest='crab_dom_params', default=None,
                   help="Set abstract domain options STR=\"param1=val1:param2=val2:...\"",
                   metavar='STR')
    p.add_argument('--crab-widening-delay',
                    type=int, dest='widening_delay',
                    help='Max number of iterations until performing widening', default=1)
    p.add_argument('--crab-widening-jump-set',
                    type=int, dest='widening_jump_set',
                    help='Size of the jump set used in widening', default=0)
    p.add_argument('--crab-narrowing-iterations',
                    type=int, dest='narrowing_iterations',
                    help='Max number of narrowing iterations', default=3)
    p.add_argument('--crab-relational-threshold',
                    type=int, dest='num_threshold',
                    help='Max number of live vars per block before switching to a non-relational domain',
                    default=10000)
    p.add_argument('--crab-track',
                   help='Track integers (num), num + singleton memory objects (sing-mem), and num + all memory objects (mem)',
                   choices=['num', 'sing-mem', 'mem'], dest='track', default='num')
    p.add_argument('--crab-heap-analysis',
                   help="Heap analysis used for memory disambiguation (if --crab-track != num):\n"
                   "- ci-sea-dsa: context-insensitive sea-dsa\n"
                   "- cs-sea-dsa: context-sensitive sea-dsa\n"
                   "- ci-sea-dsa-types: context-insensitive sea-dsa with types (default)\n"
                   "- cs-sea-dsa-types: context-sensitive sea-dsa with types\n",
                   choices=['none', 'ci-sea-dsa', 'cs-sea-dsa', 'ci-sea-dsa-types', 'cs-sea-dsa-types'],
                   dest='crab_heap_analysis',
                   default='ci-sea-dsa-types')
    p.add_argument('--crab-heap-dot',
                    help='Print seadsa memory graphs of each function to dot file',
                    dest='crab_heap_dot', default=False, action='store_true')
    p.add_argument('--crab-singleton-aliases',
                    help='Translate singleton alias sets (mostly globals) as scalar values',
                    dest='crab_singleton_aliases', default=False, action='store_true')
    p.add_argument('--crab-inter',
                    help='Run summary-based, inter-procedural analysis',
                    dest='crab_inter', default=False, action='store_true')
    p.add_argument('--crab-inter-max-summaries',
                    type=int, dest='inter_max_summaries',
                    help='Max number of summaries per function',
                    default=1000000)
    add_bool_argument(p, 'crab-inter-recursive-functions', default=False,
                      help='Precise analysis of recursive functions (more expensive). False by default',
                      dest='crab_inter_recursive')
    add_bool_argument(p, 'crab-inter-exact-summary-reuse', default=True,
                      help='Reuse summaries without losing precision (more expensive). True by default',
                      dest='crab_inter_exact_summary_reuse')
    add_bool_argument(p, 'crab-inter-entry-main', default=False,
                      help='Start analysis only from main (not applicable to libraries). False by default',
                      dest='crab_inter_entry_main')
    p.add_argument('--crab-backward',
                    help='Run iterative forward/backward analysis for proving assertions (only intra version available and very experimental)',
                    dest='crab_backward', default=False, action='store_true')
    # WARNING: --crab-live may lose precision.
    # If x=z in bb1 and y=z in bb2 and z is dead after bb1 and bb2 then
    # the equality x=y is lost.
    p.add_argument('--crab-live',
                    help='Delete dead symbols: may lose precision with relational domains.',
                    dest='crab_live', default=False, action='store_true')
    add_bool_argument(p, 'crab-lower-unsigned-icmp', default=False,
                    help='Replace unsigned comparison with signed comparisons in CrabIR',
                    dest='crab_lower_unsigned_icmp')
    add_bool_argument(p, 'crab-lower-with-overflow-intrinsics', default=False,
                    help='Replace llvm.OP.with.overflow.* in CrabIR with OP assuming no overflow occurs.\n'
                         'This option should be only used if the arithmetic operations are known to not overflow',
                    dest='crab_lower_with_overflow_intrinsics')
    p.add_argument('--crab-opt',
                    help='Optimize LLVM bitcode using invariants',
                    choices=['none',
                             'dce',
                             'add-invariants',
                             'replace-with-constants',
                             'all'],
                    dest='crab_optimizer', default='none')
    p.add_argument('--crab-opt-invariants-loc',
                    help='Specify the location where invariants are added (only if crab-opt=add-invariants)',
                    choices=['none',
                             'block-entry',
                             'loop-header',
                             'after-load',
                             'all'],
                    dest='crab_optimizer_inv_loc', default='none')
    add_bool_argument(p, 'crab-preserve-invariants',
                      help='Preserve invariants for queries after analysis has finished',
                      dest='store_invariants', default=True)
    p.add_argument('--crab-promote-assume',
                    help='Promote verifier.assume calls to llvm.assume intrinsics',
                    dest='crab_promote_assume', default=False, action='store_true')
    p.add_argument('--crab-check',
                   help="Check properties (default none):\n"
                   "- assert: user-defined assertions via __CRAB_assert\n"
                   "- null-legacy: insert __CRAB_assert calls in LLVM IR for null dereference errors\n"                   
                   "- null: insert CrabIR assertions for null dereference errors\n"
                   "- uaf-legacy: insert __CRAB_assert calls in LLVM IR for use-after-free errors\n"                   
                   "- uaf: insert CrabIR assertions for use-after-free errors\n"
                   "- bounds: insert CrabIR assertions and ghost code for buffer overflow errors\n"
                   "  (instrumentation is not complete: lack of modeling pointers stored in memory)\n"
                   "- is-deref: insert CrabIR assertions for proving that calls to LLVM function sea.is_dereferenceable cannot fail\n"
                   "  (this option is used mostly by SeaHorn which adds the calls to sea.is_dereferenceable)\n",
                   choices=['none','assert','null-legacy','null','uaf-legacy','uaf','bounds','is-deref'],
                   dest='crab_check', default='none')
    add_bool_argument(p, 'crab-check-only-typed', default=False,
                      help='Add checks only on typed regions (only for null and uaf). False by default',
                      dest='crab_check_only_typed')
    add_bool_argument(p, 'crab-check-only-noncyclic', default=False,
                      help='Add checks only on noncyclic regions (only for null and uaf). False by default',
                      dest='crab_check_only_noncyclic')
    p.add_argument('--crab-check-verbose', metavar='INT',
                    help='Print verbose information about checks\n' +
                         '>=1: only error checks\n' +
                         '>=2: error and warning checks\n' +
                         '>=3: error, warning, and safe checks',
                    dest='check_verbose', type=int, default=0)
    add_bool_argument(p, 'crab-print-cfg',
                    help='Display Crab CFG',
                    dest='print_cfg', default=False)
    add_bool_argument(p, 'crab-dot-cfg', default=False,
                      help='Print Crab CFG of function to dot file',
                      dest='crab_dot_cfg')
    add_bool_argument(p, 'crab-print-invariants',
                    help='Print invariants',
                    dest='crab_print_invariants', default=True)
    p.add_argument('--crab-print-invariants-kind',
                    help='Specify which invariants should be printed (only if crab-print-invariants=true)',
                    choices=['blocks', 'loops'],
                    dest='crab_print_invariants_kind', default='blocks')
    add_bool_argument(p, 'crab-print-unjustified-assumptions',
                    help='Print unjustified assumptions done by the analyzer (very very experimental, only for integer overflows and for intra-procedural analysis)',
                    dest='print_assumptions', default=False)
    add_bool_argument(p, 'crab-print-voi',
                    help='Print variables-of-influence of assertions (only if --crab-inter)',
                    dest='print_voi', default=False)
    add_bool_argument(p, 'crab-print-ghost-variables', default=False,
                      help='Print all ghost variables in the invariants. False by default.',
                      dest='crab_keep_shadows')
    p.add_argument('--crab-stats',
                    help='Display crab statistics',
                    dest='print_stats', default=False, action='store_true')
    p.add_argument('--crab-disable-warnings',
                    help='Disable clam and crab warnings',
                    dest='crab_disable_warnings', default=False, action='store_true')
    p.add_argument('--crab-sanity-checks',
                    help='Enable clam and crab sanity checks',
                    dest='crab_sanity_checks', default=False, action='store_true')
    ######################################################################
    # Hidden options
    ######################################################################
    ## These three might be unsound if enabled
    p.add_argument('--crab-dsa-disambiguate-unknown',
                    help=a.SUPPRESS,
                    dest='crab_dsa_unknown', default=False, action='store_true')
    p.add_argument('--crab-dsa-disambiguate-ptr-cast',
                    help=a.SUPPRESS,
                    dest='crab_dsa_ptr_cast', default=False, action='store_true')
    p.add_argument('--crab-dsa-disambiguate-external',
                    help=a.SUPPRESS,
                    dest='crab_dsa_external', default=False, action='store_true')
    # Choose between own crab way of naming values and instnamer
    add_bool_argument(p, 'crab-name-values', default=True,
                      help=a.SUPPRESS, dest='crab_name_values')
    add_bool_argument(p, 'crab-enable-bignums', default=False,
                      help=a.SUPPRESS, dest='crab_enable_bignums')
    #### END CRAB

    args = p.parse_args(argv)

    if args.L < 0 or args.L > 3:
        p.error("Unknown option: -O%s" % args.L)

    if args.machine != 32 and args.machine != 64:
        p.error("Unknown option -m%s" % args.machine)

    return args

def createWorkDir(dname = None, save = False):
    if dname is None:
        workdir = tempfile.mkdtemp(prefix='clam-')
    else:
        workdir = dname

    if False: #verbose:
        print("Working directory {0}".format(workdir))

    if not save:
        atexit.register(shutil.rmtree, path=workdir)
    return workdir

def getClam():
    clam_cmd = None
    if 'CLAM' in os.environ:
        clam_cmd = os.environ ['CLAM']
    if not isexec(clam_cmd):
        clam_cmd = os.path.join(root, "bin/clam")
    if not isexec(clam_cmd):
        clam_cmd = which('clam')
    if not isexec(clam_cmd):
        raise IOError("Cannot find clam")
    return clam_cmd

def getClamPP():
    crabpp_cmd = None
    if 'CLAMPP' in os.environ:
        crabpp_cmd = os.environ ['CLAMPP']
    if not isexec(crabpp_cmd):
        crabpp_cmd = os.path.join(root, "bin/clam-pp")
    if not isexec(crabpp_cmd): crabpp_cmd = which('clam-pp')
    if not isexec(crabpp_cmd):
        raise IOError("Cannot find clam pre-processor")
    return crabpp_cmd

def getClangVersion(clang_cmd):
    p = sub.Popen([clang_cmd,'--version'], stdout = sub.PIPE)
    out, _ = p.communicate()
    clang_version = "not-found"
    found = False # true if string 'version' is found
    tokens = out.split()
    for t in tokens:
        if found is True:
            clang_version = t
            break
        if t == 'version':
            found = True
    return clang_version

def getClang(is_plus_plus):
    cmd_name = None
    if is_plus_plus:
        cmd_name = which (['clang++-mp-' + llvm_version, 'clang++-' + llvm_version, 'clang++'])
    else:
        cmd_name = which (['clang-mp-' + llvm_version, 'clang-' + llvm_version, 'clang'])
    if cmd_name is None:
        raise IOError('clang was not found')
    return cmd_name

# return a pair: the first element is the command and the second is a
# bool that it is true if seaopt has been found.
def getOptLlvm ():
    cmd_name = which (['seaopt'])
    if cmd_name is not None:
        return (cmd_name, True)

    cmd_name = which (['opt-mp-' + llvm_version, 'opt-' + llvm_version, 'opt'])
    if cmd_name is None:
        raise IOError ('neither seaopt nor opt where found')
    return (cmd_name, False)

### Passes
def defBCName(name, wd=None):
    base = os.path.basename(name)
    if wd is None:
        wd = os.path.dirname (name)
    fname = os.path.splitext(base)[0] + '.bc'
    return os.path.join(wd, fname)
def defPPName(name, wd=None):
    base = os.path.basename(name)
    if wd is None:
        wd = os.path.dirname (name)
    fname = os.path.splitext(base)[0] + '.pp.bc'
    return os.path.join(wd, fname)
def defOptName(name, wd=None):
    base = os.path.basename(name)
    if wd is None:
        wd = os.path.dirname (name)
    fname = os.path.splitext(base)[0] + '.o.bc'
    return os.path.join(wd, fname)

def defOutPPName(name, wd=None):
    base = os.path.basename(name)
    if wd is None:
        wd = os.path.dirname (name)
    fname = os.path.splitext(base)[0] + '.ll'
    return os.path.join(wd, fname)


def _bc_or_ll_file (name):
    ext = os.path.splitext (name)[1]
    return ext == '.bc' or ext == '.ll'

def _plus_plus_file(name):
    ext = os.path.splitext(name)[1]
    return ext in ('.cpp', '.cc')

# Run Clang
def clang(in_name, out_name, args, arch=32, extra_args=[]):

    if _bc_or_ll_file(in_name):
        if verbose:
            print('--- Clang skipped: input file is already bitecode')
        shutil.copy2(in_name, out_name)
        return

    if out_name in ('', None):
        out_name = defBCName(in_name)

    clang_cmd = getClang(_plus_plus_file(in_name))
    clang_version = getClangVersion(clang_cmd)
    if clang_version != "not-found":
        if not clang_version.startswith(llvm_version):
            print("WARNING clam.py: clang version " + clang_version +  \
                  " different from " + llvm_version)

    clang_args = [clang_cmd, '-emit-llvm', '-o', out_name, '-c', in_name ]

    # New for clang >= 5.0: to avoid add optnone if -O0
    # Otherwise, seaopt cannot optimize.
    clang_args.append('-Xclang')
    clang_args.append('-disable-O0-optnone')
    # To allow syntax such as __declspec(noalias) in C programs
    clang_args.append('-fdeclspec')
    # best-effort to keep C names 
    clang_args.append ('-fno-discard-value-names')    
    clang_args.extend (extra_args)
    clang_args.append ('-m{0}'.format (arch))
    # To avoid the error:
    # "unknown target triple 'unknown-apple-macosx10.17.0', please use -triple or -arch"
    if arch == 64 and platform.processor() == 'arm' and platform.system() == 'Darwin':
        # Maybe we should add this line also for other systems.
        clang_args.extend(['-arch', 'arm64'])

    if args.include_dir is not None:
        if ':' in args.include_dir:
            idirs = ["-I{}".format(x.strip())  \
                for x in args.include_dir.split(":") if x.strip() != '']
            clang_args.extend(idirs)
        else:
            clang_args.append ('-I' + args.include_dir)

    include_dir = os.path.dirname (sys.argv[0])
    include_dir = os.path.dirname (include_dir)
    include_dir = os.path.join (include_dir, 'include')
    clang_args.append ('-I' + include_dir)


    # Disable always vectorization
    if not args.disable_scalarize:
        clang_args.append('-fno-vectorize') ## disable loop vectorization
        clang_args.append('-fno-slp-vectorize') ## disable store/load vectorization

    # Since Mojave, OSX does not longer exposes libc and libstd headers by default.
    if platform.system() == 'Darwin':
        osx_sdk_path = sub.run(["xcrun","--show-sdk-path"], text=True, stdout=sub.PIPE)
        if osx_sdk_path.returncode == 0:
            clang_args.append('--sysroot=' + osx_sdk_path.stdout.strip())
    
    if verbose:
        print('Clang command: ' + ' '.join(clang_args))
    returnvalue, timeout, out_of_mem, segfault, unknown = \
        run_command_with_limits(clang_args, -1, -1)
    if timeout:
        sys.exit(FRONTEND_TIMEOUT)
    elif out_of_mem:
        sys.exit(FRONTEND_MEMORY_OUT)
    elif segfault or unknown or returnvalue != 0:
        sys.exit(CLANG_ERROR)

# Run llvm optimizer
def optLlvm(in_name, out_name, args, extra_args=[], cpu = -1, mem = -1):
    if out_name in ('', None):
        out_name = defOptName(in_name)
    opt_cmd, is_seaopt = getOptLlvm()

    opt_args = [opt_cmd, '-f']
    if out_name is not None: opt_args.extend(['-o', out_name])
    opt_args.append('-O{0}'.format(args.L))

    # disable sinking instructions to end of basic block
    # this might create unwanted aliasing scenarios
    # for now, there is no option to undo this switch
    opt_args.append('--simplifycfg-sink-common=false')

    ## Unavailable after porting to LLVM12    
    ### REVISIT: these flags are gone. See here some discussion
    ### https://reviews.llvm.org/D77989.
    ### It seems they were not having much effect anyway.
    # opt_args.append('--disable-loop-vectorization')
    # opt_args.append('--disable-slp-vectorization')


    # LLVM 12 onwards we need to disable slp vectorization (vec instr not handled by sea-dsa and opsem)
    # vectorization for loops is split into multiple options so not adding any here.
    # See https://sourcegraph.com/github.com/llvm/llvm-project@release/12.x/-/blob/llvm/lib/Transforms/Vectorize/LoopVectorize.cpp
    opt_args.extend (['--vectorize-slp=false'])    
    
    ## Unavailable after porting to LLVM10
    # if is_seaopt:
    #     # disable always loop rotation. Loop rotation converts to loops
    #     # that are much harder to reason about them using crab due to
    #     # several reasons:
    #     #
    #     # 1. Complex loops that break widening heuristics
    #     # 2. Rewrite loop exits by adding often disequalities
    #     # 3. Introduce new *unsigned* loop variables.
    #     opt_args.append('--disable-loop-rotate')

    # These two should be optional
    #opt_args.append('--enable-indvar=true')
    #opt_args.append('--enable-loop-idiom=true')

    ## Unavailable after porting to LLVM10
    # if is_seaopt:
    #     if args.undef_nondet:
    #         opt_args.append('--enable-nondet-init=true')
    #     else:
    #         opt_args.append('--enable-nondet-init=false')

    if args.inline_threshold is not None:
        opt_args.append('--inline-threshold={t}'.format
                        (t=args.inline_threshold))
    # if args.unroll_threshold is not None:
    #     opt_args.append('--unroll-threshold={t}'.format
    #                     (t=args.unroll_threshold))
    if args.print_after_all: opt_args.append('--print-after-all')
    if args.debug_pass: opt_args.append('--debug-pass=Structure')
    opt_args.extend(extra_args)
    opt_args.append(in_name)

    if verbose:
        print('seaopt command: ' + ' '.join(opt_args))
    returnvalue, timeout, out_of_mem, _, unknown = \
        run_command_with_limits(opt_args, cpu, mem)
    if timeout:
        sys.exit(FRONTEND_TIMEOUT)
    elif out_of_mem:
        sys.exit(FRONTEND_MEMORY_OUT)
    elif unknown or returnvalue != 0:
        sys.exit(OPT_ERROR)

# Generate dot files for each LLVM function.
def dot(in_name, view_dot = False, cpu = -1, mem = -1):
    fnull = open(os.devnull, 'w')
    args = [getOptLlvm(), in_name, '-dot-cfg']
    if view_dot: args.append('-view-cfg')
    if verbose:
        print(' '.join(args))
    ## We don't bother here analyzing the exit code
    run_command_with_limits(args, cpu, mem, fnull)

# Run crabpp
def crabpp(in_name, out_name, args, extra_args=[], cpu = -1, mem = -1):
    if out_name in ('', None):
        out_name = defPPName(in_name)

    crabpp_args = [getClamPP(), '-o', out_name, in_name ]

    # disable sinking instructions to end of basic block
    # this might create unwanted aliasing scenarios
    # for now, there is no option to undo this switch
    crabpp_args.append('--simplifycfg-sink-common=false')

    if args.entry is not None:
        crabpp_args.append('--entry-point={0}'.format(args.entry))
    
    if args.promote_malloc:
        crabpp_args.append('--clam-promote-malloc=true')
    else:
        crabpp_args.append('--clam-promote-malloc=false')

    if args.inline:
        crabpp_args.append('--clam-inline-all')
    if args.pp_loops:
        crabpp_args.append('--clam-pp-loops')
    if args.peel_loops > 0:
        crabpp_args.append('--clam-peel-loops={0}'.format(args.peel_loops))
    if args.undef_nondet:
        crabpp_args.append('--clam-turn-undef-nondet')

    if args.disable_scalarize:
        crabpp_args.append('--clam-scalarize=false')
    else:
        # Force to scalarize everthing
        crabpp_args.append('--scalarize-load-store=true')
    if args.disable_lower_cst_expr:
        crabpp_args.append('--clam-lower-constant-expr=false')
    if args.disable_lower_switch:
        crabpp_args.append('--clam-lower-switch=false')

    # Postponed until clam is run, otherwise it can be undone by the optLlvm
    # if args.lower_unsigned_icmp:
    #     crabpp_args.append( '--clam-lower-unsigned-icmp')
    if args.devirt != 'none':
        crabpp_args.append('--clam-devirt')
        if args.devirt == 'types':
            crabpp_args.append('--devirt-resolver=types')
        elif args.devirt == 'sea-dsa':
            crabpp_args.append('--devirt-resolver=sea-dsa')
            crabpp_args.append('--sea-dsa-type-aware=true')
        elif args.devirt == 'dsa':
            crabpp_args.append('--devirt-resolver=dsa')
            
    if args.extern_funcs:
        for f in args.extern_funcs.split(','):
            crabpp_args.append('--clam-externalize-function={0}'.format(f))
    if args.extern_addr_taken_funcs:
        crabpp_args.append('--clam-externalize-addr-taken-funcs')
        
    if args.print_after_all: crabpp_args.append('--print-after-all')
    if args.debug_pass: crabpp_args.append('--debug-pass=Structure')

    crabpp_args.extend(extra_args)
    if verbose:
        print('clam-pp command: ' + ' '.join(crabpp_args))
    returnvalue, timeout, out_of_mem, segfault, unknown = \
        run_command_with_limits(crabpp_args, cpu, mem)
    if timeout:
        sys.exit(FRONTEND_TIMEOUT)
    elif out_of_mem:
        sys.exit(FRONTEND_MEMORY_OUT)
    elif segfault or unknown or returnvalue != 0:
        sys.exit(PP_ERROR)

# Run clam
def clam(in_name, out_name, args, extra_opts, cpu = -1, mem = -1):
    clam_args = [ getClam(), in_name, '-oll', out_name]
    clam_args = clam_args + extra_opts

    if args.log is not None:
        for l in args.log.split(':'):
            clam_args.extend(['-crab-log', l])

    if args.crab_dom_params is not None:
        for l in args.crab_dom_params.split(':'):
            clam_args.extend(['-crab-dom-param', l])
            
        
    # disable sinking instructions to end of basic block
    # this might create unwanted aliasing scenarios
    # for now, there is no option to undo this switch
    clam_args.append('--simplifycfg-sink-common=false')

    if args.only_preprocess:
        clam_args.append('-no-crab')
    if args.crab_verbose:
        clam_args.append('--crab-verbose={0}'.format(args.crab_verbose))
    if args.crab_only_cfg:
        clam_args.append('--crab-only-cfg')
    if args.dot_cfg:
        clam_args.append('--clam-llvm-cfg-dot')

        
    ## This option already run in crabpp
    if args.undef_nondet: clam_args.append( '--clam-turn-undef-nondet')

    if args.lower_unsigned_icmp:
        clam_args.append('--clam-lower-unsigned-icmp')
    if args.lower_select:
        clam_args.append('--clam-lower-select')
    if args.disable_lower_cst_expr:
        clam_args.append('--clam-lower-constant-expr=false')
    if args.disable_lower_switch:
        clam_args.append('--clam-lower-switch=false')

    if args.crab_lower_unsigned_icmp:
        clam_args.append('--crab-lower-unsigned-icmp')
    if args.crab_lower_with_overflow_intrinsics:
        clam_args.append('--crab-lower-with-overflow-intrinsics')
    clam_args.append('--crab-dom={0}'.format(args.crab_dom))
    clam_args.append('--crab-widening-delay={0}'.format(args.widening_delay))
    clam_args.append('--crab-widening-jump-set={0}'.format(args.widening_jump_set))
    clam_args.append('--crab-narrowing-iterations={0}'.format(args.narrowing_iterations))
    clam_args.append('--crab-relational-threshold={0}'.format(args.num_threshold))
    clam_args.append('--crab-track={0}'.format(args.track))
    if args.crab_heap_analysis == 'none' or \
       args.crab_heap_analysis == 'ci-sea-dsa' or \
       args.crab_heap_analysis == 'cs-sea-dsa':
        clam_args.append('--crab-heap-analysis={0}'.format(args.crab_heap_analysis))
    elif args.crab_heap_analysis == 'ci-sea-dsa-types':
        clam_args.append('--crab-heap-analysis=ci-sea-dsa')
        clam_args.append('--sea-dsa-type-aware=true')
    elif args.crab_heap_analysis == 'cs-sea-dsa-types':
        clam_args.append('--crab-heap-analysis=cs-sea-dsa')
        clam_args.append('--sea-dsa-type-aware=true')
    # if context-sensitive then we run the analysis on a callgraph
    # where indirect calls have been resolved already by seadsa. This
    # is important among other things to avoid errors with callee/caller
    # simulation relation.
    if args.crab_heap_analysis == 'cs-sea-dsa' or \
       args.crab_heap_analysis == 'cs-sea-dsa-types':
        clam_args.append('--sea-dsa-devirt')

    if args.crab_heap_dot: clam_args.append('--crab-dsa-dot')
        
    if args.crab_singleton_aliases: clam_args.append('--crab-singleton-aliases')

    if args.crab_inter:
        clam_args.append('--crab-inter')
        clam_args.append('--crab-inter-max-summaries={0}'.format(args.inter_max_summaries))
        if args.crab_inter_recursive:
            clam_args.append('--crab-inter-recursive=true')
        else:
            clam_args.append('--crab-inter-recursive=false')
        if args.crab_inter_exact_summary_reuse:
            clam_args.append('--crab-inter-exact-summary-reuse=true')
        else:
            clam_args.append('--crab-inter-exact-summary-reuse=false')
        if args.crab_inter_entry_main:
            clam_args.append('--crab-inter-entry-main=true')
        else:
            clam_args.append('--crab-inter-entry-main=false')

    if args.crab_backward: clam_args.append('--crab-backward')
    if args.crab_live: clam_args.append('--crab-live')

    if args.crab_optimizer != 'none':
        clam_args.append('--crab-opt')
        if args.crab_optimizer == 'dce' or args.crab_optimizer == 'all':
            clam_args.append('--crab-opt-dce')
        if args.crab_optimizer == 'replace-with-constants' or args.crab_optimizer == 'all':
            clam_args.append('--crab-opt-replace-with-constants')
        if args.crab_optimizer == 'add-invariants' or args.crab_optimizer == 'all':
            clam_args.append('--crab-opt-add-invariants={0}'.format(args.crab_optimizer_inv_loc))

    if args.crab_promote_assume:
        clam_args.append('--crab-promote-assume')

    if args.crab_check != 'none':
        clam_args.append('--crab-check')
        if args.crab_check == 'null-legacy':
            clam_args.append('--clam-null-check-legacy')
        elif args.crab_check == 'null':
            clam_args.append('--crab-null-check')            
        elif args.crab_check == 'uaf-legacy':
            clam_args.append('--clam-uaf-check-legacy')
            ## special option for the region domain
            clam_args.extend(['-crab-dom-param', 'region.deallocation=true'])
        elif args.crab_check == 'uaf':
            clam_args.append('--crab-uaf-check')
            ## special option for the region domain
            clam_args.extend(['-crab-dom-param', 'region.deallocation=true'])
        elif args.crab_check == 'bounds':
            clam_args.append('--crab-bounds-check')
        elif args.crab_check == 'is-deref':
            clam_args.append('--crab-is-deref-check')
            ## special option for the region domain
            clam_args.extend(['-crab-dom-param', 'region.is_dereferenceable=true'])            
        if args.crab_check in ['null-legacy', 'uaf-legacy', 'null', 'uaf', 'bounds']:
            if args.crab_check_only_typed:
                clam_args.append('--crab-check-only-typed-regions=true')
            else:
                clam_args.append('--crab-check-only-typed-regions=false')
            if args.crab_check_only_noncyclic:
                clam_args.append('--crab-check-only-noncyclic-regions=true')
            else:
                clam_args.append('--crab-check-only-noncyclic-regions=false')
                         
    if args.check_verbose:
        clam_args.append('--crab-check-verbose={0}'.format(args.check_verbose))
    if args.print_cfg:
        clam_args.append('--crab-print-cfg=true')
    else:
        clam_args.append('--crab-print-cfg=false')
    if args.print_stats: clam_args.append('--crab-stats')
    if args.print_assumptions: clam_args.append('--crab-print-unjustified-assumptions')
    if args.print_voi: clam_args.append('--crab-print-voi')
    if args.crab_disable_warnings:
        clam_args.append('--crab-enable-warnings=false')
    if args.crab_sanity_checks: clam_args.append('--crab-sanity-checks')
    if args.crab_cfg_simplify: clam_args.append('--crab-cfg-simplify')
    if args.crab_print_invariants:
        if args.crab_print_invariants_kind == 'loops':
            clam_args.append('--crab-print-invariants=loops')
        else:
            clam_args.append('--crab-print-invariants=blocks')
    else:
        clam_args.append('--crab-print-invariants=none')
    if args.store_invariants:
        clam_args.append('--crab-store-invariants=true')
    else:
        clam_args.append('--crab-store-invariants=false')
    if args.crab_dot_cfg:
        clam_args.append('--crab-dot-cfg=true')
    else:
        clam_args.append('--crab-dot-cfg=false')
    if args.crabir_out_name is not None:
        clam_args.append('--ocrab={0}'.format(args.crabir_out_name))
    if args.json_out_name is not None:
        clam_args.append('--ojson={0}'.format(args.json_out_name))
        
    # begin hidden options
    if args.crab_dsa_unknown: clam_args.append('--crab-dsa-disambiguate-unknown')
    if args.crab_dsa_ptr_cast: clam_args.append('--crab-dsa-disambiguate-ptr-cast')
    if args.crab_dsa_external: clam_args.append('--crab-dsa-disambiguate-external')
    if args.crab_keep_shadows: clam_args.append('--crab-keep-shadows')
    if args.crab_name_values:
        clam_args.append('--use-crab-name-values=true')
    else:
        clam_args.append('--use-crab-name-values=false')
    if args.crab_enable_bignums:
        clam_args.append('--crab-enable-bignums=true')
    else:
        clam_args.append('--crab-enable-bignums=false')
    # end hidden options

    if verbose:
        print('clam command: ' + ' '.join(clam_args))

    if args.out_name is not None:
        clam_args.append('-o={0}'.format(args.out_name))

    if args.print_after_all:
        clam_args.append('--print-after-all')

    if args.debug_pass:
        clam_args.append('--debug-pass=Structure')

    returnvalue, timeout, out_of_mem, segfault, unknown = \
        run_command_with_limits(clam_args, cpu, mem)
    if timeout:
        sys.exit(CRAB_TIMEOUT)
    elif out_of_mem:
        sys.exit(CRAB_MEMORY_OUT)
    elif segfault:
        sys.exit(CRAB_SEGFAULT)
    elif unknown or returnvalue != 0:
        # crab returns EXIT_FAILURE which in most platforms is 1 but not in all.
        sys.exit(CRAB_ERROR)

def main(argv):
    #def stat(key, val): stats.put(key, val)
    os.setpgrp()
    loadEnv(os.path.join(root, "env.common"))

    ## add directory containing this file to the PATH
    os.environ ['PATH'] =  os.path.dirname(os.path.realpath(__file__)) + \
                           os.pathsep + os.environ['PATH']

    if '--llvm-version' in argv[1:] or '-llvm-version' in argv[1:]:
        print("LLVM version " + llvm_version)
        return 0

    if '--clang-version' in argv[1:] or '-clang-version' in argv[1:]:
        print("Clang version " + getClangVersion(getClang(False)))
        return 0


    print("Platform: {0} {1}".format(platform.system(), platform.release()))
    print("LLVM version: {0}".format(llvm_version))
    #print("Clam started at {0}\n\n".format(datetime.now().strftime("%H:%M:%S")))
          
    args  = parseArgs(argv[1:])
    workdir = createWorkDir(args.temp_dir, args.save_temps)
    in_name = args.file

    if args.preprocess:
        bc_out = defBCName(in_name, workdir)
        if bc_out != in_name:
            extra_args = []
            if args.debug_info: extra_args.append('-g')
            with stats.timer('Clang'):
                clang(in_name, bc_out, args, arch=args.machine, extra_args=extra_args)
            #stat('Progress', 'Clang')
        in_name = bc_out

        pp_out = defPPName(in_name, workdir)
        if pp_out != in_name:
            with stats.timer('ClamPP'):
                crabpp(in_name, pp_out, args=args, cpu=args.cpu, mem=args.mem)
            #stat('Progress', 'Clam preprocessor')
        in_name = pp_out

    if args.L > 0:
        o_out = defOptName(in_name, workdir)
        if o_out != in_name:
            extra_args = []
            with stats.timer('CrabOptLlvm'):
                optLlvm(in_name, o_out, args, extra_args, cpu=args.cpu, mem=args.mem)
            #stat('Progress', 'Llvm optimizer')
        in_name = o_out

    pp_out = defOutPPName(in_name, workdir)
    with stats.timer('Clam'):
        extra_opts = []
        clam(in_name, pp_out, args, extra_opts, cpu=args.cpu, mem=args.mem)

    #stat('Progress', 'Clam')

    if args.asm_out_name is not None and args.asm_out_name != pp_out:
        if False: #verbose:
            print('cp {0} {1}'.format(pp_out, args.asm_out_name))
        shutil.copy2(pp_out, args.asm_out_name)

    #print("\nClam finished at {0}\n".format(datetime.now().strftime("%H:%M:%S")))        
    return 0

def killall():
    global running_process
    if running_process is not None:
        try:
            running_process.terminate()
            running_process.kill()
            running_process.wait()
            running_process = None
        except OSError: pass

if __name__ == '__main__':
    # unbuffered output
    sys.stdout = io.TextIOWrapper(open(sys.stdout.fileno(), 'wb', 0), write_through=True)
    try:
        signal.signal(signal.SIGTERM, lambda x, y: killall())
        sys.exit(main(sys.argv))
    except KeyboardInterrupt: pass
    finally:
        killall()
        stats.brunch_print()
