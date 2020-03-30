#!/usr/bin/env python2

import sys
import os
import os.path
import atexit
import tempfile
import shutil
import subprocess as sub
import threading
import signal
import resource
import stats

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

llvm_version = "5.0"

def isexec(fpath):
    if fpath == None: return False
    return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

def which(program):
    if isinstance(program, str):
        choices = [program]
    else:
        choices = program

    for p in choices:
        fpath, fname = os.path.split(p)
        if fpath:
            if isexec(p): return p
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
        except OSError: pass
        
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
        (pid, status, ru_child) = os.wait4(p.pid, 0)
        signal = status & 0xff
        returnvalue = status >> 8        
        if signal <> 0:
            returnvalue = None
            if signal > 127:
                segfault = True
            else:
                print "** Killed by signal " + str(signal)
                # 9: 'SIGKILL', 14: 'SIGALRM', 15: 'SIGTERM'
                if signal == 9 or signal == 14 or signal == 15:
                    ## kill sends SIGTERM by default.
                    ## The timer set above uses kill to stop the process.
                    timeout = True
                else:
                    unknown_error = True
        running_process = None
    except OSError as e:
        returnvalue = None
        print "** OS Error: " + str(e)
        if os.errno.errorcode[e.errno] == 'ECHILD':
            ## The children has been killed. We assume it has been killed by the timer.
            ## But I think it can be killed by others
            timeout = True
        elif os.errno.errorcode[e.errno] == 'ENOMEM':
            out_of_memory = True
        else:
            unknown_error = True
    finally:
        ## kill the timer if the process has terminated already
        if timer.isAlive(): timer.cancel()
        
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
    import argparse as a
    from argparse import RawTextHelpFormatter

    p = a.ArgumentParser(description='Abstract Interpretation-based Analyzer for LLVM bitecode',
                         formatter_class=RawTextHelpFormatter)
    p.add_argument ('-oll', '--oll', dest='asm_out_name', metavar='FILE',
                    help='Output analyzed bitecode')
    p.add_argument('--log', dest='log', default=None,
                    metavar='STR', help='Log level for clam')
    p.add_argument('--sea-dsa-log', dest='dsa_log', default=None,
                    metavar='STR', help='Log level for sea-dsa')    
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
    p.add_argument('--llvm-view-cfg',
                    help='View LLVM CFG of function',
                    dest='view_cfg', default=False, action='store_true')
    p.add_argument('--llvm-inline-threshold', dest='inline_threshold',
                    type=int, metavar='NUM',
                    help='Inline threshold (default = 255)')
    p.add_argument('--llvm-pp-loops',
                    help='Optimizing loops',
                    dest='pp_loops', default=False, action='store_true')
    p.add_argument('--llvm-unroll-threshold', type=int,
                    help='Unrolling threshold (default = 150)',
                    dest='unroll_threshold',
                    default=150, metavar='NUM')
    p.add_argument('--inline', dest='inline', help='Inline all functions',
                    default=False, action='store_true')
    p.add_argument('--turn-undef-nondet',
                    help='Turn undefined behaviour into non-determinism',
                    dest='undef_nondet', default=False, action='store_true')
    p.add_argument('--lower-select',
                    help='Lower select instructions',
                    dest='lower_select', default=False, action='store_true')
    p.add_argument('--lower-unsigned-icmp',
                    help='Lower ULT and ULE instructions',
                    dest='lower_unsigned_icmp', default=False, action='store_true')    
    p.add_argument('--disable-lower-gv',
                    help='Disable lowering of global variable initializers into main',
                    dest='disable_lower_gv', default=False, action='store_true')
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
                    "- sea-dsa: use sea-dsa analysis to select the callees\n"
                    "- dsa: use llvm-dsa analysis to select the callees (deprecated)\n",
                    dest='devirt',
                    choices=['none','types','sea-dsa','dsa'],
                    default='none')
    p.add_argument('--externalize-addr-taken-functions',
                    help='Externalize uses of address-taken functions (potentially unsound)',
                    dest='enable_ext_funcs', default=False,
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
    ### BEGIN CRAB
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
                          "- ric: reduced product of intervals and congruences\n"
                          "- term-int: int with uninterpreted functions\n"
                          "- dis-int: disjunctive intervals based on Clousot's DisInt domain\n"
                          "- term-dis-int: dis-int with uninterpreted functions\n"
                          "- boxes: disjunctive intervals based on LDDs\n"
                          "- zones: zones domain using sparse DBM in Split Normal Form\n"
                          "- oct: octagons domain\n"
                          "- pk: polyhedra domain\n"
                          "- rtz: reduced product of term-dis-int with zones\n"
                          "- w-int: wrapped intervals\n",
                    choices=['int', 'ric', 'term-int',
                             'dis-int', 'term-dis-int', 'boxes',  
                             'zones', 'oct', 'pk', 'rtz',
                             'w-int'],
                    dest='crab_dom', default='zones')
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
                   help='Track integers (num), num + pointer offsets (ptr), and num + memory contents (arr) via memory abstraction',
                   choices=['num', 'ptr', 'arr'], dest='track', default='num')
    p.add_argument('--crab-heap-analysis',
                   help="Heap analysis used for memory disambiguation (if --crab-track=arr):\n"
                   "- llvm-dsa: context-insensitive llvm-dsa (deprecated) \n"
                   "- ci-sea-dsa: context-insensitive sea-dsa\n"
                   "- cs-sea-dsa: context-sensitive sea-dsa\n"
                   "- ci-sea-dsa-types: context-insensitive sea-dsa with types (default)\n"
                   "- cs-sea-dsa-types: context-sensitive sea-dsa with types\n",
                   choices=['none',
                            'llvm-dsa',
                            'ci-sea-dsa', 'cs-sea-dsa',
                            'ci-sea-dsa-types', 'cs-sea-dsa-types'],                   
                    dest='crab_heap_analysis',
                    default='ci-sea-dsa-types')
    p.add_argument('--crab-singleton-aliases',
                    help='Translate singleton alias sets (mostly globals) as scalar values',
                    dest='crab_singleton_aliases', default=False, action='store_true')
    p.add_argument('--crab-inter',
                    help='Run summary-based, inter-procedural analysis',
                    dest='crab_inter', default=False, action='store_true')
    # p.add_argument('--crab-inter-sum-dom',
    #                 help='Choose abstract domain for computing summaries',
    #                 choices=['zones','oct','rtz'],
    #                 dest='crab_inter_sum_dom', default='zones')
    p.add_argument('--crab-inter-max-summaries', 
                    type=int, dest='inter_max_summaries', 
                    help='Max number of summaries per function',
                    default=1000000)
    p.add_argument('--crab-backward',
                    help='Run iterative forward/backward analysis for proving assertions (only intra version available and very experimental)',
                    dest='crab_backward', default=False, action='store_true')
    # WARNING: --crab-live may lose precision.
    # If x=z in bb1 and y=z in bb2 and z is dead after bb1 and bb2 then
    # the equality x=y is lost.
    p.add_argument('--crab-live',
                    help='Delete dead symbols: may lose precision with relational domains.',
                    dest='crab_live', default=False, action='store_true')        
    p.add_argument('--crab-add-invariants',
                    help='Instrument code with invariants at different locations',
                    choices=['none',
                             'dead-code',
                             'block-entry',
                             'loop-header',                             
                             'after-load',
                             'all'],
                    dest='insert_inv_loc', default='none')
    p.add_argument('--crab-do-not-store-invariants',
                    help='Do not store invariants',
                    dest='store_invariants', default=True, action='store_false')        
    p.add_argument('--crab-promote-assume',
                    help='Promote verifier.assume calls to llvm.assume intrinsics',
                    dest='crab_promote_assume', default=False, action='store_true')
    p.add_argument('--crab-check',
                    help='Check user assertions (default no check)',
                    choices=['none', 'assert'],
                    dest='assert_check', default='none')
    p.add_argument('--crab-check-verbose', metavar='INT',
                    help='Print verbose information about checks\n' + 
                         '>=1: only error checks\n' + 
                         '>=2: error and warning checks\n' + 
                         '>=3: error, warning, and safe checks',
                    dest='check_verbose', type=int, default=0)
    p.add_argument('--crab-print-summaries',
                    help='Display computed summaries (if --crab-inter)',
                    dest='print_summs', default=False, action='store_true')
    p.add_argument('--crab-print-cfg',
                    help='Display crab CFG',
                    dest='print_cfg', default=False, action='store_true')
    p.add_argument('--crab-do-not-print-invariants',
                    help='Do not print invariants',
                    dest='crab_print_invariants', default=True, action='store_false')    
    p.add_argument('--crab-print-unjustified-assumptions',
                    help='Print unjustified assumptions done by the analyzer (experimental, only for integer overflows)',
                    dest='print_assumptions', default=False, action='store_true')
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
    # Options that might affect soundness
    ######################################################################
    ## This might be unsound if disable and the abstract array domain is smashing
    p.add_argument('--crab-disable-array-smashing',
                    help=a.SUPPRESS,
                    dest='crab_disable_array_smashing', default=False, action='store_true')
    ## These might be unsound if enabled
    p.add_argument('--crab-dsa-disambiguate-unknown',
                    help=a.SUPPRESS,
                    dest='crab_dsa_unknown', default=False, action='store_true')
    p.add_argument('--crab-dsa-disambiguate-ptr-cast',
                    help=a.SUPPRESS,
                    dest='crab_dsa_ptr_cast', default=False, action='store_true')
    p.add_argument('--crab-dsa-disambiguate-external',
                    help=a.SUPPRESS,
                    dest='crab_dsa_external', default=False, action='store_true')
    p.add_argument('--crab-unsound-array-init',
                    help=a.SUPPRESS,
                    dest='crab_unsound_array_init', default=False, action='store_true')
    ######################################################################
    # Other hidden options
    ######################################################################
    # Choose between own crab way of naming values and instnamer
    p.add_argument('--crab-name-values',
                    help=a.SUPPRESS,
                    dest='crab_name_values', default=True, action='store_false')
    p.add_argument('--crab-keep-shadows',
                    help=a.SUPPRESS,
                    dest='crab_keep_shadows', default=False, action='store_true')
    p.add_argument('--crab-unsigned-to-signed',
                    help=a.SUPPRESS,
                    dest='unsigned_to_signed', default=False, action='store_true')
    p.add_argument('--crab-enable-bignums',
                    help=a.SUPPRESS,
                    dest='crab_enable_bignums', default=False, action='store_true')
    #Instrument each memory instruction with shadow.mem functions to
    #convert the program into memory SSA form and translate to crab
    #preserving that memory SSA form.
    p.add_argument('--crab-memssa',
                    help=a.SUPPRESS,
                    dest='crab_memssa', default=False, action='store_true')
    
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

    if verbose:
        print "Working directory", workdir

    if not save:
        atexit.register(shutil.rmtree, path=workdir)
    return workdir

def getClam():
    clam = None
    if 'CLAM' in os.environ: clam = os.environ ['CLAM']
    if not isexec(clam):
        clam = os.path.join(root, "bin/clam")
    if not isexec(clam): clam = which('clam')
    if not isexec(clam):
        raise IOError("Cannot find clam")
    return clam

def getClamPP():
    crabpp = None
    if 'CLAMPP' in os.environ:
        crabpp = os.environ ['CLAMPP']
    if not isexec(crabpp):
        crabpp = os.path.join(root, "bin/clam-pp")
    if not isexec(crabpp): crabpp = which('clam-pp')
    if not isexec(crabpp):
        raise IOError("Cannot find clam pre-processor")
    return crabpp

def getClangVersion(clang):
    p = sub.Popen([clang,'--version'], stdout = sub.PIPE)
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
        cmd_name = which (['clang++-mp-5.0', 'clang++-5.0', 'clang++'])
    else:
        cmd_name = which (['clang-mp-5.0', 'clang-5.0', 'clang'])
    if cmd_name is None:
        raise IOError('clang was not found')
    return cmd_name

# return a pair: the first element is the command and the second is a
# bool that it is true if seaopt has been found.
def getOptLlvm ():
    cmd_name = which (['seaopt'])
    if cmd_name is not None:
        return (cmd_name, True)
    
    cmd_name = which (['opt-mp-5.0', 'opt-5.0', 'opt'])
    if cmd_name is None:
        raise IOError ('neither seaopt nor opt where found')
    return (cmd_name, False)

### Passes
def defBCName(name, wd=None):
    base = os.path.basename(name)
    if wd == None: wd = os.path.dirname (name)
    fname = os.path.splitext(base)[0] + '.bc'
    return os.path.join(wd, fname)
def defPPName(name, wd=None):
    base = os.path.basename(name)
    if wd == None: wd = os.path.dirname (name)
    fname = os.path.splitext(base)[0] + '.pp.bc'
    return os.path.join(wd, fname)
def defOptName(name, wd=None):
    base = os.path.basename(name)
    if wd == None: wd = os.path.dirname (name)
    fname = os.path.splitext(base)[0] + '.o.bc'
    return os.path.join(wd, fname)

def defOutPPName(name, wd=None):
    base = os.path.basename(name)
    if wd == None: wd = os.path.dirname (name)
    fname = os.path.splitext(base)[0] + '.ll'
    return os.path.join(wd, fname)


def _plus_plus_file(name):
    ext = os.path.splitext(name)[1]
    return ext == '.cpp' or ext == '.cc'

# Run Clang
def clang(in_name, out_name, args, arch=32, extra_args=[]):

    if os.path.splitext(in_name)[1] == '.bc':
        if verbose:
            print '--- Clang skipped: input file is already bitecode'
        shutil.copy2(in_name, out_name)
        return

    if out_name == '' or out_name == None:
        out_name = defBCName(in_name)

    clang_cmd = getClang(_plus_plus_file(in_name))
    clang_version = getClangVersion(clang_cmd)
    if not clang_version == "not-found":
        if not clang_version.startswith(llvm_version):
            print "WARNING clam.py: clang version " + clang_version +  \
                " different from " + llvm_version
    
    clang_args = [clang_cmd, '-emit-llvm', '-o', out_name, '-c', in_name ]
    
    # New for clang 5.0: to avoid add optnone if -O0
    # Otherwise, seaopt cannot optimize.
    clang_args.append('-Xclang')
    clang_args.append('-disable-O0-optnone')
            
    clang_args.extend (extra_args)
    clang_args.append ('-m{0}'.format (arch))

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
    
    ## Hack for OSX Mojave that no longer exposes libc and libstd headers by default
    osx_sdk_dirs = ['/Applications/Xcode.app/Contents/Developer/Platforms/' + \
                    'MacOSX.platform/Developer/SDKs/MacOSX10.14.sdk',
                    '/Applications/Xcode.app/Contents/Developer/Platforms/' + \
                    'MacOSX.platform/Developer/SDKs/MacOSX10.15.sdk']

    for osx_sdk_dir in osx_sdk_dirs:
        if os.path.isdir(osx_sdk_dir):
            clang_args.append('--sysroot=' + osx_sdk_dir)
            break
    
    if verbose: print ' '.join(clang_args)
    returnvalue, timeout, out_of_mem, segfault, unknown = \
        run_command_with_limits(clang_args, -1, -1)
    if timeout:
        sys.exit(FRONTEND_TIMEOUT)
    elif out_of_mem:
        sys.exit(FRONTEND_MEMORY_OUT)
    elif segfault or unknown or returnvalue <> 0:
        sys.exit(CLANG_ERROR)    

# Run llvm optimizer
def optLlvm(in_name, out_name, args, extra_args=[], cpu = -1, mem = -1):
    if out_name == '' or out_name == None:
        out_name = defOptName(in_name)
    opt_cmd, is_seaopt = getOptLlvm()
        
    opt_args = [opt_cmd, '-f', '-funit-at-a-time']
    if out_name is not None: opt_args.extend(['-o', out_name])
    opt_args.append('-O{0}'.format(args.L))

    # disable sinking instructions to end of basic block
    # this might create unwanted aliasing scenarios
    # for now, there is no option to undo this switch
    opt_args.append('--simplifycfg-sink-common=false')

    # disable always vectorization
    opt_args.append('--disable-loop-vectorization')
    opt_args.append('--disable-slp-vectorization')

    if is_seaopt:
        # disable always loop rotation. Loop rotation converts to loops
        # that are much harder to reason about them using crab due to
        # several reasons:
        # 
        # 1. Complex loops that break widening heuristics
        # 2. Rewrite loop exits by adding often disequalities
        # 3. Introduce new *unsigned* loop variables.
        opt_args.append('--disable-loop-rotate')
    
    # These two should be optional
    #opt_args.append('--enable-indvar=true')
    #opt_args.append('--enable-loop-idiom=true')

    if is_seaopt:
        if args.undef_nondet: 
            opt_args.append('--enable-nondet-init=true')
        else: 
            opt_args.append('--enable-nondet-init=false')
            
    if args.inline_threshold is not None:
        opt_args.append('--inline-threshold={t}'.format
                        (t=args.inline_threshold))
    if args.unroll_threshold is not None:
        opt_args.append('--unroll-threshold={t}'.format
                        (t=args.unroll_threshold))
    if args.print_after_all: opt_args.append('--print-after-all')
    if args.debug_pass: opt_args.append('--debug-pass=Structure')    
    opt_args.extend(extra_args)
    opt_args.append(in_name)

    if verbose: print ' '.join(opt_args)
    returnvalue, timeout, out_of_mem, segfault, unknown = \
        run_command_with_limits(opt_args, cpu, mem)
    if timeout:
        sys.exit(FRONTEND_TIMEOUT)
    elif out_of_mem:
        sys.exit(FRONTEND_MEMORY_OUT)
    elif unknown or returnvalue <> 0:
        sys.exit(OPT_ERROR)

# Generate dot files for each LLVM function.
def dot(in_name, view_dot = False, cpu = -1, mem = -1):
    fnull = open(os.devnull, 'w')
    args = [getOptLlvm(), in_name, '-dot-cfg']
    if view_dot: args.append('-view-cfg')
    if verbose: print ' '.join(args)
    ## We don't bother here analyzing the exit code
    run_command_with_limits(args, cpu, mem, fnull)
    
# Run crabpp
def crabpp(in_name, out_name, args, extra_args=[], cpu = -1, mem = -1):
    if out_name == '' or out_name == None:
        out_name = defPPName(in_name)

    crabpp_args = [getClamPP(), '-o', out_name, in_name ]
    
    # disable sinking instructions to end of basic block
    # this might create unwanted aliasing scenarios
    # for now, there is no option to undo this switch
    crabpp_args.append('--simplifycfg-sink-common=false')

    if args.inline: 
        crabpp_args.append('--crab-inline-all')
    if args.pp_loops: 
        crabpp_args.append('--clam-pp-loops')
    if args.undef_nondet:
        crabpp_args.append('--crab-turn-undef-nondet')
        
    if args.disable_lower_gv:
        crabpp_args.append('--crab-lower-gv=false')
    if args.disable_scalarize:
        crabpp_args.append('--crab-scalarize=false')
    if args.disable_lower_cst_expr:
        crabpp_args.append('--crab-lower-constant-expr=false')
    if args.disable_lower_switch:
        crabpp_args.append('--crab-lower-switch=false')
        
    # Postponed until clam is run, otherwise it can be undone by the optLlvm
    # if args.lower_unsigned_icmp:
    #     crabpp_args.append( '--crab-lower-unsigned-icmp')
    if args.devirt is not 'none':
        crabpp_args.append('--crab-devirt')
        if args.devirt == 'types':
            crabpp_args.append('--devirt-resolver=types')
        elif args.devirt == 'sea-dsa':
            crabpp_args.append('--devirt-resolver=sea-dsa')
            crabpp_args.append('--sea-dsa-type-aware=true')            
        elif args.devirt == 'dsa':
            crabpp_args.append('--devirt-resolver=dsa')            
    if args.enable_ext_funcs:
        crabpp_args.append('--crab-externalize-addr-taken-funcs')
    if args.print_after_all: crabpp_args.append('--print-after-all')
    if args.debug_pass: crabpp_args.append('--debug-pass=Structure')        

    if args.dsa_log is not None:
        for l in args.dsa_log.split(':'): crabpp_args.extend(['-sea-dsa-log', l])
    
    crabpp_args.extend(extra_args)
    if verbose: print ' '.join(crabpp_args)
    returnvalue, timeout, out_of_mem, segfault, unknown = \
        run_command_with_limits(crabpp_args, cpu, mem)
    if timeout:
        sys.exit(FRONTEND_TIMEOUT)
    elif out_of_mem:
        sys.exit(FRONTEND_MEMORY_OUT)
    elif segfault or unknown or returnvalue <> 0:
        sys.exit(PP_ERROR)
    
# Run clam
def clam(in_name, out_name, args, extra_opts, cpu = -1, mem = -1):
    clam_args = [ getClam(), in_name, '-oll', out_name]
    clam_args = clam_args + extra_opts
    
    if args.log is not None:
        for l in args.log.split(':'): clam_args.extend(['-crab-log', l])

    if args.dsa_log is not None:
        for l in args.dsa_log.split(':'): clam_args.extend(['-sea-dsa-log', l])
        
    # disable sinking instructions to end of basic block
    # this might create unwanted aliasing scenarios
    # for now, there is no option to undo this switch
    clam_args.append('--simplifycfg-sink-common=false')

    if args.crab_verbose:
        clam_args.append('--crab-verbose={0}'.format(args.crab_verbose))
    if args.crab_only_cfg:
        clam_args.append('--crab-only-cfg')
    ## This option already run in crabpp    
    if args.undef_nondet: clam_args.append( '--crab-turn-undef-nondet')

    if args.lower_unsigned_icmp:
        clam_args.append('--crab-lower-unsigned-icmp')
    if args.lower_select:
        clam_args.append('--crab-lower-select')
    if args.disable_lower_cst_expr:
        clam_args.append('--crab-lower-constant-expr=false')
    if args.disable_lower_switch:
        clam_args.append('--crab-lower-switch=false')
    
    clam_args.append('--crab-dom={0}'.format(args.crab_dom))
    clam_args.append('--crab-widening-delay={0}'.format(args.widening_delay))
    clam_args.append('--crab-widening-jump-set={0}'.format(args.widening_jump_set))
    clam_args.append('--crab-narrowing-iterations={0}'.format(args.narrowing_iterations))
    clam_args.append('--crab-relational-threshold={0}'.format(args.num_threshold))
    if args.track == 'arr':    
        clam_args.append('--crab-track=arr')
        clam_args.append('--crab-arr-init')
    else:
        clam_args.append('--crab-track={0}'.format(args.track))
    if args.crab_heap_analysis == 'none' or \
       args.crab_heap_analysis == 'llvm-dsa' or \
       args.crab_heap_analysis == 'ci-sea-dsa' or \
       args.crab_heap_analysis == 'cs-sea-dsa':
        clam_args.append('--crab-heap-analysis={0}'.format(args.crab_heap_analysis))
    elif args.crab_heap_analysis == 'ci-sea-dsa-types':
        clam_args.append('--crab-heap-analysis=ci-sea-dsa')
        clam_args.append('--sea-dsa-type-aware=true')
    elif args.crab_heap_analysis == 'cs-sea-dsa-types':
        clam_args.append('--crab-heap-analysis=cs-sea-dsa')
        clam_args.append('--sea-dsa-type-aware=true')
    if args.crab_singleton_aliases: clam_args.append('--crab-singleton-aliases')
    if args.crab_inter:
        clam_args.append('--crab-inter')
        clam_args.append('--crab-inter-max-summaries={0}'.format(args.inter_max_summaries))
        #clam_args.append('--crab-inter-sum-dom={0}'.format(args.crab_inter_sum_dom))
        
    if args.crab_backward: clam_args.append('--crab-backward')
    if args.crab_live: clam_args.append('--crab-live')
    clam_args.append('--crab-add-invariants={0}'.format(args.insert_inv_loc))
    if args.crab_promote_assume: clam_args.append('--crab-promote-assume')
    if args.assert_check: clam_args.append('--crab-check={0}'.format(args.assert_check))
    if args.check_verbose:
        clam_args.append('--crab-check-verbose={0}'.format(args.check_verbose))
    if args.print_summs: clam_args.append('--crab-print-summaries')
    if args.print_cfg: clam_args.append('--crab-print-cfg')
    if args.print_stats: clam_args.append('--crab-stats')
    if args.print_assumptions: clam_args.append('--crab-print-unjustified-assumptions')
    if args.crab_disable_warnings:
        clam_args.append('--crab-enable-warnings=false')
    if args.crab_sanity_checks: clam_args.append('--crab-sanity-checks')
    if args.crab_cfg_simplify: clam_args.append('--crab-cfg-simplify')
    if args.crab_print_invariants:
        clam_args.append('--crab-print-invariants')
    if args.store_invariants:
        clam_args.append('--crab-store-invariants=true')
    else:
        clam_args.append('--crab-store-invariants=false')    
    # begin hidden options
    if args.crab_disable_array_smashing:
        clam_args.append('--crab-use-array-smashing=false')
    if args.crab_dsa_unknown: clam_args.append('--crab-dsa-disambiguate-unknown')
    if args.crab_dsa_ptr_cast: clam_args.append('--crab-dsa-disambiguate-ptr-cast')
    if args.crab_dsa_external: clam_args.append('--crab-dsa-disambiguate-external')    
    if args.crab_unsound_array_init: clam_args.append('--crab-unsound-array-init') 
    if args.crab_keep_shadows: clam_args.append('--crab-keep-shadows')
    if args.crab_name_values:
        clam_args.append('--crab-name-values=true')
    else:
        clam_args.append('--crab-name-values=false')    
    if args.unsigned_to_signed: clam_args.append('--crab-unsigned-to-signed')
    if args.crab_enable_bignums:
        clam_args.append('--crab-enable-bignums=true')
    else:
        clam_args.append('--crab-enable-bignums=false')
    if args.crab_memssa:
        clam_args.append('--crab-memssa=true')
    # end hidden options
        
    if verbose: print ' '.join(clam_args)

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
    elif unknown or returnvalue <> 0:
        # crab returns EXIT_FAILURE which in most platforms is 1 but not in all.
        sys.exit(CRAB_ERROR)
    
def main(argv):
    def stat(key, val): stats.put(key, val)
    os.setpgrp()
    loadEnv(os.path.join(root, "env.common"))

    ## add directory containing this file to the PATH
    os.environ ['PATH'] =  os.path.dirname(os.path.realpath(__file__)) + \
                           os.pathsep + os.environ['PATH']

    if '--llvm-version' in argv[1:] or '-llvm-version' in argv[1:]:
        print "LLVM version " + llvm_version
        return 0
    
    if '--clang-version' in argv[1:] or '-clang-version' in argv[1:]:
        print "Clang version " + getClangVersion(getClang(False))
        return 0

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
        if args.only_preprocess:
            extra_opts.append('-no-crab')
        clam(in_name, pp_out, args, extra_opts, cpu=args.cpu, mem=args.mem)

    if args.dot_cfg: dot(pp_out)
    if args.view_cfg: dot(pp_out, True)
    #stat('Progress', 'Clam')

    if args.asm_out_name is not None and args.asm_out_name != pp_out:
        if verbose: print 'cp {0} {1}'.format(pp_out, args.asm_out_name)
        shutil.copy2(pp_out, args.asm_out_name)

    return 0

def killall():
    global running_process
    if running_process != None:
        try:
            running_process.terminate()
            running_process.kill()
            running_process.wait()
            running_process = None
        except OSError: pass

if __name__ == '__main__':
    # unbuffered output
    sys.stdout = os.fdopen(sys.stdout.fileno(), 'w', 0)
    try:
        signal.signal(signal.SIGTERM, lambda x, y: killall())
        sys.exit(main(sys.argv))
    except KeyboardInterrupt: pass
    finally:
        killall()
        stats.brunch_print()

