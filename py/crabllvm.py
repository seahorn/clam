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

root = os.path.dirname (os.path.dirname (os.path.realpath (__file__)))
verbose = False

running_process = None

####### SPECIAL ERROR CODES USEFUL FOR DEBUGGING ############
## special error codes for the frontend (clang + opt + pp)
ERR_FRONTEND_TIMEOUT=20    
ERR_FRONTEND_MEMORY_OUT=21  
#### specific errors for each frontend component
ERR_CLANG = 22
ERR_OPT = 23
ERR_PP = 24
## special error code for crab-llvm/crab
ERR_CRAB = 25
#############################################################

def isexec (fpath):
    if fpath == None: return False
    return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

def which(program):
    if isinstance (program, str):
        choices = [program]
    else:
        choices = program

    for p in choices:
        fpath, fname = os.path.split(p)
        if fpath:
            if isexec (p): return p
        else:
            for path in os.environ["PATH"].split(os.pathsep):
                exe_file = os.path.join(path, p)
                if isexec (exe_file):
                    return exe_file
    return None

def run_command_with_limits(cmd, cpu, mem, out = None):
    def set_limits ():
        if mem > 0:
            mem_bytes = mem * 1024 * 1024
            resource.setrlimit (resource.RLIMIT_AS, [mem_bytes, mem_bytes])
    def kill (proc):
        try:
            proc.terminate ()
            proc.kill ()
            proc.wait ()
            global running_process
            running_process = None
        except OSError: pass
        
    if out is not None:
        p = sub.Popen (cmd, stdout = out, preexec_fn=set_limits)
    else:
        p = sub.Popen (cmd, preexec_fn=set_limits)    
    global running_process
    running_process = p
    timer = threading.Timer (cpu, kill, [p])
    if cpu > 0: timer.start ()
    try:
        (pid, returnvalue, ru_child) = os.wait4 (p.pid, 0)
        # returnvalue is a 16-bit number, whose low byte is the signal number 
        # that killed the process, and whose high byte is the exit status 
        # (if the signal number is zero)
        killed_by_signal = returnvalue & 127
        if not killed_by_signal:
            returnvalue = returnvalue >> 8
        running_process = None
    except OSError:
        ## p has been killed probably because it consumed too much memory
        ## It could be for other reasons but we assume OOM
        returnvalue = 134 
    finally:
        ## kill the timer if the process has terminated already
        if timer.isAlive (): timer.cancel ()            
    #print "RETURNVALUE =" + str(returnvalue)    
    return returnvalue

# # Based on http://www.ostricher.com/2015/01/python-subprocess-with-timeout/    
# class SubprocessTimeout(RuntimeError):
#   """Error class for subprocess timeout."""
#   pass
    
# def run_command_with_limits(cmd, cpu, mem, out = None):
#     """Execute `cmd` in a subprocess and enforce timeout `cpu` seconds.
#     Return subprocess exit code on natural completion of the subprocess.
#     Raise an exception if timeout expires before subprocess completes."""

#     def set_limits ():
#         if mem > 0:
#             mem_bytes = mem * 1024 * 1024
#             resource.setrlimit (resource.RLIMIT_AS, [mem_bytes, mem_bytes])

#     if not out is None:
#         proc = sub.Popen (cmd, stdout = out, preexec_fn=set_limits)
#     else:
#         proc = sub.Popen (cmd, preexec_fn=set_limits)
            
#     proc_thread = threading.Thread(target=proc.communicate)
#     proc_thread.start()
#     if cpu > 0:
#         proc_thread.join(cpu)
#     else:
#         proc_thread.join()
        
#     if proc_thread.is_alive():
#         # Process still running - kill it and raise timeout error
#         try:
#             proc.kill()
#         except OSError, e:
#             # The process finished between the `is_alive()` and `kill()`
#             #return proc.returncode
#             sys.exit(proc.returncode)
#         # OK, the process was definitely killed
#         # raise SubprocessTimeout('Process #%d killed after %f seconds' % (proc.pid, cpu))
#         sys.exit(proc.returncode)

def loadEnv (filename):
    if not os.path.isfile (filename): return

    f = open (filename)
    for line in f:
        sl = line.split('=', 1)
        # skip lines without equality
        if len(sl) != 2:
            continue
        (key, val) = sl
        os.environ [key] = os.path.expandvars (val.rstrip ())

        
def parseArgs (argv):
    import argparse as a
    from argparse import RawTextHelpFormatter

    p = a.ArgumentParser(description='Abstract Interpretation-based Analyzer for LLVM bitecode',
                         formatter_class=RawTextHelpFormatter)
    p.add_argument ('-oll', dest='asm_out_name', metavar='FILE',
                       help='Output analyzed bitecode')
    p.add_argument ('--log', dest='log', default=None,
                    metavar='STR', help='Log level')
    p.add_argument ('-o', dest='out_name', metavar='FILE',
                       help='Output file name')
    p.add_argument ("--save-temps", dest="save_temps",
                       help="Do not delete temporary files",
                       action="store_true",
                       default=False)
    p.add_argument ("--temp-dir", dest="temp_dir", metavar='DIR',
                       help="Temporary directory",
                       default=None)
    p.add_argument ('-g', default=False, action='store_true', dest='debug_info',
                    help='Compile with debug information')
    p.add_argument ('-m', type=int, dest='machine',
                       help='Machine architecture MACHINE:[32,64]', default=32)
    p.add_argument ("--no-preprocess", dest="preprocess", 
                       help='Skip compilation and preprocessing', action='store_false',
                       default=True)
    p.add_argument ("--no-analyze", dest="analyze", 
                       help='Skip analysis (for debugging)', action='store_false',
                       default=True)
    p.add_argument ('-O', type=int, dest='L', metavar='INT',
                       help='Optimization level L:[0,1,2,3]', default=0)
    p.add_argument ('--cpu', type=int, dest='cpu', metavar='SEC',
                       help='CPU time limit (seconds)', default=-1)
    p.add_argument ('--mem', type=int, dest='mem', metavar='MB',
                       help='MEM limit (MB)', default=-1)
    p.add_argument ('--llvm-dot-cfg',
                    help='Print LLVM CFG of function to dot file',
                    dest='dot_cfg', default=False, action='store_true')
    p.add_argument ('--llvm-view-cfg',
                    help='View LLVM CFG of function',
                    dest='view_cfg', default=False, action='store_true')
    p.add_argument ('--llvm-inline-threshold', dest='inline_threshold',
                    type=int, metavar='NUM',
                    help='Inline threshold (default = 255)')
    p.add_argument ('--llvm-pp-loops',
                    help='Optimizing loops',
                    dest='pp_loops', default=False, action='store_true')
    p.add_argument ('--llvm-unroll-threshold', type=int,
                    help='Unrolling threshold (default = 150)',
                    dest='unroll_threshold',
                    default=150, metavar='NUM')
    p.add_argument ('--inline', dest='inline', help='Inline all functions',
                    default=False, action='store_true')
    p.add_argument ('--turn-undef-nondet',
                    help='Turn undefined behaviour into non-determinism',
                    dest='undef_nondet', default=False, action='store_true')
    p.add_argument ('--lower-select',
                    help='Lower select instructions',
                    dest='lower_select', default=False, action='store_true')
    p.add_argument ('--disable-lower-gv',
                    help='Disable lowering of global variable initializers into main',
                    dest='disable_lower_gv', default=False, action='store_true')
    p.add_argument ('--lower-unsigned-icmp',
                    help='Lower ULT and ULE instructions',
                    dest='lower_unsigned_icmp', default=False, action='store_true')
    p.add_argument ('--devirt-functions',
                    help='Resolve indirect calls',
                    dest='devirt', default=False, action='store_true')
    p.add_argument ('file', metavar='FILE', help='Input file')
    ### BEGIN CRAB
    p.add_argument ('--crab-dom',
                    help="Choose abstract domain:\n"
                          "- int: intervals\n"
                          "- ric: reduced product of intervals and congruences\n"
                          "- term-int: int with uninterpreted functions\n"
                          "- dis-int: disjunctive intervals based on Clousot's DisInt domain\n"
                          "- term-dis-int: dis-int with uninterpreted functions\n"
                          "- boxes: disjunctive intervals based on LDDs\n"
                          "- zones: zones domain using sparse DBM in Split Normal Form\n"
                          "- oct: Elina's optimized octagon domain\n"
                          "- pk: Apron's polka domain\n"
                          "- rtz: reduced product of term-dis-int with zones\n",
                    choices=['int','ric', 'term-int',
                             'dis-int', 'term-dis-int', 'boxes',  
                             'zones', 'oct', 'pk', 'rtz', ],
                    dest='crab_dom', default='zones')
    p.add_argument ('--crab-widening-delay', 
                    type=int, dest='widening_delay', 
                    help='Max number of iterations until performing widening', default=1)
    p.add_argument ('--crab-widening-jump-set', 
                    type=int, dest='widening_jump_set', 
                    help='Size of the jump set used in widening', default=0)
    p.add_argument ('--crab-narrowing-iterations', 
                    type=int, dest='narrowing_iterations', 
                    help='Max number of narrowing iterations', default=3)
    p.add_argument ('--crab-relational-threshold', 
                    type=int, dest='num_threshold', 
                    help='Max number of live vars per block before switching to a non-relational domain',
                    default=10000)
    p.add_argument ('--crab-track',
                    help='Track integers, pointer offsets, and memory contents',
                    choices=['num', 'ptr', 'arr'], dest='track', default='num')
    p.add_argument ('--crab-bool-as-int',
                    help='Boolean variables are treated as integers',
                    dest='crab_disable_bool', default=False, action='store_true')        
    p.add_argument ('--crab-disable-ptr',
                    help='Track memory contents but ignoring pointer offsets',
                    dest='crab_disable_ptr', default=False, action='store_true')    
    p.add_argument ('--crab-singleton-aliases',
                    help='Treat singleton alias sets as scalar values',
                    dest='crab_singleton_aliases', default=False, action='store_true')
    p.add_argument ('--crab-inter',
                    help='Run inter-procedural analysis',
                    dest='crab_inter', default=False, action='store_true')
    p.add_argument ('--crab-inter-sum-dom',
                    help='Choose abstract domain for computing summaries',
                    choices=['zones','oct','rtz'],
                    dest='crab_inter_sum_dom', default='zones')
    p.add_argument ('--crab-backward',
                    help='Run iterative forward/backward analysis (only intra version available)',
                    dest='crab_backward', default=False, action='store_true')
    # --crab-live may lose precision e.g. when computing summaries.
    # However, note that due to non-monotonicity of operators such as widening the use of
    # liveness information may actually improve precision. Thus, it's quite unpredictable
    # the impact of this option.
    p.add_argument ('--crab-live',
                    help='Use of liveness information: may lose precision with relational domains.',
                    dest='crab_live', default=False, action='store_true')        
    p.add_argument ('--crab-add-invariants',
                    help='Instrument code with invariants',
                    choices=['none', 'block-entry', 'after-load', 'all'],
                    dest='insert_invs', default='none')
    p.add_argument ('--crab-check',
                    help='Check assertions: user assertions, null dereference, etc',
                    choices=['none', 'assert', 'null'],
                    dest='assert_check', default='none')
    p.add_argument ('--crab-check-verbose', metavar='INT',
                    help='Print verbose information about checks\n' + 
                         '>=1: only error checks\n' + 
                         '>=2: error and warning checks\n' + 
                         '>=3: error, warning, and safe checks',
                    dest='check_verbose', type=int, default=0)
    p.add_argument ('--crab-print-summaries',
                    help='Display computed summaries (if --crab-inter)',
                    dest='print_summs', default=False, action='store_true')
    p.add_argument ('--crab-print-preconditions',
                    help='Display computed necessary preconditions (if --crab-backward)',
                    dest='print_preconds', default=False, action='store_true')
    p.add_argument ('--crab-print-cfg',
                    help='Display Crab CFG',
                    dest='print_cfg', default=False, action='store_true')
    p.add_argument ('--crab-stats',
                    help='Display Crab statistics',
                    dest='print_stats', default=False, action='store_true')
    p.add_argument ('--crab-print-unjustified-assumptions',
                    help='Print unjustified assumptions done by the analyzer',
                    dest='print_assumptions', default=False, action='store_true')
    p.add_argument ('--crab-disable-warnings',
                    help='Disable some crab-llvm warnings',
                    dest='crab_disable_warnings', default=False, action='store_true')
    ######################################################################
    p.add_argument ('--crab-cfg-simplify',
                    help=a.SUPPRESS,
                    dest='crab_cfg_simplify', default=False, action='store_true')
    p.add_argument ('--crab-keep-shadows',
                    help=a.SUPPRESS,
                    dest='crab_keep_shadows', default=False, action='store_true')
    p.add_argument ('--do-not-print-invariants',
                    help=a.SUPPRESS,
                    dest='do_not_print_invariants', default=False, action='store_true')
    
    #### END CRAB
    
    args = p.parse_args (argv)
    
    if args.L < 0 or args.L > 3:
        p.error ("Unknown option: -O%s" % args.L)

    if args.machine != 32 and args.machine != 64:
        p.error ("Unknown option -m%s" % args.machine)

    return args

def createWorkDir (dname = None, save = False):
    if dname is None:
        workdir = tempfile.mkdtemp (prefix='crabllvm-')
    else:
        workdir = dname

    if verbose:
        print "Working directory", workdir

    if not save:
        atexit.register (shutil.rmtree, path=workdir)
    return workdir

def getCrabLlvm ():
    crabllvm = None
    if 'CRABLLVM' in os.environ: crabllvm = os.environ ['CRABLLVM']
    if not isexec (crabllvm):
        crabllvm = os.path.join (root, "bin/crabllvm")
    if not isexec (crabllvm): crabllvm = which ('crabllvm')
    if not isexec (crabllvm):
        raise IOError ("Cannot find crabllvm")
    return crabllvm

def getCrabLlvmPP ():
    crabpp = None
    if 'CRABLLVMPP' in os.environ:
        crabpp = os.environ ['CRABLLVMPP']
    if not isexec (crabpp):
        crabpp = os.path.join (root, "bin/crabllvm-pp")
    if not isexec (crabpp): crabpp = which ('crabllvm-pp')
    if not isexec (crabpp):
        raise IOError ("Cannot find crabllvm pre-processor")
    return crabpp

def getClang ():
    cmd_name = which (['clang-mp-3.6', 'clang-3.6', 'clang', 'clang-mp-3.5', 'clang-mp-3.4'])
    if cmd_name is None: raise IOError ('clang was not found')
    return cmd_name

def getOptLlvm ():
    cmd_name = which (['seaopt', 'opt-mp-3.6', 'opt-3.6', 'opt'])
    if cmd_name is None: raise IOError ('neither seaopt nor opt where found')
    return cmd_name

### Passes
def defBCName (name, wd=None):
    base = os.path.basename (name)
    if wd == None: wd = os.path.dirname  (name)
    fname = os.path.splitext (base)[0] + '.bc'
    return os.path.join (wd, fname)
def defPPName (name, wd=None):
    base = os.path.basename (name)
    if wd == None: wd = os.path.dirname  (name)
    fname = os.path.splitext (base)[0] + '.pp.bc'
    return os.path.join (wd, fname)
def defOptName (name, wd=None):
    base = os.path.basename (name)
    if wd == None: wd = os.path.dirname  (name)
    fname = os.path.splitext (base)[0] + '.o.bc'
    return os.path.join (wd, fname)

def defOutPPName (name, wd=None):
    base = os.path.basename (name)
    if wd == None: wd = os.path.dirname  (name)
    fname = os.path.splitext (base)[0] + '.ll'
    return os.path.join (wd, fname)

# Run Clang
def clang (in_name, out_name, arch=32, extra_args=[]):
    if os.path.splitext (in_name)[1] == '.bc':
        if verbose:
            print '--- Clang skipped: input file is already bitecode'
        shutil.copy2 (in_name, out_name)
        return

    if out_name == '' or out_name == None:
        out_name = defBCName (in_name)

    clang_args = [getClang (), '-emit-llvm', '-o', out_name, '-c', in_name ]
    clang_args.extend (extra_args)
    clang_args.append ('-m{0}'.format (arch))

    if verbose: print ' '.join (clang_args)
    returnvalue = run_command_with_limits(clang_args, -1, -1)
    if returnvalue != 0:        
        if returnvalue == 9:
            returnvalue = ERR_FRONTEND_TIMEOUT
        elif returnvalue == 134:
            returnvalue = ERR_FRONTEND_MEMORY_OUT
        else:
            returnvalue = ERR_CLANG
        sys.exit(returnvalue)    
    

# Run llvm optimizer
def optLlvm (in_name, out_name, args, extra_args=[], cpu = -1, mem = -1):
    if out_name == '' or out_name == None:
        out_name = defOptName (in_name)

    opt_args = [getOptLlvm (), '-f', '-funit-at-a-time']
    if out_name is not None: opt_args.extend (['-o', out_name])
    opt_args.append('-O{0}'.format (args.L))

    # These two should be optional
    opt_args.append ('--enable-indvar=true')
    opt_args.append ('--enable-loop-idiom=true')

    if args.undef_nondet: 
        opt_args.append ('--enable-nondet-init=true')
    else: 
        opt_args.append ('--enable-nondet-init=false')
    if args.inline_threshold is not None:
        opt_args.append ('--inline-threshold={t}'.format
                         (t=args.inline_threshold))
    if args.unroll_threshold is not None:
        opt_args.append ('--unroll-threshold={t}'.format
                         (t=args.unroll_threshold))
    opt_args.extend (extra_args)
    opt_args.append (in_name)

    if verbose: print ' '.join (opt_args)
    returnvalue = run_command_with_limits(opt_args, cpu, mem)
    if returnvalue != 0:
        if returnvalue == 9:
            returnvalue = ERR_FRONTEND_TIMEOUT
        elif returnvalue == 134:
            returnvalue = ERR_FRONTEND_MEMORY_OUT
        else:
            returnvalue = ERR_OPT
        sys.exit(returnvalue)    
    

# Generate dot files for each LLVM function.
def dot (in_name, view_dot = False, cpu = -1, mem = -1):
    fnull = open(os.devnull, 'w')
    args = [getOptLlvm(), in_name, '-dot-cfg']
    if view_dot: args.append ('-view-cfg')
    if verbose: print ' '.join (args)
    returnvalue = run_command_with_limits(args, cpu, mem, fnull)
    if returnvalue != 0: sys.exit (returnvalue)    
    
# Run crabpp
def crabpp (in_name, out_name, args, extra_args=[], cpu = -1, mem = -1):
    if out_name == '' or out_name == None:
        out_name = defPPName (in_name)

    crabpp_args = [getCrabLlvmPP (), '-o', out_name, in_name ]
    if args.inline: 
        crabpp_args.append ('--crab-inline-all')
    if args.pp_loops: 
        crabpp_args.append ('--crab-llvm-pp-loops')
    if args.undef_nondet:
        crabpp_args.append( '--crab-turn-undef-nondet')
    if args.disable_lower_gv:
        crabpp_args.append( '--crab-lower-gv=false')
    if args.lower_unsigned_icmp:
        crabpp_args.append( '--crab-lower-unsigned-icmp')
    if args.devirt:
        crabpp_args.append ('--crab-devirt')

    crabpp_args.extend (extra_args)
    if verbose: print ' '.join (crabpp_args)
    returnvalue = run_command_with_limits(crabpp_args, cpu, mem)
    if returnvalue != 0:
        if returnvalue == 9:
            returnvalue = ERR_FRONTEND_TIMEOUT
        elif returnvalue == 134:
            returnvalue = ERR_FRONTEND_MEMORY_OUT
        else:
            returnvalue = ERR_PP
        sys.exit(returnvalue)    
    
# Run crabllvm
def crabllvm (in_name, out_name, args, extra_opts, cpu = -1, mem = -1):
    crabllvm_cmd = [ getCrabLlvm(), in_name, '-oll', out_name]
    crabllvm_cmd = crabllvm_cmd + extra_opts

    if args.log is not None:
        for l in args.log.split (':'): crabllvm_cmd.extend (['-log', l])

    ## This option already run in crabpp    
    if args.undef_nondet: crabllvm_cmd.append( '--crab-turn-undef-nondet')
        
    if args.lower_select: crabllvm_cmd.append( '--crab-lower-select')
    crabllvm_cmd.append ('--crab-dom={0}'.format(args.crab_dom))
    crabllvm_cmd.append ('--crab-inter-sum-dom={0}'.format(args.crab_inter_sum_dom))
    crabllvm_cmd.append ('--crab-widening-delay={0}'.format(args.widening_delay))
    crabllvm_cmd.append ('--crab-widening-jump-set={0}'.format(args.widening_jump_set))
    crabllvm_cmd.append ('--crab-narrowing-iterations={0}'.format(args.narrowing_iterations))
    crabllvm_cmd.append ('--crab-relational-threshold={0}'.format(args.num_threshold))
    crabllvm_cmd.append ('--crab-track={0}'.format(args.track))
    if args.crab_disable_bool: crabllvm_cmd.append ('--crab-bool-as-int')    
    if args.crab_disable_ptr: crabllvm_cmd.append ('--crab-disable-ptr')
    if args.crab_singleton_aliases: crabllvm_cmd.append ('--crab-singleton-aliases')
    if args.crab_inter: crabllvm_cmd.append ('--crab-inter')
    if args.crab_backward: crabllvm_cmd.append ('--crab-backward')
    if args.crab_live: crabllvm_cmd.append ('--crab-live')
    crabllvm_cmd.append ('--crab-add-invariants={0}'.format (args.insert_invs))
    if args.assert_check: crabllvm_cmd.append ('--crab-check={0}'.format(args.assert_check))
    if args.check_verbose:
        crabllvm_cmd.append ('--crab-check-verbose={0}'.format(args.check_verbose))
    if args.print_summs: crabllvm_cmd.append ('--crab-print-summaries')
    if args.print_preconds: crabllvm_cmd.append ('--crab-print-preconditions')    
    if args.print_cfg: crabllvm_cmd.append ('--crab-print-cfg')
    if args.print_stats: crabllvm_cmd.append ('--crab-stats')
    if args.print_assumptions: crabllvm_cmd.append ('--crab-print-unjustified-assumptions')    
    if args.crab_disable_warnings: crabllvm_cmd.append('--crab-disable-warnings')

    # hidden options
    if args.crab_cfg_simplify: crabllvm_cmd.append ('--crab-cfg-simplify')
    if args.crab_keep_shadows: crabllvm_cmd.append ('--crab-keep-shadows')
    if not args.do_not_print_invariants: crabllvm_cmd.append ('--crab-print-invariants')
        
    if verbose: print ' '.join (crabllvm_cmd)

    if args.out_name is not None:
        crabllvm_cmd.append ('-o={0}'.format (args.out_name))

    returnvalue = run_command_with_limits(crabllvm_cmd, cpu, mem)
    if returnvalue != 0:
        # crab returns EXIT_FAILURE which in most platforms is 1 but not in all.
        if returnvalue == 1:
            returnvalue = ERR_CRAB
        sys.exit (returnvalue)    
    
def main (argv):
    def stat (key, val): stats.put (key, val)
    os.setpgrp ()
    loadEnv (os.path.join (root, "env.common"))

    ## add directory containing this file to the PATH
    os.environ ['PATH'] =  os.path.dirname (os.path.realpath (__file__)) + \
                           os.pathsep + os.environ['PATH']
    
    args  = parseArgs (argv[1:])
    workdir = createWorkDir (args.temp_dir, args.save_temps)
    in_name = args.file

    if args.preprocess:
        bc_out = defBCName (in_name, workdir)
        if bc_out != in_name:
                extra_args = []
                if args.debug_info: extra_args.append ('-g')
                with stats.timer ('Clang'):
                    clang (in_name, bc_out, arch=args.machine, extra_args=extra_args)
                #stat ('Progress', 'Clang')
        in_name = bc_out

        pp_out = defPPName (in_name, workdir)
        if pp_out != in_name:
            with stats.timer ('CrabLlvmPP'):
                crabpp (in_name, pp_out, args=args, cpu=args.cpu, mem=args.mem)
            #stat ('Progress', 'Crab Llvm preprocessor')
        in_name = pp_out

    if args.L > 0:
        o_out = defOptName (in_name, workdir)
        if o_out != in_name:
            extra_args = []
            with stats.timer ('CrabOptLlvm'):
                optLlvm (in_name, o_out, args, extra_args, cpu=args.cpu, mem=args.mem)
            #stat ('Progress', 'Llvm optimizer')
        in_name = o_out

    pp_out = defOutPPName(in_name, workdir)
    with stats.timer ('CrabLlvm'):
        extra_opts = []
        if not args.analyze:
            extra_opts.append ('-no-crab')
        crabllvm (in_name, pp_out, args, extra_opts, cpu=args.cpu, mem=args.mem)

    if args.dot_cfg: dot(pp_out)
    if args.view_cfg: dot(pp_out, True)
    #stat ('Progress', 'Crab Llvm')

    if args.asm_out_name is not None and args.asm_out_name != pp_out:
        if verbose: print 'cp {0} {1}'.format (pp_out, args.asm_out_name)
        shutil.copy2 (pp_out, args.asm_out_name)

    return 0

def killall ():
    global running_process
    if running_process != None:
        try:
            running_process.terminate ()
            running_process.kill ()
            running_process.wait ()
            running_process = None
        except OSError: pass

if __name__ == '__main__':
    # unbuffered output
    sys.stdout = os.fdopen (sys.stdout.fileno (), 'w', 0)
    try:
        signal.signal (signal.SIGTERM, lambda x, y: killall ())
        sys.exit (main (sys.argv))
    except KeyboardInterrupt: pass
    finally:
        killall ()
        stats.brunch_print ()

