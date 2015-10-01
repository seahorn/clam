#!/usr/bin/env python

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
verbose = True

running_process = None

def isexec (fpath):
    if fpath == None: return False
    return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

def which(program):
    fpath, fname = os.path.split(program)
    if fpath:
        if isexec (program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            exe_file = os.path.join(path, program)
            if isexec (exe_file):
                return exe_file
    return None

def kill (proc):
    try:
        proc.terminate ()
        proc.kill ()
        proc.wait ()
        global running_process
        running_process = None
    except OSError:
        pass

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
    p = a.ArgumentParser (description='Abstract Interpretation-based Analyzer for LLVM bitecode')
    p.add_argument ('-oll', dest='out_name', metavar='FILE',
                       help='Output file name')
    p.add_argument ("--save-temps", dest="save_temps",
                       help="Do not delete temporary files",
                       action="store_true",
                       default=False)
    p.add_argument ("--temp-dir", dest="temp_dir", metavar='DIR',
                       help="Temporary directory",
                       default=None)
    p.add_argument ('-O', type=int, dest='L', metavar='INT',
                       help='Optimization level L:[0,1,2,3]', default=3)
    p.add_argument ('-g', default=False, action='store_true', dest='debug_info',
                    help='Compile with debug information')
    p.add_argument ('-m', type=int, dest='machine',
                       help='Machine architecture MACHINE:[32,64]', default=32)
    p.add_argument ("--disable-cc", dest="no_cc", 
                       help=a.SUPPRESS, action='store_true',
                       default=False)
    p.add_argument ("--disable-pp", dest="no_pp", 
                       help=a.SUPPRESS, action='store_true',
                       default=False)
    p.add_argument ('--cpu', type=int, dest='cpu', metavar='SEC',
                       help='CPU time limit (seconds)', default=-1)
    p.add_argument ('--mem', type=int, dest='mem', metavar='MB',
                       help='MEM limit (MB)', default=-1)
    p.add_argument ('--inline', dest='inline', help='Inline all functions',
                    default=False, action='store_true')
    p.add_argument ('--pp-loops',
                    help='Preprocessing Loops',
                    dest='pp_loops', default=False, action='store_true')
    p.add_argument ('file', metavar='FILE', help='Input file')
    ### BEGIN CRAB
    p.add_argument ('--crab-dom',
                    help='Choose abstract domain',
                    choices=['int','ric','zones','term'],
                    dest='crab_dom', default='int')
    p.add_argument ('--crab-track',
                    help='Track integers, pointers, and memory',
                    choices=['int', 'ptr', 'arr'], dest='track', default='int')
    p.add_argument ('--crab-inter',
                    help='Run inter-procedural analysis',
                    dest='crab_inter', default=False, action='store_true')
    p.add_argument ('--crab-live',
                    help='Use of liveness information',
                    dest='crab_live', default=False, action='store_true')        
    p.add_argument ('--crab-devirt',
                    help='Resolve indirect calls',
                    dest='crab_devirt', default=False, action='store_true')
    p.add_argument ('--crab-add-invariants-at-entries',
                    help='Instrument code with invariants at each block entry',
                    dest='insert_invs_entries', default=False, action='store_true')
    p.add_argument ('--crab-add-invariants-after-loads',
                    help='Instrument code with invariants after each load instruction',
                    dest='insert_invs_loads', default=False, action='store_true')
    p.add_argument ('--crab-print-invariants',
                    help='Display computed invariants',
                    dest='show_invars', default=False, action='store_true')
    p.add_argument ('--crab-print-summaries',
                    help='Display computed summaries (if --crab-inter)',
                    dest='show_summs', default=False, action='store_true')
    p.add_argument ('--crab-print-cfg',
                    help='Display Crab CFG',
                    dest='print_cfg', default=False, action='store_true')
    ######################################################################
    p.add_argument ('--crab-disable-ptr',
                    #help='Disable translation of pointer arithmetic instructions (experimental)',
                    help=a.SUPPRESS,
                    dest='crab_disable_ptr', default=False, action='store_true')
    p.add_argument ('--crab-cfg-simplify',
                    #help='Simplify CFG built by Crab (experimental)',
                    help=a.SUPPRESS,
                    dest='crab_cfg_simplify', default=False, action='store_true')
    p.add_argument ('--crab-keep-shadows',
                    help=a.SUPPRESS,
                    dest='crab_keep_shadows', default=False, action='store_true')
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
        crabpp = os.path.join (root, "bin/crabllvmpp")
    if not isexec (crabpp): crabpp = which ('crabllvmpp')
    if not isexec (crabpp):
        raise IOError ("Cannot find crabllvm pre-processor")
    return crabpp

def getClang ():
    names = ['clang-mp-3.6', 'clang-3.6', 'clang', 'clang-mp-3.5', 'clang-mp-3.4']
    for n in names:
        clang = which (n)
        if clang is not None:
            return clang
    raise IOError ('Cannot find clang (required)')    

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

def defOutPPName (name, wd=None):
    base = os.path.basename (name)
    if wd == None: wd = os.path.dirname  (name)
    fname = os.path.splitext (base)[0] + '.out.pp.bc'
    return os.path.join (wd, fname)

# Run Clang
def clang (in_name, out_name, arch=32, extra_args=[]):
    if out_name == '' or out_name == None:
        out_name = defBCName (in_name)

    clang_args = [getClang (), '-emit-llvm', '-o', out_name, '-c', in_name ]
    clang_args.extend (extra_args)

    if verbose: print ' '.join (clang_args)
    sub.check_call (clang_args)

# Run crabpp
def crabpp (in_name, out_name, arch, args, extra_args=[]):
    if out_name == '' or out_name == None:
        out_name = defPPName (in_name)

    crabpp_args = [getCrabLlvmPP (), '-o', out_name, in_name ]
    if args.inline: 
        crabpp_args.append ('--crab-inline-all')
    if args.pp_loops: 
        crabpp_args.append ('--crab-pp-loops')

    crabpp_args.extend (extra_args)

    if verbose: print ' '.join (crabpp_args)
    sub.check_call (crabpp_args)

def sharedLib (base):
    ext = '.so'
    if sys.platform.startswith ('darwin'): ext = '.dylib'
    return base + ext

# Run crabllvm
def crabllvm (in_name, out_name, args, cpu = -1, mem = -1):
    def set_limits ():
        if mem > 0:
            mem_bytes = mem * 1024 * 1024
            resource.setrlimit (resource.RLIMIT_AS, [mem_bytes, mem_bytes])

    crabllvm_cmd = [ getCrabLlvm(), in_name, '-oll', out_name]

    crabllvm_cmd.append ('--crab-dom={0}'.format (args.crab_dom))
    crabllvm_cmd.append ('--crab-track-lvl={0}'.format (args.track))
    if args.crab_inter:
        crabllvm_cmd.append ('--crab-inter')
    if args.crab_devirt:
        crabllvm_cmd.append ('--crab-devirt')
    if args.crab_live:
        crabllvm_cmd.append ('--crab-live')
    if args.insert_invs_entries:
        crabllvm_cmd.append ('--crab-add-invariants-at-entries')
    if args.insert_invs_loads:
        crabllvm_cmd.append ('--crab-add-invariants-after-loads')
    if args.show_invars:
        crabllvm_cmd.append ('--crab-print-invariants')
    if args.show_summs:
        crabllvm_cmd.append ('--crab-print-summaries')
    if args.print_cfg:
        crabllvm_cmd.append ('--crab-print-cfg')

    # hidden options
    if args.crab_disable_ptr:
        crabllvm_cmd.append ('--crab-disable-ptr')
    if args.crab_cfg_simplify:
        crabllvm_cmd.append ('--crab-cfg-simplify')
    if args.crab_keep_shadows:
        crabllvm_cmd.append ('--crab-keep-shadows')


    if verbose: print ' '.join (crabllvm_cmd)

    p = sub.Popen (crabllvm_cmd, preexec_fn=set_limits)

    global running_process
    running_process = p

    timer = threading.Timer (cpu, kill, [p])
    if cpu > 0: timer.start ()

    try:
        (pid, returnvalue, ru_child) = os.wait4 (p.pid, 0)
        running_process = None
    finally:
        ## kill the timer if the process has terminated already
        if timer.isAlive (): timer.cancel ()

    ## if crabllvm did not terminate properly, propagate this error code
    if returnvalue != 0: sys.exit (returnvalue)


def stat (key, val): stats.put (key, val)
def main (argv):
    os.setpgrp ()
    loadEnv (os.path.join (root, "env.common"))

    ## add directory containing this file to the PATH
    os.environ ['PATH'] =  os.path.dirname (os.path.realpath (__file__)) + \
                           os.pathsep + os.environ['PATH']
    
    args  = parseArgs (argv[1:])
    workdir = createWorkDir (args.temp_dir, args.save_temps)
    in_name = args.file

    if not args.no_cc:
        bc_out = defBCName (in_name, workdir)
        if bc_out != in_name:
            with stats.timer ('Clang'):
                extra_args = []
                if args.debug_info:
                    extra_args.append ('-g')
                clang (in_name, bc_out, arch=args.machine, extra_args=extra_args)
            stat ('Progress', 'CLANG')
        in_name = bc_out

    if not args.no_pp:
        pp_out = defPPName (in_name, workdir)
        if pp_out != in_name:
            with stats.timer ('CrabLlvmPP'):
                crabpp (in_name, pp_out, arch=args.machine, args=args)
            stat ('Progress', 'CRABLLVMPP')
        in_name = pp_out

    pp_out = defOutPPName(in_name, workdir)
    with stats.timer ('CrabLlvm'):
        crabllvm (in_name, pp_out, args, cpu=args.cpu, mem=args.mem)
    stat ('Progress', 'CRABLLVM')

    if args.out_name is not None and args.out_name != pp_out:
        if verbose: print 'cp {0} {1}'.format (pp_out, args.out_name)
        shutil.copy2 (pp_out, args.out_name)

    return 0

def killall ():
    global running_process
    if running_process != None:
        running_process.terminate ()
        running_process.kill ()
        running_process.wait ()
        running_process = None

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

