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
    p.add_argument ('--cpu', type=int, dest='cpu', metavar='SEC',
                       help='CPU time limit (seconds)', default=-1)
    p.add_argument ('--mem', type=int, dest='mem', metavar='MB',
                       help='MEM limit (MB)', default=-1)
    p.add_argument ('--inline', dest='inline', help='Inline all functions',
                    default=False, action='store_true')
    p.add_argument ('file', metavar='FILE', help='Input file')
    ### BEGIN IKOS
    p.add_argument ('--ikos-dom',
                    help='Choose IKOS abstract domain',
                    choices=['int','ric','zones','term'],
                    dest='ikos_dom', default='int')
    p.add_argument ('--ikos-track',
                    help='Track registers, pointers, and memory',
                    choices=['reg', 'ptr', 'mem'], dest='track', default='reg')
    p.add_argument ('--ikos-answer',
                    help='Display computed invariants',
                    dest='show_invars', default=False, action='store_true')
    p.add_argument ('--ikos-print-cfg',
                    help='Print Ikos CFG',
                    dest='print_cfg', default=False, action='store_true')
    p.add_argument ('--ikos-insert-invs',
                    help='Instrument code with invariants',
                    dest='insert_invs', default=False, action='store_true')
    p.add_argument ('--ikos-disable-ptr',
                    help='Disable translation of pointer arithmetic instructions',
                    dest='ikos_disable_ptr', default=False, action='store_true')
    p.add_argument ('--ikos-cfg-simplify',
                    help='Simplify CFG built by Ikos (experimental)',
                    dest='ikos_cfg_simplify', default=False, action='store_true')
    p.add_argument ('--ikos-live',
                    help='Use of liveness information (experimental)',
                    dest='ikos_live', default=False, action='store_true')        
    p.add_argument ('--ikos-cfg-interproc',
                    help='Build inter-procedural CFG (experimental)',
                    dest='ikos_interproc', default=False, action='store_true')
    #### END IKOS
    
    args = p.parse_args (argv)
    
    if args.L < 0 or args.L > 3:
        p.error ("Unknown option: -O%s" % args.L)

    if args.machine != 32 and args.machine != 64:
        p.error ("Unknown option -m%s" % args.machine)

    return args

def createWorkDir (dname = None, save = False):
    if dname is None:
        workdir = tempfile.mkdtemp (prefix='llvmikos-')
    else:
        workdir = dname

    if verbose:
        print "Working directory", workdir

    if not save:
        atexit.register (shutil.rmtree, path=workdir)
    return workdir

def getLlvmIkos ():
    llvmikos = None
    if 'LLVMIKOS' in os.environ: llvmikos = os.environ ['LLVMIKOS']
    if not isexec (llvmikos):
        llvmikos = os.path.join (root, "bin/llvmikos")
    if not isexec (llvmikos): llvmikos = which ('llvmikos')
    if not isexec (llvmikos):
        raise IOError ("Cannot find llvmikos")
    return llvmikos

def getLlvmPP ():
    llvmpp = None
    if 'LLVMPP' in os.environ:
        llvmpp = os.environ ['LLVMPP']
    if not isexec (llvmpp):
        llvmpp = os.path.join (root, "bin/llvmpp")
    if not isexec (llvmpp): llvmpp = which ('llvmpp')
    if not isexec (llvmpp):
        raise IOError ("Cannot find llvmikos pre-processor")
    return llvmpp

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

# Run llvmpp
def llvmpp (in_name, out_name, arch, args, extra_args=[]):
    if out_name == '' or out_name == None:
        out_name = defPPName (in_name)

    llvmpp_args = [getLlvmPP (), '-o', out_name, in_name ]
    if args.inline: llvmpp_args.append ('--ikos-inline-all')
    llvmpp_args.extend (extra_args)

    if verbose: print ' '.join (llvmpp_args)
    sub.check_call (llvmpp_args)

def sharedLib (base):
    ext = '.so'
    if sys.platform.startswith ('darwin'): ext = '.dylib'
    return base + ext

# Run llvmikos
def llvmikos (in_name, out_name, args, cpu = -1, mem = -1):
    def set_limits ():
        if mem > 0:
            mem_bytes = mem * 1024 * 1024
            resource.setrlimit (resource.RLIMIT_AS, [mem_bytes, mem_bytes])

    llvmikos_cmd = [ getLlvmIkos(), in_name,
                     '-oll', out_name]

    llvmikos_cmd.append ('--ikos-dom={0}'.format (args.ikos_dom))
    llvmikos_cmd.append ('--ikos-track-lvl={0}'.format (args.track))

    if args.ikos_disable_ptr:
        llvmikos_cmd.append ('--ikos-disable-ptr')
    if args.ikos_cfg_simplify:
        llvmikos_cmd.append ('--ikos-cfg-simplify')
    if args.ikos_interproc:
        llvmikos_cmd.append ('--ikos-cfg-interproc')
    if args.ikos_live:
        llvmikos_cmd.append ('--ikos-live')
    if args.show_invars:
        llvmikos_cmd.append ('--ikos-answer')
    if args.print_cfg:
        llvmikos_cmd.append ('--ikos-print-cfg')
    if args.insert_invs:
        llvmikos_cmd.append ('--ikos-insert-invs')

    if verbose: print ' '.join (llvmikos_cmd)

    p = sub.Popen (llvmikos_cmd, preexec_fn=set_limits)

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

    ## if llvmikos did not terminate properly, propagate this error code
    if returnvalue != 0: sys.exit (returnvalue)


def stat (key, val): stats.put (key, val)
def main (argv):
    stat ('Progress', 'UNKNOWN')
    os.setpgrp ()
    loadEnv (os.path.join (root, "env.common"))

    ## add directory containing this file to the PATH
    os.environ ['PATH'] =  os.path.dirname (os.path.realpath (__file__)) + \
                           os.pathsep + os.environ['PATH']
    
    args  = parseArgs (argv[1:])

    workdir = createWorkDir (args.temp_dir, args.save_temps)

    in_name = args.file

    bc_out = defBCName (in_name, workdir)
    if bc_out != in_name:
        with stats.timer ('Clang'):
            extra_args = []
            if args.debug_info:
                extra_args.append ('-g')
            clang (in_name, bc_out, arch=args.machine, extra_args=extra_args)
        stat ('Progress', 'CLANG')

    in_name = bc_out

    pp_out = defPPName (in_name, workdir)
    if pp_out != in_name:
        with stats.timer ('LlvmPP'):
            llvmpp (in_name, pp_out, arch=args.machine, args=args)
        stat ('Progress', 'LLVMPP')

    in_name = pp_out

    final_pp_out = defOutPPName(in_name, workdir)
    with stats.timer ('LlvmIkos'):
        llvmikos (in_name, final_pp_out, args, cpu=args.cpu, mem=args.mem)
    stat ('Progress', 'LLVMIKOS')

    if args.out_name is not None and args.out_name != final_pp_out:
        if verbose: print 'cp {0} {1}'.format (final_pp_out, args.out_name)
        shutil.copy2 (final_pp_out, args.out_name)

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

