#!/usr/bin/env python3

# This script checks that a clam run reports the expected number of safe, error, and
# warning checks which arguments of the script.
#
# If the run reports the expected number then the script returns 0, else 1.
# 
# This script is useful for tools such as llvm-reduce.

import os
import os.path
import sys
import subprocess as sub

Verbose = True

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

def parseArgs (argv):
    import argparse as a
    p = a.ArgumentParser(description='Output checker')
    p.add_argument ('--file', metavar='FILE', dest='file', help='LLVM program')
    p.add_argument ('-safe', '--safe', metavar='UINT', dest='safe', type=int, default=0, help='Number of expected safe checks')
    p.add_argument ('-err', '--err', metavar='UINT', dest='err', type=int, default=0, help='Number of expected error checks')
    p.add_argument ('-warn', '--warn', metavar='UINT', dest='warn', type=int, default=0, help='Number of expected warning checks')
    p.add_argument ('-keep-files', '--keep-files', dest='keep_files', default=False, action='store_true',
                    help='Keep temporary file created to capture standard output')
    
    # default output directory includes a date
    #import datetime as dt
    #dt = dt.datetime.now ().strftime ('%d_%m_%Y-t%H-%M-%S')
    #def_out = 'out.{dt}'.format (dt=dt)
    #p.add_argument ('--outdir', metavar='DIR',
    #                default=def_out, help='Output directory')

    if '-h' in argv or '--help' in argv:
        p.print_help ()
        p.exit (0)

    try:
        k = argv.index ('--')
    except ValueError:
        p.error ("No '--' argument")

    args = p.parse_args (argv[:k])
    args.tool_args = argv[k+1:]
    return args

def processOutput(outfile, safe, err, warn):
    safe_str = str(safe)
    err_str = str(err)
    warn_str = str(warn)

    safe_len = len(safe_str)
    err_len = len(err_str)
    warn_len = len(warn_str)
    
    max_len = max(safe_len, err_len, warn_len)
    
    safe_str = safe_str.rjust(safe_len + (max_len - safe_len), ' ')
    err_str = err_str.rjust(err_len + (max_len - err_len), ' ')
    warn_str = warn_str.rjust(warn_len + (max_len - warn_len), ' ')    
    
    pattern = """
{0}  Number of total safe checks
{1}  Number of total error checks
{2}  Number of total warning checks
""".format(safe_str, err_str, warn_str)

    content = ""
    with open(outfile, 'r') as out_fd:
        content = out_fd.read()
        
    if pattern in content:
        return 0
    else:
        return 1
        
def runTool(tool_args, f):
    fmt_tool_args = [v.format(f=f) for v in tool_args]
    tool = fmt_tool_args[0]
    fmt_tool_args[0] = which(tool)
    if fmt_tool_args[0] is None:
        print("Error: {0} not found".format(tool))
        return None

    fmt_tool_args.append(f)
    base = os.path.basename(f)
    outfile = os.path.join(base + '.stdout')

    if Verbose: print(' '.join(fmt_tool_args))

    with open(outfile, 'w') as out_fd:
        p = sub.Popen(fmt_tool_args, stdout=out_fd)
        p.communicate()
    
    return outfile

def main(argv):
    args = parseArgs (argv[1:])
    
    # run tool    
    outfile = runTool(args.tool_args, args.file)
    if outfile is None:
        return 1
    else:
        # process output
        returncode = processOutput(outfile, args.safe, args.err, args.warn)
        if not args.keep_files:
            os.remove(outfile)
        return returncode


if __name__ == '__main__':
    sys.exit (main (sys.argv))

