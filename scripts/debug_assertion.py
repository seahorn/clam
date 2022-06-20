#!/usr/bin/env python3

import sys
import os
import os.path
import re
import tempfile
import shutil

#------------------------------------------------------------------------------#
# Slice a CrabIR program containing invariants to show only relevant
# information to an assertion.
#
# This is useful when an assertion cannot be proven and we want to see
# only the part of the invariants that are relevant to the assertion.
# 
# This script simply processes a CrabIR file so everything done is
# fragile because it depends on how Clam/Crab prints invariants which
# can change from time to time or from option to option.
#
# Usage:
#   clam.py -g prog.c --crab-track=mem --crab-inter --crab-check=assert \
#                     --crab-print-invariants=true --crab-print-voi=true \
#                     --log=region-print-debug \
#                     --ocrab=prog.crabir MORE_OPTS
#   debug_assertion.py --assertion=NUM prog.crabir
#
# All the above options passed to clam.py are needed. First, the
# program must be compiled with debug symbols. Second, we want clam.py
# to print invariants (--crab-print-invariants) and the set of
# variables of interest (voi) for each assertion. A variable v is of
# interest for an assertion s, if the reachability of s depends on the
# evaluation of v. The computation of set of voi's is done by a
# dataflow analysis that requires interprocedural analysis
# (--crab-inter). Finally, this script assumes that invariants are
# printed in a certain form. The option value --crab-track=mem
# produces usually the most precise results so that this script only
# understands the invariants printed by that option value.
# ------------------------------------------------------------------------------#

abortIfInvariantsNotProcessed = True

# return [v1,v2,...] from string "{v1;v2;...}"
def extractVariables(varset_str):
    if varset_str == "{}":
        return list()
    # remove first and last characters: '{' and '}:
    varlist = varset_str[1:-1].split(';')
    return [v.strip() for v in varlist]

# TOIMPROVE: less domain dependent
# Assume that the invariants are printed by the Crab region
# domain. Moreover, it assumes that all the subdomains of the region
# domain are environment domains and thus, invariants are printed of
# the form x -> Val.

# Environment domains are always printed in the same way.
def processEnvironmentDomain(variables, envInvariant):
    res = list()
    for v in variables:
            envInvariantSliced = 'top'
            variableRegex = v + "\s->\s([^;\}]*)"
            m = re.search(variableRegex, envInvariant)
            if m is not None:
                envInvariantSliced = m.group(1)
                res.append(v + " -> " + envInvariantSliced)
    if len(res) == 0:
        #print("Not found variables="+ str(variables) + " in " + envInvariant + "\n")
        return 'top'
    else:
        return ','.join(res)

# The base domain used by the region domain can be configurable and
# thus, it can be printed differently.  The most common will be a
# product of two domains ({...},{...}) or a product of three domains
# ({...},({...},{...})).
def processRegionBaseDomain(variables, regionInvariant):
    # HACK: top can be printed by {} or "top"
    if regionInvariant == "{}":
        return "{}"
    if regionInvariant == "top":
        return "top"
    
    pattern = re.compile(r"\(\{(.*)\},\s\{(.*)\}\)")
    m = re.search(pattern, regionInvariant)
    if m is not None:
        boolEnvDom = m.group(1)
        numEnvDom = m.group(2)
        e1 = processEnvironmentDomain(variables, boolEnvDom)
        e2 = processEnvironmentDomain(variables, numEnvDom)
        return "({{{0}}},{{{1}}})".format(e1,e2)
    else:
        pattern = re.compile(r"\(\{(.*)\},\s\(\{(.*)\},\s\{(.*)\}\)\)")
        m = re.search(pattern, regionInvariant)
        if m is not None:
            boolEnvDom = m.group(1)
            numEnvDom1 = m.group(2)
            numEnvDom2 = m.group(3)
            e1 = processEnvironmentDomain(variables, boolEnvDom)
            e2 = processEnvironmentDomain(variables, numEnvDom1)
            e3 = processEnvironmentDomain(variables, numEnvDom2)            
            return "({{{0}}},{{{1}}},{{{2}}})".format(e1,e2,e3)        
    return None

def cannotProcessInvariants(variables, invariants):
    if abortIfInvariantsNotProcessed:
        print("ERROR: could not slice invariants {0} onto variables {1}".format(invariants, variables))
        sys.exit(1)
    else:
        return invariants

def sliceInvariants(variables, invariants):
    if invariants == "_|_":
        return invariants

    pattern = re.compile(r"\(RgnCounter=\{(.*)\},BaseDom=(.*)\)")
    m = re.search(pattern, invariants)
    if m is not None:
        counterDom = m.group(1)
        baseDom = m.group(2)
        if counterDom == "" and \
           (baseDom == "({}, ({}, {}))" or baseDom == "({}, {})"):
            return invariants

        counterDomSliced = processEnvironmentDomain(variables, counterDom)
        baseDomSliced = processRegionBaseDomain(variables, baseDom)
        if baseDomSliced is not None:
            return "(RgnCounter={{{0}}},BaseDom={{{1}}})".format(counterDomSliced, baseDomSliced)
        else:
            return cannotProcessInvariants(variables, invariants)
    else:        
        # If here we assume that no information about RgnCounter has been
        # printed and therefore, only the base domain is being printed). 
        baseDomSliced = processRegionBaseDomain(variables, invariants)
        if baseDomSliced is not None:
            return baseDomSliced
        else:
            return cannotProcessInvariants(variables, invariants)

def processLog(infile, outfile, assertionId, verbosity):
    #print("Verbosity level=" + str(verbosity))
    #print(assertionId)
    
    infd, outfd = None, None
    
    try:
        infd = open(infile, 'r')
    except Exception as e:
        #print(e)        
        sys.exit('ERROR: could not open {}'.format(infile))
    try:
        outfd = open(outfile, 'w')
    except Exception as e:
        # print(e)
        sys.exit('ERROR: could not open {}'.format(outfile))


    begin_annotation = re.compile(r"\/\*\*[^\*]*")
    end_annotation = re.compile(r"[^\*]*\*\*\/")    
    annotation_pattern = re.compile(r"\/\*\*([^\*]*)")
    ##the 1st capturing group is just the invariant as printed by the
    ##abstract state and the 2nd capturing group is has multiple lines
    ##of the form \svoi\((.*)\)={(.*)}
    invariants_and_voi_regex = re.compile(r"\s*INVARIANTS:\s*(.*)\s*VARIABLES-OF-INFLUENCE:((.|\n)*)")
    assertion_regex = "\s(assert\(.*\)\sloc\(.*\)\sid=" + str(assertionId) + ")\s({.*})"
    annotation = ""
    while True:
        line = infd.readline()
        if not line: break

        # when /** and **/ are found in the same  line
        if re.search(re.compile(r"\/\*\*[^\*]*\*\*\/"), line) is not None:
            annotation += line
        # when /** and **/ are in different lines    
        elif re.search(begin_annotation, line) is not None:
            annotation += line
            continue
        elif re.search(end_annotation, line) is not None:
            annotation += line
        elif annotation != "":
            annotation += line
            continue
        else:
            outfd.write(line)
            continue

        processed_annotation = annotation
        annotation = ""
        
        # found end of the annotation: process the annotation

        matched_invariant = ""
        matched_assertion = ""

        m = re.search(annotation_pattern, processed_annotation)
        if m is None:
            # this shouldn't happen
            outfd.write(line)
            continue

        # removed /** and **/ 
        processed_annotation = m.group(1)

        m = re.search(invariants_and_voi_regex, processed_annotation)
        if m is None:
            #outfd.write(line)
            #
            if verbosity == 1:
                print("warning: could not extract invariant followed by variables of influence")
            elif verbosity > 1:
                print("warning: could not extract invariant followed by variables of influence in {0}".format(processed_annotation))
            continue

        # extract invariants and voi annotations
        matched_invariant  = m.group(1)
        matched_assertions = m.group(2)

        m = re.search(assertion_regex, matched_assertions)
        if m is None:
            #outfd.write(line)
            if verbosity == 1:
                print("warning: could not find pattern {0} from annotations".format(assertion_regex))
            elif verbosity > 1:
                print("warning: could not find pattern {0} from annotations in {1}".format(assertion_regex, matched_assertions))   
            continue;

        matched_assertion = m.group(1)
        matched_voi = m.group(2)

        #print("Annotation=" + processed_annotation + "\n")
        #print("Matched voi=" + matched_voi + "\n")
        #print("Matched invariant=" + matched_invariant + "\n")

    
        ## simplify invariants
        voi = extractVariables(matched_voi)
        sliced_invariant = sliceInvariants(voi, matched_invariant)

        #print("Sliced invariant="+ sliced_invariant)
        outfd.write("/**\n    ")
        outfd.write(matched_assertion)
        outfd.write(" variables-of-influence={0}\n".format(matched_voi))
        outfd.write("    Invariant: {0}\n".format(sliced_invariant))
        outfd.write("**/\n")
              
    infd.close()
    outfd.close()
        
def parseOpt (argv):
    import argparse as a
    p = a.ArgumentParser(description='Slice a CrabIR program to show only relevant information wrt an assertion',
                         formatter_class=a.RawTextHelpFormatter)
    p.add_argument(metavar='FILE', dest="infile", help="Log file"),
    p.add_argument('-assertion','--assertion', metavar='NUM', required=True,
                   type=int,  dest="assertion",  default=0,
                   help="Numerical identifier that refers to assertion of interest.\n"
                   "E.g., if we are interested in debugging why\n"
                   "  assert(tmp42 > NULL_REF) /* loc(file=commands.c line=427 col=15) id=13 */;\n"
                   "cannot be proven then pass the option --assertion=13\n")
    p.add_argument('-verbose','--verbose', metavar='NUM', required=False,
                   type=int,  dest="verbose",  default=0,
                   help="Verbosity level")
    args = p.parse_args(argv)
    return args

def main (argv):
    args = parseOpt(argv[1:])
    (root, ext) = os.path.splitext(os.path.abspath(args.infile))
    outfile = "{0}.sliced{1}".format(root, ext)
    processLog(args.infile, outfile, args.assertion, args.verbose)
    return 0
    
if __name__ == '__main__':
    res = None
    try:
        res = main(sys.argv)
    except Exception as e:
        print(e)
    except KeyboardInterrupt:
        pass
    finally:
        sys.exit(res)
