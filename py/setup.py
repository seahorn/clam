#!/usr/bin/env python2

from distutils.core import setup
from os.path import join

scripts = ['crabllvm.py']
scripts = map(lambda x: join('${CMAKE_CURRENT_SOURCE_DIR}', x), scripts)

setup(name='crabllvm',
      description='An Abstract Interpretation-based Analyzer for LLVM bitecode',
      url='https://github.com/seahorn/crab-llvm',
      package_dir={'': '${CMAKE_CURRENT_SOURCE_DIR}'},
      py_modules=['stats'],
      scripts=scripts
      )
