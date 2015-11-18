from distutils.core import setup
from os.path import join

scripts = ['crabllvm.py']
scripts = map(lambda x: join('${CMAKE_CURRENT_SOURCE_DIR}', x), scripts)

setup(name='crabllvm',
      version='1.0',
      description='An Abstract Interpretation-based Analyzer for LLVM bitecode (python)',
      author='Jorge A. Navas',
      url='https://github.com/seahorn/crab-llvm',
      license='MIT',
      package_dir={'': '${CMAKE_CURRENT_SOURCE_DIR}'},
      py_modules=['stats'],
      scripts=scripts
      )
