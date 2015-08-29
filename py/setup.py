from distutils.core import setup
from os.path import join

scripts = ['llvmikos.py']
scripts = map(lambda x: join('${CMAKE_CURRENT_SOURCE_DIR}', x), scripts)

setup(name='llvmikos',
      version='1.0',
      description='An Abstract Interpretation-based Analyzer for LLVM bitecode (python)',
      author='Jorge A. Navas',
      url='https://github.com/seahorn/ikos-llvm',
      license='TBD',
      package_dir={'': '${CMAKE_CURRENT_SOURCE_DIR}'},
      py_modules=['stats'],
      scripts=scripts
      )
