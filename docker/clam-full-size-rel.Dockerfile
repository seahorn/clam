#
# Dockerfile for Clam binary
# produces package in /clam/build
# Arguments:
#  - UBUNTU:     trusty, xenial, bionic
#  - BUILD_TYPE: debug, release
#

ARG UBUNTU

# Pull base image.
FROM seahorn/clam-build-llvm8:$UBUNTU

# Needed to run clang with -m32
RUN apt-get update && \
    apt-get install -yqq libc6-dev-i386 

RUN cd / && rm -rf /clam && \
    git clone https://github.com/seahorn/crab-llvm -b llvm-8.0 clam --depth=10 ; \
    mkdir -p /clam/build
WORKDIR /clam/build

ARG BUILD_TYPE
# Build configuration.
RUN cmake -GNinja \
          -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
          -DBOOST_ROOT=/deps/boost \
          -DLLVM_DIR=/deps/LLVM-8.0.1-Linux/lib/cmake/llvm \
          -DCMAKE_INSTALL_PREFIX=run \
          -DCMAKE_CXX_COMPILER=g++-5 \
          -DCMAKE_EXPORT_COMPILE_COMMANDS=1 \
          -DCRAB_USE_LDD=ON \
          -DCRAB_USE_APRON=ON \
          ../ && \
    cmake --build . --target extra  && cmake .. && \
    cmake --build . --target crab  && cmake .. && \
    cmake --build . --target ldd  && cmake .. && \
    cmake --build . --target apron  && cmake .. && \
    cmake --build . --target install

# symlink clang (from base image)
RUN ln -s /clang-8.0/bin/clang run/bin/clang
RUN ln -s /clang-8.0/bin/clang++ run/bin/clang++

ENV PATH "/deps/LLVM-8.0.1-Linux/bin:$PATH"
ENV PATH "/clam/build/run/bin:$PATH"

# run tests
RUN cmake --build . --target test-simple
RUN cmake --build . --target test-readme
RUN cmake --build . --target test-ssh-simplified
RUN cmake --build . --target test-ntdrivers-simplified


WORKDIR /clam

