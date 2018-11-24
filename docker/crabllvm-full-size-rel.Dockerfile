#
# Dockerfile for CrabLlvm binary
# produces package in /crabllvm/build
# Arguments:
#  - UBUNTU:     trusty, xenial
#  - BUILD_TYPE: debug, release
#

ARG UBUNTU

# Pull base image.
FROM seahorn/seahorn-build-llvm5:$UBUNTU

RUN cd / && rm -rf /crabllvm && \
    git clone https://github.com/seahorn/crab-llvm -b dev-llvm-5.0 crabllvm --depth=10 ; \
    mkdir -p /crabllvm/build
WORKDIR /crabllvm/build

ARG BUILD_TYPE
# Build configuration.
RUN cmake -GNinja \
          -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
          -DBOOST_ROOT=/deps/boost \
          -DLLVM_DIR=/deps/LLVM-5.0.2-Linux/lib/cmake/llvm \
          -DCMAKE_INSTALL_PREFIX=run \
          -DCMAKE_CXX_COMPILER=g++-5 \
          -DCMAKE_EXPORT_COMPILE_COMMANDS=1 \
          -DUSE_LDD=ON \
          -DUSE_APRON=ON \
          ../ && \
    cmake --build . --target extra  && cmake .. && \
    cmake --build . --target crab  && cmake .. && \
    cmake --build . --target ldd  && cmake .. && \
    cmake --build . --target apron  && cmake .. && \
    cmake --build . --target install

# symlink clang (from base image)
RUN ln -s /clang-5.0/bin/clang run/bin/clang
RUN ln -s /clang-5.0/bin/clang++ run/bin/clang++

ENV PATH "/deps/LLVM-5.0.2-Linux/bin:$PATH"
ENV PATH "/crabllvm/build/run/bin:$PATH"

# run tests
RUN cmake --build . --target test-simple
RUN cmake --build . --target test-readme
RUN cmake --build . --target test-ssh-simplified
RUN cmake --build . --target test-ntdrivers-simplified


WORKDIR /crabllvm

