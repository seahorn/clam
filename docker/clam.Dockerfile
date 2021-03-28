# Dockerfile for Clam binary
# produces package in /clam/build
# Arguments:
#  - BASE-IMAGE: bionic-llvm10, focal-llvm10
#  - BUILD_TYPE: Debug, RelWithDebInfo, Coverage
#  - BRANCH

ARG BASE_IMAGE=bionic-llvm10
FROM seahorn/buildpack-deps-seahorn:$BASE_IMAGE

# Needed to run clang with -m32
RUN apt-get install -yqq libc6-dev-i386
# seadsa needs clang-format
RUN apt-get install -yqq clang-format

ARG BRANCH=master
RUN cd / && rm -rf /clam && \
    git clone https://github.com/seahorn/crab-llvm -b $BRANCH clam --depth=10 ; \
    mkdir -p /clam/build
WORKDIR /clam/build

ARG BUILD_TYPE=RelWithDebInfo
# Build configuration.
RUN cmake .. -GNinja \
          -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
          -DCMAKE_INSTALL_PREFIX=run \
          -DCMAKE_CXX_COMPILER=clang++-10 \
	  -DCMAKE_C_COMPILER=clang-10 \	  
          -DCMAKE_EXPORT_COMPILE_COMMANDS=1 \
          -DCRAB_USE_LDD=ON \
          -DCRAB_USE_APRON=ON \
          && \
    cmake --build . --target extra  && cmake .. && \
    cmake --build . --target crab  && cmake .. && \
    cmake --build . --target ldd  && cmake .. && \
    cmake --build . --target apron  && cmake .. && \
    cmake --build . --target install

RUN ln -s /usr/bin/clang-10 /usr/bin/clang
RUN ln -s /usr/bin/llvm-dis-10 /usr/bin/llvm-dis
ENV PATH "/usr/bin:$PATH"
ENV PATH "/clam/build/run/bin:$PATH"

# run tests
RUN cmake --build . --target test-simple
RUN cmake --build . --target test-readme
RUN cmake --build . --target test-ssh-simplified
RUN cmake --build . --target test-ntdrivers-simplified
RUN cmake --build . --target test-array-adapt
RUN cmake --build . --target test-mem

WORKDIR /clam

