# Dockerfile for Clam binary
# produces package in /clam/build
#
# Arguments:
#  - LLVM_VERSION: major LLVM release to build against (e.g. 15, 16, 17). Drives
#    the base image, the clang/clang++/llvm-dis binaries and the tool symlinks.
#  - BUILDPACK_IMAGE: buildpack-deps image repo. LLVM 15+ base images are
#    published on GitHub Container Registry; the older Docker Hub
#    seahorn/buildpack-deps-seahorn only goes up to jammy-llvm14.
#  - BASE_IMAGE: buildpack tag (defaults to jammy-llvm${LLVM_VERSION})
#  - BUILD_TYPE: Debug, RelWithDebInfo, Coverage
ARG LLVM_VERSION=15
ARG BUILDPACK_IMAGE=ghcr.io/seahorn/buildpack-deps-seahorn
ARG BASE_IMAGE=jammy-llvm${LLVM_VERSION}
FROM ${BUILDPACK_IMAGE}:${BASE_IMAGE}

# ARGs declared before FROM are only visible to FROM; re-declare to use below.
ARG LLVM_VERSION

# libc6-dev-i386: needed to run clang with -m32.
# python3-pip: to install OutputCheck below (the lit tests pipe into it).
RUN apt-get install -yqq libc6-dev-i386 python3-pip

# Assume that docker-build is ran in the top-level Clam directory
COPY . /clam
# Re-create the build directory that might have been present in the source tree
RUN rm -rf /clam/build /clam/debug /clam/release && \
  mkdir /clam/build && \
# Remove any third-party dependencies that build process clones
  rm -rf /clam/crab /clam/sea-dsa /clam/llvm-seahorn
WORKDIR /clam/build

ARG BUILD_TYPE=Release

# Build configuration.
RUN cmake .. -GNinja \
          -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
          -DCMAKE_INSTALL_PREFIX=run \
          -DCMAKE_CXX_COMPILER=clang++-${LLVM_VERSION} \
          -DCMAKE_C_COMPILER=clang-${LLVM_VERSION} \
          -DCMAKE_EXPORT_COMPILE_COMMANDS=1 \
          -DCRAB_USE_APRON=ON \
          && \
    cmake --build . --target extra  && cmake .. && \
    cmake --build . --target crab  && cmake .. && \
    cmake --build . --target apron  && cmake .. && \
    cmake --build . --target install

RUN ln -s /usr/bin/clang-${LLVM_VERSION} /usr/bin/clang
RUN ln -s /usr/bin/llvm-dis-${LLVM_VERSION} /usr/bin/llvm-dis
ENV PATH "/usr/bin:$PATH"
ENV PATH "/clam/build/run/bin:$PATH"

# lit is provided by the base image, but OutputCheck (which the lit RUN
# directives pipe into) is not; install it.
RUN pip3 install --no-cache-dir OutputCheck

# run tests
RUN cmake --build . --target test-simple
RUN cmake --build . --target test-readme
RUN cmake --build . --target test-inter
RUN cmake --build . --target test-array-adapt
RUN cmake --build . --target test-mem
RUN cmake --build . --target test-devirt
RUN cmake --build . --target test-opt
#RUN cmake --build . --target test-ssh-simplified
#RUN cmake --build . --target test-ntdrivers-simplified

WORKDIR /clam
