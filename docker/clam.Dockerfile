# Dockerfile for Clam binary
# produces package in /clam/build

# Arguments:
#  - BASE-IMAGE: jammy-llvm14
#  - BUILD_TYPE: Debug, RelWithDebInfo, Coverage
ARG BASE_IMAGE=jammy-llvm14
FROM seahorn/buildpack-deps-seahorn:$BASE_IMAGE

# Needed to run clang with -m32
RUN apt-get install -yqq libc6-dev-i386

#ARG BRANCH=dev14
#RUN cd / && rm -rf /clam && \
#    git clone -b $BRANCH https://github.com/seahorn/clam.git clam --depth=10 ; \
#    mkdir -p /clam/build

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
          -DCMAKE_CXX_COMPILER=clang++-14 \
	  -DCMAKE_C_COMPILER=clang-14 \	  
          -DCMAKE_EXPORT_COMPILE_COMMANDS=1 \
          -DCRAB_USE_APRON=ON \
          && \
    cmake --build . --target extra  && cmake .. && \
    cmake --build . --target crab  && cmake .. && \
    cmake --build . --target apron  && cmake .. && \
    cmake --build . --target install

RUN ln -s /usr/bin/clang-14 /usr/bin/clang
RUN ln -s /usr/bin/llvm-dis-14 /usr/bin/llvm-dis
ENV PATH "/usr/bin:$PATH"
ENV PATH "/clam/build/run/bin:$PATH"

# run tests
RUN cmake --build . --target test-simple
RUN cmake --build . --target test-readme
RUN cmake --build . --target test-inter
RUN cmake --build . --target test-array-adapt
RUN cmake --build . --target test-mem
#RUN cmake --build . --target test-ssh-simplified
#RUN cmake --build . --target test-ntdrivers-simplified

WORKDIR /clam

