# Dockerfile for Crab

# OS image
FROM ubuntu:14.04

MAINTAINER Temesghen Kahsai <lememta@gmail.com>

ENV PATH="/home/seahorn/crab-llvm/build/run/bin:$PATH"

# Install
RUN \
  sed -i 's/# \(.*multiverse$\)/\1/g' /etc/apt/sources.list && \
  apt-get update -qq && \
  apt-get upgrade -qq && \
  apt-get install -qq build-essential clang-3.6 && \
  apt-get install -qq software-properties-common && \
  apt-get install -qq curl git ninja-build man subversion vim wget cmake && \
  apt-get install -qq libboost-program-options-dev && \
  apt-get install python2.7 python2.7-dev -y && \
  apt-get install -y libboost1.55-all-dev && \
  apt-get install --yes libgmp-dev

WORKDIR /home/crab

# Install Docker
RUN git clone https://github.com/seahorn/crab-llvm.git 
RUN \
   cd crab-llvm && \
   mkdir build && \
   cd build && \
   cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=run ../ && \
   cmake --build . && \
   cmake --build . /home/crab/crab-llvm && \
   cmake --build . && \
   cmake --build . /home/crab/crab-llvm && \
   cmake --build . --target install  
  
   
