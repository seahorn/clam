# Building CrabLlvm's dependencies with Docker

LLVM 8.0 can be prebuilt using docker. E.g.
```shell
cd deps/llvm
docker build --build-arg UBUNTU=xenial --build-arg BUILD_TYPE=Release -t llvm_xenial_rel .
docker run -v $(pwd):/host -it llvm_xenial_rel
```
This will automatically create a llvm.tar file in the current working directory.

For all the dependencies, the possible build arguments are:
- UBUNTU: trusty, xenial
- BUILD_TYPE: Release, Debug

Note that both `UBUNTU` and `BUILD_TYPE` are required arguments.
