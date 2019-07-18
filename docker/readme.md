# Building CrabLlvm with Docker and running tests

## crabllvm-build.Dockerfile (downloading build dependencies):
```shell
docker build --build-arg UBUNTU=xenial --build-arg BUILD_TYPE=Release -t seahorn/crabllvm-build-llvm8:xenial -f docker/crabllvm-build.Dockerfile .
```
The above generates a container with all dependencies required to
build CrabLlvm, other than CrabLlvm itself.

Build arguments (required):
- UBUNTU: trusty, xenial
- BUILD_TYPE: Release, Debug

Depends on: `buildpack-deps`
Dockerhub: `docker pull seahorn/crabllvm-build-llvm8`

## crabllvm-full-size-rel.Dockerfile (building CrabLlvm with Docker):

```shell
docker build --build-arg UBUNTU=xenial --build-arg BUILD_TYPE=Release -t seahorn/crabllvm_xenial_rel -f docker/crabllvm-full-size-rel.Dockerfile .
docker run -v `pwd`:/host -it seahorn/crabllvm_xenial_rel"
```

This will automatically download all dependencies from a base image
and build CrabLlvm under `/crabllvm/build`.

CrabLlvm's install directory is added to `PATH`.

Build arguments (required):
- UBUNTU: trusty, xenial, bionic
- BUILD_TYPE: Release, Debug

