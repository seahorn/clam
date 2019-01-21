# Building CrabLlvm with Docker and running tests

```shell
docker build --build-arg UBUNTU=xenial --build-arg BUILD_TYPE=Release -t crabllvm_xenial_rel -f docker/crabllvm-full-size-rel.Dockerfile .
docker run -v `pwd`:/host -it crabllvm_xenial_rel"
```

This will automatically download all dependencies from a base image
and build CrabLlvm under `/crabllvm/build`.

CrabLlvm's install directory is added to `PATH`.

Build arguments (required):
- UBUNTU: trusty, xenial
- BUILD_TYPE: Release, Debug

