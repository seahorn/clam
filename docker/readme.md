# Building Clam with Docker and running tests

## clam-build.Dockerfile (downloading build dependencies):

```shell
docker build --build-arg UBUNTU=xenial --build-arg BUILD_TYPE=Release -t seahorn/clam-build-llvm8:xenial -f docker/clam-build.Dockerfile .
```
The above generates a container with all dependencies required to
build Clam, other than Clam itself.

Build arguments (required):
- UBUNTU: trusty, xenial
- BUILD_TYPE: Release, Debug

Depends on: `buildpack-deps`
Dockerhub: `docker pull seahorn/clam-build-llvm8`

## clam-full-size-rel.Dockerfile (building Clam with Docker):

```shell
docker build --build-arg UBUNTU=xenial --build-arg BUILD_TYPE=Release -t seahorn/clam-8.0:xenial -f docker/clam-full-size-rel.Dockerfile .
docker run -v `pwd`:/host -it seahorn/clam-8.0:xenial"
```

This will automatically download all dependencies from a base image
and build Clam under `/clam/build`.

Clam install directory is added to `PATH`.

Build arguments (required):
- UBUNTU: trusty, xenial, bionic
- BUILD_TYPE: Release, Debug

