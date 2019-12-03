# Building Clam with Docker and running tests

```shell
docker build --build-arg UBUNTU=xenial --build-arg BUILD_TYPE=Release -t clam_xenial_rel -f docker/clam-full-size-rel.Dockerfile .
docker run -v `pwd`:/host -it clam_xenial_rel"
```

This will automatically download all dependencies from a base image
and build Clam under `/clam/build`.

Clam install directory is added to `PATH`.

Build arguments (required):
- UBUNTU: trusty, xenial, bionic
- BUILD_TYPE: Release, Debug

