# Building Clam in Docker and running tests

This container builds Clam and runs some tests:

```shell
docker build -t seahorn/clam-llvm17 -f docker/clam.Dockerfile .
docker run -v `pwd`:/host -it seahorn/clam-llvm17
```
