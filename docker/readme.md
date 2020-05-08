# Building Clam in Docker and running tests

This container builds Clam and runs some tests:

```shell
docker build -t seahorn/clam-llvm10:latest -f clam.Dockerfile .
docker run -v `pwd`:/host -it seahorn/clam-llvm10:latest"
```
