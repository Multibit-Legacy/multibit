# Building MultiBit Classic in Docker

The included `Dockerfile` allows verifiable building of the
last release of MultiBit Classic.

This is useful for recovering old
Bitcoin wallets, exporting private keys, and importing it
into a supported wallet, including Bitcoin forks.

## Instructions

```
docker build . -t multibit:0.5.19
docker create -ti --name dummy multibit:0.5.19 bash
docker cp dummy:/target/multibit-exe.jar .
java -jar .\multibit-exe.jar
```