# Building MultiBit Classic in Docker

The included `Dockerfile` allows verifiable building of the
last release of MultiBit Classic.

This is useful for recovering old
Bitcoin wallets, exporting private keys, and importing it
into a supported wallet, including Bitcoin forks.

You can build the image from scratch, or pull a pre-built
image from DockerHub, then continue to the last section
for copying out the built Java JAR executable.

## Instructions for Building

If you'd like to build this from scratch after
examining `Dockerfile`:

```
docker build . -t <your_login>/multibit:0.5.19
```

## Instructions for Pulling

```
docker pull cryptogoth/multibit:0.5.19
```

## Instructions for Running Executable

```
docker create -ti --name dummy multibit:0.5.19 bash
docker cp dummy:/target/multibit-exe.jar .
java -jar .\multibit-exe.jar
```

Enjoy.