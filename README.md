### Introduction

MultiBit is a lightweight Bitcoin desktop client powered by the [Bitcoinj library](https://code.google.com/p/bitcoinj/).

### How to Build MultiBit

MultiBit use Maven as the build system, so the usual Maven processes apply. If you're not familiar 
with Maven then [download it first](http://maven.apache.com) and follow their installation instructions.

### Bitcoinj dependency

MultiBit depends on a special fork of Bitcoinj for its Bitcoin support. This is due to legacy wallet serialization issues
and the MultiBit team are working towards a complete integration. To build MultiBit you will need to clone this fork from
here:
```
https://code.google.com/r/jimburton618-bitcoinj-coinbase-tx/source/checkout
```

The branch you should use is: `bcj-master-mb-alice`

Once cloned, you should then install the custom Bitcoinj library using

```
mvn clean install
```

### Description of GitHub branches

There are 2 main branches: `master` and `develop`. The `master` branch is exclusively for releases, while the `develop`
is exclusively for release candidates. The `develop` branch always has a Maven version of `develop-SNAPSHOT`.

Every GitHub Issue gets a branch off develop. When it is complete and code reviewed it is merged into `develop`.

When sufficient Issues are merged into `develop` to justify a release, a new branch off `develop` is created with the release number (e.g. `release-1.2.3`).
The Maven `pom.xml` is updated to reflect the snapshot version (e.g. `1.2.3-SNAPSHOT`).

Once the release has been tested and is ready to go live, the final act is to update the `pom.xml` to remove the SNAPSHOT suffix and merge it into `master`.

The `master` branch is then tagged with the release number. Tags are in the format `v1.2.3` to distinguish them from branch names.

An announcement is made on the MultiBit website to alert everyone that a new version is available.

### Maven build targets

The important targets are:

```
mvn clean package
```

which will package the MultiBit project into `multibit-x.y.z.jar` where `x.y.z` is the current version
number. This is suitable for local development work.

If you want to generate a complete set of multi-platform installers (Windows, Mac and Linux) you 
use the following command

```
maven clean install
```

After some processing, you will have the following artifacts in the target directory:

* an executable jar = multibit-exe.jar
* a Mac application bundle = MultiBit.app
* a Mac DMG file = multibit-x.y.z.dmg
* an installer for Windows = multibit-x.y.z-windows.exe
* an installer for Linux = multibit-x.y.z-linux.jar

To run MultiBit from these artifacts you can follow the instructions [provided on the main MultiBit
website](https://multibit.org/help.html)

### Custom configuration

MultiBit is quite flexible and has several features only accessible to power users through the configuration file. This
is discussed in more detail in [configuration.md](configuration.md)
