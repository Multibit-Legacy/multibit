### Introduction

MultiBit is a Simplified Payment Verification (SPV) Bitcoin desktop client.

MultiBit relies on the following technologies:

* Maven as the build system, so the usual Maven processes apply. If you're not familiar
with Maven then [download it first](http://maven.apache.org) and follow their installation instructions.
* [ZXing ("Zebra Crossing")](https://code.google.com/p/zxing/) for QR codes
* [Bitcoinj](https://code.google.com/p/bitcoinj/) for access to the Bitcoin network
* IzPack for creating installers for Windows, Mac, Linux
* [Bitcoinj Enforcer Rules](https://github.com/gary-rowe/BitcoinjEnforcerRules) to prevent dependency chain attacks
* [XChange](https://github.com/timmolter/XChange) for access to several Bitcoin exchanges

### The Bitcoinj "Alice" dependency

MultiBit depends on a special fork of Bitcoinj for its Bitcoin support. This is due to legacy wallet serialization issues
and the MultiBit team are working towards a complete integration through the MultiBit HD project.

While it is possible to build MultiBit using our staging repository you may want to review the modified Bitcoinj library
for yourself. You can clone from this fork:
```
https://code.google.com/r/jimburton618-bitcoinj-coinbase-tx/source/checkout
```

The branch you should use for the MultiBit master code is: `bcj-0.11.2-mb-alice`
The branch you should use for the MultiBit develop code is: `bcj-0.11.2-mb-alice`

Once cloned, you should then install the custom Bitcoinj library using

```
mvn clean install
```

### Branching strategy

This follows the ["master-develop" or "Git flow"](http://nvie.com/posts/a-successful-git-branching-model/) pattern.

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

### MultiBit contains cut down JREs so is a large clone

The MultiBit installers contain cut down JREs so the project clone is quite large.
(100 to 200 MB).

### Custom configuration

MultiBit is quite flexible and has several features only accessible to power users through the configuration file. This
is discussed in more detail in [configuration.md](configuration.md)

### Contributing

MultiBit is a very active project and if you would like to contribute please feel free to get in touch using [jim@multibit.org](mailto:jim@multibit.org).
We are particularly looking for developers with the following skills to contribute:

* Experienced Java programmers
* Web designers [for the website](https://github.com/jim618/multibit-website)
* Translators using the [Crowdin site](http://translate.multibit.org/)
* Beta testers for checking the latest pre-release

All contributors must be OK with releasing their work under the MIT license.