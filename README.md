### Introduction

MultiBit is a Simplified Payment Verification (SPV) Bitcoin desktop client.

MultiBit is now in maintenance mode as it has largely been replaced by [MultiBit HD](https://multibit.org). To avoid confusion
we refer to MultiBit Classic and MultiBit HD to keep them separate.

MultiBit Classic relies on the following technologies:

* Maven as the build system, so the usual Maven processes apply. If you're not familiar
with Maven then [download it first](http://maven.apache.org) and follow their installation instructions.
* [ZXing ("Zebra Crossing")](https://code.google.com/p/zxing/) for QR codes
* [Bitcoinj](https://code.google.com/p/bitcoinj/) for access to the Bitcoin network
* Install4j for creating installers for Windows, Mac, Linux
* [Bitcoinj Enforcer Rules](https://github.com/gary-rowe/BitcoinjEnforcerRules) to prevent dependency chain attacks
* [XChange](https://github.com/timmolter/XChange) for access to several Bitcoin exchanges

### The Bitcoinj "Alice" dependency

MultiBit Classic depends on a special fork of Bitcoinj for its Bitcoin support. This is due to legacy wallet serialization issues
and the MultiBit team are working towards a complete integration through the MultiBit HD project.

While it is possible to build MultiBit Classic using our staging repository you may want to review the modified Bitcoinj library
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

This loosely follows the ["master-develop" or "Git flow"](http://nvie.com/posts/a-successful-git-branching-model/) pattern.

There are 2 main branches: `master` and `develop`. The `master` branch is exclusively for releases, while the `develop`
is exclusively for release candidates. The `develop` branch always has a Maven version of `develop-SNAPSHOT`.

Occasionally a feature branch will be made off `develop` to cover a long-running issue. This will then be merged back into `develop`

When `develop` is ready for release it is subjected to extensive testing (manual and automated). The final act is to update the `pom.xml`
to remove the SNAPSHOT suffix and merge it into `master`.

The `master` branch is then tagged with the release number. Tags are in the format `v1.2.3` to distinguish them from branch names.

An announcement is made on the MultiBit website and social media (Twitter, Reddit, Bitcointalk etc) to alert everyone that a new version is available.

### Maven build targets

The important targets are:

```
mvn clean install
```

After some processing, you will have the following artifacts in the target directory:

* an executable jar: `multibit-exe.jar`

### Bitcoin Solutions staff

Use the Install4j installer in the multibit-installers project to create your Mac/ Win/ Linux installers.

To run MultiBit from these artifacts you can follow the instructions [provided on the main MultiBit website](https://multibit.org/help.html)

### Custom configuration

MultiBit Classic is quite flexible and has several features only accessible to power users through the configuration file. This
is discussed in more detail in [configuration.md](configuration.md)

### Contributing

All contributors must be OK with releasing their work under the MIT license.