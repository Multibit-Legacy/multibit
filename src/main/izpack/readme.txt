readme.txt

MultiBit is currently experimental software.
Do not use it with large amounts of bitcoin.

IMPORTANT - Mac users - Make sure you do not lose your wallets 
                        when upgrading !

Mac users - DO THIS BEFORE UPGRADING
1) Right click on /Applications/MultiBit.app and select 
   'Show Package Contents'.
2) Navigate to the directory 'Contents/Resources/Java' in the 
   application bundle.
3) Copy the multibit.wallet and multibit.info files to a safe place.
4) If you have any other wallet and info files in this directory,
   copy them to a safe place.
5) Install MultiBit as normal.

For PC and Linux users the default installation directory is 
different between MultiBit 0.2.0beta2 and previous versions.





Configuration Options:

+ Connect to single node
If you want to connect to a single node set the following 
property in multibit.properties:
singleConnectionNode=<node to connect to>

The node can be specified as either a domain name (www.myNode.com) 
or an IP address.


+ Testnet
To use the testnet set the property "testOrProductionNetwork" 
in the file multibit.properties to be "test".

