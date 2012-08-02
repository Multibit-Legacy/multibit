Linux readme.txt

MultiBit 0.5.0alpha is a test version of MultiBit that supports encrypted wallets. Please try it out cautiously.

This is NOT production code yet so please read the following notes:

1) The encryption algorithm and/ or wallet persistence format may change (i.e. be improved) between now and the production version. This will most likely make any wallets you create in MultiBit 0.5.0alpha unreadable by the final 0.5.x release. Older pre-encryption wallets will be read ok. It is just not worth cluttering up the code and supporting a wallet format if we intentionally improve it during the alpha test period.

It is recommended you create a test directory and save any wallets you create for testing in there. Then if the wallet format/ encryption routines change you can just open the wallets using MultiBit 0.5.0alpha, transfer the bitcoin out and throw the test wallets away.



2) There is a lot of new code that has been added to MultiBit to support encrypted wallets. New code usually means new bugs. For this reason I have temporarily altered the MultiBit build so that MultiBit 0.5.0alpha opens in 'portable mode'. It has its own startup file (multibit.properties) so that it does not open your 'real' MultiBit wallets.

At this stage do not put large amounts of bitcoin in the encrypted wallets just in case there is a bug that zaps your private keys. Do an export of your private keys to be on the safe side. I have been testing it with 0.1 BTC here and there. (You can probably use the testnet - see the configuration.txt - to to be honest I never bother with the testnet).

I have managed to zap my private keys a couple of times in development so please be careful!



3) Let me know if you have any problems even if they are difficult to reproduce or a little vague. Tightening up and hardening the code now may very well save someone's hard earned bitcoins in the future.



4) Caveats aside - I hope you like the new encrypted wallets functionality. :-)

