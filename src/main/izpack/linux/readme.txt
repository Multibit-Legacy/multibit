Linux readme.txt

MultiBit 0.5.5beta is a test version of MultiBit that supports
encrypted wallets. Please try it out cautiously.

1) The encrypted wallet format HAS CHANGED. It is not backwards
   compatible so earlier 0.5.x versions will not be able to read
   wallets written by 0.5.5beta. 
   This is a side effect of a bug fix to prevent 0.4.x
   versions (potentially) opening encrypted wallets erroneously.

2) It is recommended you keep your encrypted wallets in a separate
   directory so that you can keep track of them.

3) MultiBit 0.5.5beta opens in 'portable mode'. It has its own 
   startup file (multibit.properties) so that it does not open 
   your 'real' MultiBit wallets.

4) Let me know if you have any problems.