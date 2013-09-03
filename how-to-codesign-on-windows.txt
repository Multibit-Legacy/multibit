This how-to describes how to code sign the Windows installer.

1) Build all the installers on your main build machine (assumed not to be a Windows machine).

2) Prepare a directory on a USB drive containing:
   + the windows installer file e.g. multibit-0.5.14-windows-setup.exe
   + the encrypted private key + object signing certificate PFX file.
     (James-Burton-object-pfx.p12). This is not under source control for security reasons.
   + we will use KSign to do the signing so you might want a copy of the KSign
     installer, ksign-installer.exe, on your USB.

3) Transfer the USB to your Windows signing machine.
4) Install KSign if necessary.
5) Start KSign. add the following entries:
   File to sign: multibit-0.5.14-windows-setup.exe
   Description: MultiBit - lightweight Bitcoin wallet
   Enter the password to decrypt the PFX file.
6) You should see a success message.
7) Test the installer by double clicking on it.
   You should see a User Account Control dialog signed by "James Burton".
8) Transfer the signed installer back to your main build machine.
9) Put it in the releases directory.

Remember: you need to do the PGP signing and SHA256 hashes AFTER code signing. 