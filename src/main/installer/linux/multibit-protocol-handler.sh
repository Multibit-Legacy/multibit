gconftool-2 -t string -s /desktop/gnome/url-handlers/bitcoin/command "java -splash:doesnotexist.png -jar $INSTALL_PATH/multibit-exe.jar %s"
gconftool-2 -s /desktop/gnome/url-handlers/bitcoin/needs_terminal false -t bool
gconftool-2 -t bool -s /desktop/gnome/url-handlers/bitcoin/enabled true