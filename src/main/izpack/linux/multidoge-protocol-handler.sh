gconftool-2 -t string -s /desktop/gnome/url-handlers/dogecoin/command "java -splash:doesnotexist.png -jar $INSTALL_PATH/multidoge-exe.jar %s"
gconftool-2 -s /desktop/gnome/url-handlers/dogecoin/needs_terminal false -t bool
gconftool-2 -t bool -s /desktop/gnome/url-handlers/dogecoin/enabled true