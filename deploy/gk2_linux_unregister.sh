#!/bin/sh
APP="GEDKeeper2"

# Remove associated icons to pixmaps
rm -f ~/.local/share/pixmaps/$APP.png

# Remove mime xml 
rm -f ~/.local/share/mime/packages/application-x-$APP.xml

# Remove application desktop
rm -f ~/.local/share/applications/$APP.desktop
rm -f ~/Desktop/$APP.desktop

# Update databases for both application and mime
update-desktop-database ~/.local/share/applications
update-mime-database ~/.local/share/mime

