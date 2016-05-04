#!/bin/sh
APP="GEDKeeper2"
EXT="ged"
COMMENT="Personal genealogical database editor"
# APP_PATH=/usr/bin/
APP_PATH="~/share/GEDKeeper/"

# Create directories if missing
mkdir -p ~/.local/share/mime/packages
mkdir -p ~/.local/share/applications
mkdir -p ~/.local/share/pixmaps

# copy associated icons to pixmaps
cp ./$APP.png ~/.local/share/pixmaps

# Create mime xml 
echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<mime-info xmlns=\"http://www.freedesktop.org/standards/shared-mime-info\">
    <mime-type type=\"application/x-$APP\">
        <comment>$COMMENT</comment>
        <icon name=\"$APP.png\"/>
        <glob pattern=\"*.$EXT\"/>
    </mime-type>
</mime-info>" > ~/.local/share/mime/packages/application-x-$APP.xml

# Create application desktop
echo "[Desktop Entry]
Name=$APP
Comment=$COMMENT
Exec=mono $APP_PATH$APP.exe %U
TryExec=mono $APP_PATH$APP.exe
MimeType=application/x-$APP
Icon=$APP.png
Terminal=false
Type=Application
Categories=Hobby
StartupNotify=true
"> ~/.local/share/applications/$APP.desktop

# update databases for both application and mime
update-desktop-database ~/.local/share/applications
update-mime-database ~/.local/share/mime

