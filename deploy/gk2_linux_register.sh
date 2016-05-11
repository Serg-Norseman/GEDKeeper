#!/bin/sh
APP="GEDKeeper2"
EXT="ged"
COMMENT="Personal genealogical database editor"
# APP_PATH=/usr/bin/
APP_PATH="/home/norseman/share/GEDKeeper/"

# Create directories if missing
mkdir -p ~/.local/share/mime/packages
mkdir -p ~/.local/share/applications
mkdir -p ~/.local/share/pixmaps

# Copy associated icons to pixmaps
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
Comment[ru]=Редактор персональной генеалогической базы данных
Comment[uk]=Редактор персональної генеалогічної бази даних
Path=$APP_PATH
Exec=mono $APP.exe %U
MimeType=application/x-$APP
Icon=$APP.png
Terminal=false
Type=Application
Categories=Office;Hobby
StartupNotify=true
"> ~/.local/share/applications/$APP.desktop

cp ~/.local/share/applications/$APP.desktop ~/Desktop/$APP.desktop
chmod a+x ~/Desktop/$APP.desktop

# Update databases for both application and mime
update-desktop-database ~/.local/share/applications
update-mime-database ~/.local/share/mime

