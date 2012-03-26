This installer will install Bigloo into /usr/local (see attached 'Files' for a list of the files).

Uninstall:
To uninstall Bigloo use the following command (update the VERSION if necessary):

export VERSION=3.8a;
bash -c "\
lsbom -fls /Library/Receipts/bigloo-$VERSION.pkg/Contents/Archive.bom | (cd /; sudo xargs rm); \
lsbom -ds /Library/Receipts/bigloo-$VERSION.pkg/Contents/Archive.bom | (cd /; sudo xargs rmdir -p); \
sudo rm -r /Library/Receipts/bigloo-$VERSION.pkg"

Error-messages "Directory not empty" are normal and can be ignored.

Or you may use a graphical frontend for this task.
for instance OSXPM (we haven't tested this program though):
 http://www.osxgnu.org/info/pkgdelete.html

