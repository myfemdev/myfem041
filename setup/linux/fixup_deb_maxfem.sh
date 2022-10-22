#! /bin/bash -e

pkgfilename="$1"

# Make a temporary folder and extract the files from the Debian package
mkdir fix_up_deb
dpkg -x "$pkgfilename" fix_up_deb
dpkg --control "$pkgfilename" fix_up_deb/DEBIAN

# Change the mode of some files to set the permissions needed
find fix_up_deb -type d -print0 | xargs -0 chmod 755
find fix_up_deb/usr/share/maxfem -type f -print0 | xargs -0 chmod 644
chmod 755 fix_up_deb/usr/share/maxfem/code/MaxFEM.py
chmod 755 fix_up_deb/usr/share/maxfem/sources/libs/mumps-4.9.2/debian/rules
chmod 644 fix_up_deb/DEBIAN/md5sums
chmod 644 fix_up_deb/usr/share/applications/MaxFEM.desktop
chmod 644 fix_up_deb/usr/share/doc/maxfem/changelog.gz
chmod 644 fix_up_deb/usr/share/doc/maxfem/copyright

# Change the changelog file name according to Debian policy
#gzip -d fix_up_deb/usr/share/doc/maxfem/changelog.gz
#mv fix_up_deb/usr/share/doc/maxfem/changelog fix_up_deb/usr/share/doc/maxfem/changelog.Debian
#gzip -9 fix_up_deb/usr/share/doc/maxfem/changelog.Debian

# Rebuild the MD5 checksum file
cd fix_up_deb
find usr -type f -exec md5sum {} \; > DEBIAN/md5sums
cd ..

# Remove the old package
#rm "$pkgfilename"

# Rebuild teh Debian package
fakeroot dpkg -b fix_up_deb "$pkgfilename"

# Remove the temporary folder
rm -rf fix_up_deb
