===========================================
SOFTWARE REQUIREMENTS
===========================================

Before building the package, you should have installed the following 
software:
  - Debreate
Debreate can be downloaded from https://antumdeluge.github.io/debreate-web/
We recommend Debreate version 0.7.13.

You should also have installed some of these other software:
  - lintian
  - bash
  - dpkg
  - gzip
  - md5sum
  - fakeroot
Please, read the instructions below to be sure what do you need before 
installing them.

-------------------------------------------
How to build the Debian package
-------------------------------------------

To build the Debian package you must have a MaxFEM project folder (where 
the full path will be referenced by MAXFEMDIR) with these folders and files:
  - apps
  - code
  - config
  - help
  - images
  - materialsDB
  - plots
  - setup
  - solvers
  - sources
  - Changelog.txt
  - LICENSE.txt
  - README.txt
  - remote-run-data.txt

The Debian package can be built following these steps:

  1. You must check, and probably change, the MaxFEM version in the "About" 
  section of the "Help" by editing the file MAXFEMDIR/help/e-About/index.html.
  2. You must check, and probably change, the MaxFEM change log by editing 
  the file MAXFEMDIR/Changelog.txt.
  3. Go to the folder MAXFEMDIR/setup/linux and edit the file maxfem.dbp.
  Before open it using Debreate you must do these changes:
    3.1. You must set the version of MaxFEM by changing the value of the 
    variable Version. You can find it after the tag <<CTRL>>.
    3.2. You must replace the references to the existing path (e.g. 
    /home/user/maxfem) by the real path of your MaxFEM project folder. You 
    can find those references after the tag <<FILES>>. Doing this, you won't 
    need to add all files again in the Files section of Debreate.
    3.3. Save changes and overwrite maxfem.dbp.
  4. Run Debreate and open the file maxfem.dbp.
  5. If any file or folder was created or deleted, they must be added or 
  deleted in the Files section of Debreate.
  6. If the version of MaxFEM was changed, the changes should be added in 
  the Changelog section of Debreate.
  7. Build the package by using the Debreate build option.
  8. You must create the package in the current folder MAXFEMDIR/setup/linux.

Doing this, the resultant package may have some errors and warnings. You can 
see them using lintian by running in a terminal (where <package file> is 
the resultant package file, e.g. maxfem_1.2.3_all.deb):

    lintian <package file>

This errors, and some warnings, must be fixed doing this:

  9. Go to MAXFEMDIR/setup/linux and run fixup_deb_maxfem.sh passing the 
  package file name as an argument.
  You can do it using bash by running in a terminal:
  
    bash fixup_deb_maxfem.sh <package file>

Finally, you can run lintian again to check the rebuilt package:

    lintian <package file>

Because Debian policy and/or lintian could change, it's possible that 
fixup_deb_maxfem.sh won't fix all errors in the future. In this case, you 
should fix them by your own and you should change this instructions.
