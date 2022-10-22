===========================================
SOFTWARE REQUIREMENTS
===========================================

Before building the installer, you should have installed the following 
software:
  - Inno Setup
Inno Setup can be downloaded from http://www.jrsoftware.org/isinfo.php
We recommend the Inno Setup QuickStart Pack version 5.5.9 (unicode).

-------------------------------------------
How to build the Windows installer
-------------------------------------------

To build the Windows installer you must have a MaxFEM project folder (where 
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

The Windows installer can be built following these steps:

* You can edit, but not overwrite, the file MaxFEM-win32-setup.iss.
  Keep MyAppVersion "X.Y.Z" to avoid errors in future use.
  
  1. You must check, and probably change, the MaxFEM version in the "About" 
  section of the "Help" by editing the file MAXFEMDIR\help\e-About\index.html.
  2. You must check, and probably change, the MaxFEM change log by editing 
  the file MAXFEMDIR\Changelog.txt.
  3. You must create the MaxFEM solvers by compiling from source. You can 
  see how to do that in MAXFEMDIR\README.txt.
  4. Go to the folder MAXFEMDIR\setup\windows and open the file 
  MaxFEM-win32-setup.iss using Inno Setup.
  5. Set the version of MaxFEM by changing the value "X.Y.Z" of the 
  variable MyAppVersion.
  6. If necessary, set the revision of the installer package by changing 
  the value "0" of the variable MySetupRevision.
  7. If necessary, change the value of any other variable defined by 
  "#define ...".
  8. Build the installer by using the Inno Setup compile option. Remember 
  that you can (and must) compile without saving changes.
  9. The installer will be created in the folder MAXFEMDIR\setup\windows\output.
