# README

MaxFEM is a free software for numerical simulation in electromagnetism. 

Please open [help/b-User Guide/index.html](./help/b-User%20Guide/index.html) for more information. 

## Prerequities
MaxFEM requires [Python 3](https://www.python.org/downloads/) with packages [wxPython](https://pypi.org/project/wxPython/), [vtk](https://pypi.org/project/vtk/) and [paramiko](https://pypi.org/project/paramiko/).

## Installation
We recommend to install the binaries. 
### Binaries for Windows
Download the [binaries for Windows](https://www.usc.es/en/proxectos/maxfem/index.html).
### Building from source
Only if you want to build MaxFEM from source, you will need *gfortran* and *make*:
- In Windows, both applications are included in the [official MinGW build](http://www.mingw.org/).
- In Debian-based Linux distributions, they can be found in the official repositories.
#### Building from source in Windows
- In the Command Prompt, go to the MaxFEM installation folder, subfolder *code*, and run `make -f Makefile.windows`.
- To remove all files obtained from the previous compilation, run `make -f Makefile.windows clean`.
#### Building from source in Linux
- In a terminal, go to the MaxFEM installation folder, subfolder *code*, and run `make`.
- To remove all files obtained from the previous compilation, run `make clean`.

## Updating MaxFEM
If you are updating MaxFEM, you must reset the *Materials database*:
1. Click on menu *Project - Configuration* and select *Reset*.
2. Click on manu *Materials database* and select *Reset*.

If you want to keep the materials you defined in the old version:
1. Save the file *materials.xml* located in the HOME MaxFEM folder (see 
  menu [*Help - User guide - Graphical Interface - Solvers compilation - 
  Folder location*](./help/b-User%20guide/Graph_interface/Solvers%20compilation%20and%20installation.html)).
  2. Reset the materials database as previously explained.
  3. Manually merge your *materials.xml* with the new one created in the HOME MaxFEM 
  folder.

## Usage
- Go to the MaxFEM installation folder, subfolder *code*, and execute [*MaxFEM.py*](./code/MaxFEM.py). 
- To explore some examples, first choose an application in the menu *Applications* and then a sample data in the menu *Sample data*. Please go to menu [*Help - User guide - Applications*](./help/b-User%20guide/Applications/Application.html) for a description of those examples. 

## Testing
This code has been tested in the following platforms:
- Windows 10 with Python 3.8, wxPython 4.1, vtk 9.0 and paramiko 2.7. It was built with gfortran 5.3.

## Known issues
1. If you install Python packages using `pip install` as non-root user, you may need to include the user site-packages directory in the user environment variable *Path*. See more inforation [here](https://packaging.python.org/tutorials/installing-packages/#installing-to-the-user-site).