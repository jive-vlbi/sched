# pySCHED

pySCHED is built upon [SCHED](http://www.aoc.nrao.edu/~cwalker/sched/) as developed by Craig Walker at NRAO.

## Installation

### pip

sudo pip3 install numpy  
sudo pip3 install pythonSCHED  

The setup script of pySCHED requires NumPy (>=1.16).

pip3 is included in most python3 installations, otherwise it can be installed with:

sudo apt-get install python3-pip

### conda

Alternatively, if you use conda, [this](https://github.com/jive-vlbi/sched/raw/python/pySCHED.yaml)  YAML file is a conda environment which includes both NumPy and pySCHED. To use it

conda env create -f pySCHED.yaml  
conda activate pySCHED  

## Update

The command to update pySCHED is:   

sudo pip3 install --upgrade pythonSCHED

Or when using a conda installation:   

conda activate pySCHED   
pip install --upgrade pythonSCHED   

## Usage

After executing either the pip or conda installation method, the executable `sched.py` should be available in your $PATH.

One of the features of pySCHED is that the catalog files will be downloaded/updated on start-up of `sched.py`. This allows for the catalog files to be updated separate from the pySCHED release cycle. The files will be installed in ~/.pysched. $SCHED will be set to this directory in pySCHED if not otherwise specified. The process of downloading and updating requires the version control software git to be installed and available in the $PATH. On most Linux systems, git will be available. If it is not, it can be installed with:

sudo apt-get install git

# Release history

* __v1.2.0 (2019-09-27)__: Merge with SCHED release 11.5.
* __v1.2.1 (2019-10-18)__: Bug fixes for readline and matplotlib version.
* __v1.3.0 (2019-11-19)__: Added SCANEXPS keyword to signal intent in VEX2 files. To make that work, Fortran function leading to calls of SCNDUP are translated to Python.
* __v1.4.0 (2019-12-13)__: Added 'eMERL' value to DAR keyword, to enable eMERLIN out-station automatic IF and BBC assignment.
* __v1.5.0 (2020-02-25)__: Improved error/warning messages.
* __v1.5.1 (2020-03-24)__: Updated the error message for a missing '/'.
* __v1.5.2 (2020-03-26)__: Update version restriction to PyQt version.
* __v1.5.3 (2020-04-02)__: Handle changes made in matplotlib version 3.2.
* __v1.6.0 (2020-05-12)__: Catalog updates for May/June session and plot improvements.
* __v1.7.0 (2020-05-18)__: Include pySCHED version in command line and interactive help text.
* __v1.8.0 (2020-05-19)__: Do a version check on start-up.
* __v1.8.1 (2020-05-20)__: Include README.md in distribution, it is required for installation.


