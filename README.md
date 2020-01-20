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

## Usage

After executing either the pip or conda installation method, the executable `sched.py` should be available in your $PATH.

One of the features of pySCHED is that the catalog files will be downloaded/updated on start-up of `sched.py`. This allows for the catalog files to be updated separate from the pySCHED release cycle. The files will be installed in ~/.pysched. $SCHED will be set to this directory in pySCHED if not otherwise specified. The process of downloading and updating requires the version control software git to be installed and available in the $PATH. On most Linux systems, git will be available. If it is not, it can be installed with:

sudo apt-get install git
