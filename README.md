# pySCHED



**pySCHED** is a program for planning and scheduling very long baseline interferometry (VLBI) observations conducted by different networks such as the European VLBI Network (EVN), the Very Long Baseline Array (VLBA), the High Sensitivity Array (HSA), the Global VLBI, the Australian Long Baseline Array (LBA), or the Korean VLBI Network (KVN). **pySCHED** produces the standard files that need to be distributed to the different telescopes and correlators for VLBI observations and operations.

**pySCHED** is built upon the [NRAO SCHED](http://www.aoc.nrao.edu/~cwalker/sched/) program developed by Craig Walker at NRAO, and adds unique features:

- Central catalog management on GitHub. The version controlled catalogs provide full access to the change history of each catalog file. By default, at startup, **pySCHED** always updates the catalogs; using old/outdated catalogs is no cause for errors anymore. We note that the feature can be switched off if needed (e.g. for multi-epoch observations).
- Full DBBC2 scheduling support, essential for EVN observations.
- Since v1.8.1 **pySCHED** checks for updates upon startup, informing the user of a new(er) version being available.
- The KEYIN file can be a template schedule with executable Python-code in it. This could simplify scheduling standard observations.
- Can generate VEX1 as well as VEX2 output.
- Plotting is now done through Python's `matplotlib` library, making it easier to control/enhance and/or implement different plots.



## Getting started

To learn how to write a scheduling file (`.key`) that can be used to generate the observing files (`.vex` file) with **pySCHED**, you can follow the User Guide at the [EVN Scheduling Page](https://www.evlbi.org/evn-scheduling). This documentaion is focused on observations with the European VLBI Network (EVN). For a more extended documentation on how to write the scheduling files, please take a look at the general [NRAO SCHED User Manual](http://www.aoc.nrao.edu/~cwalker/sched/).

Once the `.key` file is prepared, **pySCHED** can generate the scheduling files. The basic operations can be conducted by running:

```bash
sched.py [-p] -k {keyfile}
```

where the optional parameter `-p`  retrieves the graphical mode in order to be able to make plots about the observation.



## Installation



### Dependecies

- NumPy (>= 1.16).
- Git.



### pip

```bash
sudo pip3 install numpy
sudo pip3 install pythonSCHED
```

`pip3` is included in most python3 installations, otherwise it can be installed with:

```bash
sudo apt-get install python3-pip
```

### conda

Alternatively, if you use `conda`, [this](https://github.com/jive-vlbi/sched/raw/python/pySCHED.yaml) YAML file is a conda environment which includes both `NumPy` and **pySCHED**. To use it:

```bash
conda env create -f pySCHED.yaml  
conda activate pySCHED
```



## Update

The command to update pySCHED is:

```bash
sudo pip3 install --upgrade pythonSCHED
```

Or when using a conda installation:

```bash
conda activate pySCHED
pip install --upgrade pythonSCHED
```



## Usage

After executing either the `pip` or `conda` installation method, the executable `sched.py` should be available in your `$PATH`.

One of the features of **pySCHED** is that the catalog files will be downloaded/updated on start-up of `sched.py`. This allows for the catalog files to be updated separate from the pySCHED release cycle. The files will be installed in `~/.pysched`. `$SCHED` will be set to this directory in **pySCHED** if not otherwise specified. The process of downloading and updating requires the version control software `git` to be installed and available in the `$PATH`. On most GNU/Linux systems, `git` will be available. If it is not, it can be installed with:

```bash
sudo apt-get install git
```



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
* __v1.8.2 (2020-05-27)__: Fixed update catalog writing to an unopened log file.
* __v1.9.0 (2020-06-09)__: Overhaul of the README.
* __v1.10.0 (2020-06-22)__: SCHED 11.6 merged into pySCHED.
* __v1.11.0 (2020-07-11)__: Add warning message on empty KEYIN input record.


# Contact


**pySCHED** is maintained at the [Joint Institute for VLBI ERIC (JIVE)](https://www.jive.eu). You can reach the maintainers at [pysched@jive.eu](mailto:pysched@jive.eu).


# Acknowledgement


This project has received funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement No 730884.


