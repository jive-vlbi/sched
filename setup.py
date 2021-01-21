#!/usr/bin/env python3

import setuptools
import numpy.f2py as f2py
from numpy.distutils.core import setup, Extension
from distutils.command.sdist import sdist

import platform
import argparse
import sys
import os

parser = argparse.ArgumentParser(
    description="Setup script for pySCHED. Builds a list of source files "
    "depending on the system architecture. Other arguments will be forwarded "
    "to setup.",
    add_help=False)
parser.add_argument("--schedarch",  
                    choices=["linux", "unix", "sun", "hp"],
                    default=None, 
                    help="operating system for machine dependent fortran "
                    "routines. Defaults to linux.")

args, other_args = parser.parse_known_args()
sys.argv[1:] = other_args

if args.schedarch is None:
    arch = "linux"
    system = platform.system()
    if system not in ["Linux", "MacOS"]:
        print("Warning: unknown architecture '{}'. Will fall back to '{}'.".\
              format(system, arch))
else:
    arch = args.schedarch

# source files to compile
sources = """
Cit/geoid.f
Cit/geoxyz.f
Cit/julda.f
Cit/len1.f
Cit/pangle.f
Cit/tdatecw.f
Cit/tform.f
Cit/upcase.f
Jpl/const.f
Jpl/findt.f
Jpl/fsizer2.f
Jpl/interp.f
Jpl/jplep2.f
Jpl/jplpo2.f
Jpl/jplver.f
Jpl/pleph.f
Jpl/pvobs.f
Jpl/split.f
Jpl/state.f
Jpl/transp.f
Plot/plbeam.f
Plot/plfft.f
Plot/pltran.f
Plot/plver.f
Plot/plweig.f
Plotstub/pgqinf.f
Satstub/satep.f
Satstub/sattle.f
Sched/accsrc.f
Sched/addgeo.f
Sched/addpeak.f
Sched/antpos.f
Sched/autodown.f
Sched/azelxyew.f
Sched/azelxyns.f
Sched/badlo.f
Sched/bbcalt.f
Sched/bbccdas.f
Sched/bbcdbbc.f
Sched/bbcgeo.f
Sched/bbckvsr.f
Sched/bbclba.f
Sched/bbcm4.f
Sched/bbcrdbe.f
Sched/bbcvlba.f
Sched/bbcvs2.f
Sched/bbcwidar.f
Sched/chars.f
Sched/chkcdas.f
Sched/chkcode.f
Sched/chkcor.f
Sched/chkdbbc.f
Sched/chkdbfq.f
Sched/chkdisk.f
Sched/chkfirm.f
Sched/chkgdar.f
Sched/chkif.f
Sched/chkrdbe.f
Sched/chkrdfq.f
Sched/chksc1.f
Sched/chkscn.f
Sched/chkset.f
Sched/chksfil.f
Sched/chkspd.f
Sched/chkvdar.f
Sched/chkvdifx.f
Sched/chkvla.f
Sched/chkvlba.f
Sched/chkwidar.f
Sched/cordef.f
Sched/corlst.f
Sched/corsoc.f
Sched/crdwrt.f
Sched/defaults.f
Sched/defset.f
Sched/delscr.f
Sched/dequal.f
Sched/diskpos.f
Sched/dopcrd.f
Sched/dopfq.f
Sched/dwcase.f
Sched/errlog.f
Sched/errset.f
Sched/fcompare.f
Sched/fileopen.f
Sched/flags.f
Sched/flagwrt.f
Sched/flush.f
Sched/fmatch.f
Sched/fmtmkiii.f
Sched/fmtmkiv.f
Sched/fmtpick.f
Sched/fmts2.f
Sched/fmtvlba.f
Sched/frchar.f
Sched/fsfreq.f
Sched/fsmatch.f
Sched/fspread.f
Sched/fsvlba.f
Sched/geochk.f
Sched/geomake.f
Sched/geoqual.f
Sched/getcrdn.f
Sched/getfset.f
Sched/getpairs.f
Sched/getsun.f
Sched/glstday.f
Sched/gmkscn.f
Sched/gnset.f
Sched/haavai.f
Sched/halim.f
Sched/harmwarn.f
Sched/headpos.f
Sched/horchk.f
Sched/ifdbbc.f
Sched/inarray.f
Sched/jplgot.f
Sched/listfreq.f
Sched/lsqfit.f
Sched/lst2ut.f
Sched/lstfreq.f
Sched/makeptg.f
Sched/makescn.f
Sched/makeseg.f
Sched/maxbas.f
Sched/mtaltaz.f
Sched/mtequat.f
Sched/mtxyew.f
Sched/mtxyns.f
Sched/omscor.f
Sched/omsfreq.f
Sched/omsout.f
Sched/omsset.f
Sched/omssrc.f
Sched/omssta.f
Sched/optcells.f
Sched/optcsar.f
Sched/optcspt.f
Sched/optcsub.f
Sched/opthas.f
Sched/opthiel.f
Sched/optnone.f
Sched/optsch.f
Sched/optskd.f
Sched/opttim.f
Sched/optupt.f
Sched/pcalfq.f
Sched/phint.f
Sched/pkfinish.f
Sched/pn3db.f
Sched/protect.f
Sched/prtfreq.f
Sched/prtsch.f
Sched/prtscn.f
Sched/prtset.f
Sched/ptpat.f
Sched/ptvlba.f
Sched/ran5.f
Sched/rdbelevt.f
Sched/rdbemtch.f
Sched/recctl.f
Sched/reconfig.f
Sched/resync.f
Sched/rotvlba.f
Sched/sameset.f
Sched/satgot.f
Sched/sattim.f
Sched/sbhours.f
Sched/sbpair.f
Sched/scanid.f
Sched/sch24.f
Sched/schday.f
Sched/schgeo.f
Sched/schopt.f
Sched/schpre.f
Sched/schsum.f
Sched/schtim.f
Sched/scndup.f
Sched/scngeo.f
Sched/scnrange.f
Sched/scramble.f
Sched/sdopincr.f
Sched/setband.f
Sched/setbbc.f
Sched/setchan.f
Sched/setcop.f
Sched/setdefs.f
Sched/setexpnd.f
Sched/setfcat.f
Sched/setfirm.f
Sched/setfmt.f
Sched/setform.f
Sched/setfreq.f
Sched/sethw1.f
Sched/setnorec.f
Sched/setrec.f
Sched/setstdef.f
Sched/settps.f
Sched/settrk.f
Sched/setusyn.f
Sched/sfinfo.f
Sched/shortn.f
Sched/sidtim.f
Sched/slew.f
Sched/socdef.f
Sched/srcflg.f
Sched/srcloc.f
Sched/srclst.f
Sched/srcwrt.f
Sched/srfinish.f
Sched/srinsert.f
Sched/srlist.f
Sched/srlpre.f
Sched/stafiles.f
Sched/stafrd.f
Sched/stageo.f
Sched/stalst.f
Sched/stano.f
Sched/stauv.f
Sched/stmsg.f
Sched/stsum.f
Sched/sumdat.f
Sched/sumdesc.f
Sched/sumope.f
Sched/sumscn.f
Sched/sunpos.f
Sched/sunwarn.f
Sched/suvopt.f
Sched/tavlba.f
Sched/timej.f
Sched/tpmfix.f
Sched/tpsum.f
Sched/tptpns.f
Sched/twohdset.f
Sched/uvopt.f
Sched/uvqual.f
Sched/v2dout.f
Sched/versched.f
Sched/vexout.f
Sched/vlascns.f
Sched/vlba.f
Sched/vlbabws.f
Sched/vlbachar.f
Sched/vlbadk.f
Sched/vlbaend.f
Sched/vlbaini.f
Sched/vlbaint.f
Sched/vlbareal.f
Sched/vlbast.f
Sched/vlbastop.f
Sched/vlbasu.f
Sched/vsopwrt.f
Sched/wlog.f
Sched/wrap.f
Sched/wrapzone.f
Sched/wrtcov.f
Sched/wrtfreq.f
Sched/wrtmsg.f
Sla/amp.f
Sla/ampqk.f
Sla/calyd.f
Sla/cldj.f
Sla/clyd.f
Sla/cs2c.f
Sla/dat.f
Sla/dcc2s.f
Sla/dcs2c.f
Sla/deuler.f
Sla/dimxv.f
Sla/djcl.f
Sla/dmxm.f
Sla/dmxv.f
Sla/dranrm.f
Sla/dsep.f
Sla/dsepv.f
Sla/dt.f
Sla/dvdv.f
Sla/dvn.f
Sla/dvxv.f
Sla/earth.f
Sla/ecor.f
Sla/epb2d.f
Sla/epj.f
Sla/eqeqx.f
Sla/evp.f
Sla/fk45z.f
Sla/fk524.f
Sla/fk54z.f
Sla/gmst.f
Sla/map.f
Sla/mappa.f
Sla/mapqk.f
Sla/mapqkz.f
Sla/nut.f
Sla/nutc.f
Sla/pm.f
Sla/prebn.f
Sla/prec.f
Sla/preces.f
Sla/precl.f
Sla/prenut.f
Sla/rvlsrk.f
Sla/vdv.f
Vex/chk4dar.f
Vex/chkjive.f
Vex/chkv4dar.f
Vex/vxcfan.f
Vex/vxcfbb.f
Vex/vxcfda.f
Vex/vxcffq.f
Vex/vxcfhp.f
Vex/vxcfif.f
Vex/vxcfph.f
Vex/vxcfpo.f
Vex/vxcfrl.f
Vex/vxcfsi.f
Vex/vxcftr.f
Vex/vxcovr.f
Vex/vxdefs.f
Vex/vxfqvx.f
Vex/vxgtst.f
Vex/vxmode.f
Vex/vxnhds.f
Vex/vxnman.f
Vex/vxnmbb.f
Vex/vxnmda.f
Vex/vxnmfq.f
Vex/vxnmhp.f
Vex/vxnmif.f
Vex/vxnmph.f
Vex/vxnmpo.f
Vex/vxnmpr.f
Vex/vxnmrl.f
Vex/vxnmsi.f
Vex/vxnmtr.f
Vex/vxnmxx.f
Vex/vxrl16.f
Vex/vxrl8.f
Vex/vxs2df.f
Vex/vxs2md.f
Vex/vxs2vl.f
Vex/vxsch.f
Vex/vxschk.f
Vex/vxscns.f
Vex/vxsort.f
Vex/vxstky.f
Vex/vxstli.f
Vex/vxstnm.f
Vex/vxsudt.f
Vex/vxtels.f
Vex/vxton2.f
Vex/vxtone.f
Vex/vxtrafq.f
Vex/vxtraif.f
Vex/vxtramd.f
Vex/vxtran.f
Vex/vxtraph.f
Vex/vxunql.f
Vex/vxvbrx.f
Vex/vxvers.f
Vex/vxwran.f
Vex/vxwrbb.f
Vex/vxwrda.f
Vex/vxwrex.f
Vex/vxwrfq.f
Vex/vxwrgl.f
Vex/vxwrhp.f
Vex/vxwrif.f
Vex/vxwrmd.f
Vex/vxwrph.f
Vex/vxwrpo.f
Vex/vxwrpr.f
Vex/vxwrrl.f
Vex/vxwrsi.f
Vex/vxwrst.f
Vex/vxwrsu.f
Vex/vxwrt.f
Vex/vxwrtr.f 
pysched/openwrap.f 
pysched/tformwrp.f 
pysched/ran5wrap.f 
pysched/verwrap.f 
""".split()

if arch in ["hp", "linux", "sun", "unix"]:
    sources += """
Cit/sys_unix/envir.f
Cit/sys_unix/krdlin.f
Cit/sys_unix/prognm.f
Cit/sys_unix/putout.f
Cit/sys_unix/schdefs.f
Cit/sys_unix/symsub.f
Cit/sys_unix/tsttty.f
Cit/sys_unix/vmshlp.f
""".split()

if arch == "linux":
    sources += """
Cit/sys_linux/error.f
Cit/sys_linux/gerror.c
Cit/sys_linux/idate.c
Cit/sys_linux/isatty.c
Cit/sys_linux/vlbope.f
""".split()
elif arch in ["sun", "hp"]:
    sources += """
Cit/sys_sun/error.f
Cit/sys_sun/vlbope.f
""".split()

# fortran functions to create a python interface for
functions = """
accsrc 
autodown 
bbccdas 
bbcgeo 
bbckvsr 
bbclba 
bbcm4 
bbcrdbe 
bbcvlba 
bbcwidar 
chk4dar 
chkcdas 
chkcode 
chkcor 
chkdbfq 
chkdisk 
chkfirm 
chkjive 
chkrdbe 
chkv4dar
chkvdar 
chkvgdar 
chkvla 
chkvlba  
chkwidar 
chksc1 
chkscn 
chksfil 
chkspd 
chkvdifx 
cordef 
delscr 
diskpos 
dopfq 
errlog 
error 
errset 
flags 
fluxh 
geoqual 
geoxyz 
getpairs 
getsun 
gnset 
haavai 
halim 
jplver 
listfreq 
lst2ut 
makeptg 
omsout 
openwrap 
optsch 
opttim 
plbeam 
plver 
pkfinish 
prtscn 
putout 
ran5wrap 
recctl 
sattim 
sbpair 
sch24 
schgeo 
schpre 
schsum 
schtim 
scngeo 
scnrange 
setchan 
setfcat 
setfirm 
setfreq 
sethw1 
setrec 
setstdef 
settps 
settrk 
setusyn 
sdopincr 
sfinfo 
sidtim 
sla_calyd 
sla_cldj 
sla_dat 
socdef 
srcflg 
srfinish 
srinsert 
srlist 
stafiles 
stageo 
stano 
stauv 
stmsg 
sunpos 
tdatecw 
tformwrp 
timej 
tptpns 
v2dout 
verwrap 
vexout 
vlascns 
vsopwrt 
vxs2md 
vxton2 
wlog 
wrtmsg
""".split()

includes = """
Cit/rdcat.inc
Plot/beam.inc
Plot/plot.inc
Plot/proj.inc
Plot/sched.inc
Plot/schset.inc
Plot/srlist.inc
Sat/sched.inc
Sched/plot.inc
Sched/rdcat.inc
Sched/sched.inc
Sched/schfreq.inc
Sched/schpeak.inc
Sched/schset.inc
Sched/srlist.inc
Sched/vxlink.inc
Vex/sched.inc
Vex/schset.inc
Vex/vxlink.inc
""".split()

# include catalogs and setups directory contents
data_files = [(dir_, [os.path.join(dir_, file_) for file_ in files])
              for data_dir in ("catalogs", "setups")
              for dir_, _, files in os.walk(data_dir)]

# F2py assumes a Fortran dialect from the file name. For F77, it only accepts
# comments starting with a 'C'. But Sched uses '!' for comments too. This hack
# forces f2py to scan for those too.
f2py.crackfortran.is_f_file = lambda _: False

this_dir = os.path.abspath(os.path.dirname(__file__))
with open(os.path.join(this_dir, "README.md")) as f:
    long_description = f.read()

extension = Extension(
    name="schedlib",
    sources=["src/" + s for s in sources + includes],
    f2py_options=["only:"] + functions + [":"])
setup(
    cmdclass={"sdist": sdist},
    name="pythonSCHED",
    version="1.16.1",
    author="Bob Eldering",
    author_email="eldering@jive.eu",
    description="Python extension of NRAO's VLBI scheduling program SCHED "
    "(see http://www.aoc.nrao.edu/~cwalker/sched/)",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/jive-vlbi/sched",
    packages=setuptools.find_packages("src"),
    package_dir={"": "src"},
    data_files=data_files,
    classifiers=[
        "Programming Language :: Python :: 3",
        "Programming Language :: Fortran",
        "Operating System :: POSIX :: Linux",
        "Operating System :: MacOS",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
        "Intended Audience :: Science/Research",
        "Topic :: Scientific/Engineering :: Astronomy"],
    scripts=["src/sched.py"],
    ext_modules=[extension],
    python_requires=">=3.6",
    setup_requires=["numpy>=1.16",
                    "setuptools>=24.3"],
    install_requires=["numpy>=1.16",
                      # on some systems pyqt 5.14 tries to build from source 
                      # and fails
                      "pyqt5<5.14", 
                      "matplotlib>=3",
                      "formlayout",
                      "astropy",
                      "bottle",
                      "gitpython",
                      "requests"
                  ]
)
