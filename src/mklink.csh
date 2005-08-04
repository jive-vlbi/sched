#!/bin/csh
cd Vex
ln -s ../Sched/sched.inc sched.inc
ln -s ../Sched/schset.inc schset.inc
cd ../Sched
ln -s ../Plot/plot.inc plot.inc
ln -s ../Cit/rdcat.inc rdcat.inc
cd ../Plot
ln -s ../Sched/sched.inc sched.inc
ln -s ../Sched/srlist.inc srlist.inc
cd ../PlotNRAO
ln -s ../Sched/plot.inc plot.inc
ln -s ../Sched/sched.inc sched.inc
cd ../Sat
ln -s ../Sched/sched.inc sched.inc
cd ../
