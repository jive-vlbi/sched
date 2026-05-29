#!/usr/bin/env python3

# script to normalize output files from Verify_python to use as reference files

import subprocess, shlex, sys, os

def run(cmd):
    print(cmd)
    subprocess.check_call(cmd, shell=True)

if __name__ == "__main__":

    for filename in sys.argv[1:]:
        print(filename)
        run(f'sed -i -e "\|Updating catalogs in {os.path.expanduser("~/.pysched")}|d" {filename}')
        run(f'sed -i -e "\|Catalogs in {os.path.expanduser("~/.pysched")} are up-to-date.|d" ' + filename)
        run(f'sed -i -e s:"{os.environ["SCHED"]}":"/home/eldering/sched":g {filename}')
        run('sed -i -e /"This schedule produced on:"/d ' + filename)
        run('sed -i -f Check_sed ' + filename)
