#! /usr/bin/env python3

from collections import Counter
import os.path
import re
import sys

def getAntennas (filename, stopPattern=None, startPattern=None, useCounter=False):

    if useCounter:
        antennas = Counter()
        def addOne (element, collection):
            if element in collection:
                collection[element]+=1
            else:
                collection[element]=1 
    else:
        antennas = set() if not useCounter else Counter()
        def addOne (element, collection):
            collection.add(element)

    skipping = startPattern is not None
    if skipping:
        print (f"--> Skipping in {filename}")

    if stopPattern is not None:
        print (f"--> Reading until stop pattern in {filename}")

    with open (filename) as data:

        for line in data:

            if skipping:
                skipping = line.rstrip() != startPattern
                if not skipping:
                    print (f"*** Ending skip for {filename=} at '{line}'")
                continue

            if stopPattern is not None and stopPattern == line.rstrip():
                print (f"Stopping at line: '{line}'")
                break

            m = re.match ("^ .*DBNAME=(?P<antenna>[^ ]+).*$", line)

            if m:
                addOne (m.group("antenna"), antennas)

    return antennas


def doIt (originalFile="locations-original.dat", newFile="locations.dat"):

    # Read in the old and new locations files

    print (f"Original locations file: {originalFile}")
    print (f"New locations file: {newFile}")


    originalAntennas = getAntennas (originalFile)
    newAntennas = getAntennas (newFile)

    # Determine if there are any missing files

    missing = originalAntennas.difference (newAntennas)

    print ("Comparing new file to original file:")

    if len(missing) != 0:
        print (f"--> New file is missing: {missing}")
    else:
        print ("--> No missing antennas")

    newAntennas = getAntennas ("locations.dat", useCounter=True)
    
    duplicates = [(k,v) for k,v in newAntennas.items() if v > 1]

    print ("\nChecking new file for duplicates:")
    if len (duplicates) == 0:
        print ("--> No duplicates.")
    else:
        print (f"--> Duplicated antennas: {duplicates=}")


    

    # newAntennas = getAntennas ("locations.geodesy.dat")
    # replacedAntennas = getAntennas ("locations-original.dat", 
    #                                 stopPattern="!   From file $SCHED/catalogs/Master_NRAO/locations_VLA.dat")

    # conflictingAntennas =  getAntennas ("locations-original.dat", 
    #                                     startPattern="!   From file $SCHED/catalogs/Master_NRAO/locations_VLA.dat")

    # print (f"*** {len(originalAntennas)} {sorted(list(originalAntennas))=}")
    # print (f"*** {len(newAntennas)} {sorted(list(newAntennas))=}")
    # print (f"*** {len(replacedAntennas)} {sorted(list(replacedAntennas))=}")
    # print (f"*** {len(conflictingAntennas)} {sorted(list(conflictingAntennas))=}")

    # s1 = originalAntennas.difference (replacedAntennas) 
    # updatedAntennas = s1.union(newAntennas)

    # missingAntennas = originalAntennas.difference (updatedAntennas)

    # if len(missingAntennas) != 0:
    #     print (f"*** {missingAntennas=}")
    # else:
    #     print ("--> No missing antennas")

    # conflict = conflictingAntennas.intersection (newAntennas)

    # if len (conflict) != 0:
    #     print (f"*** {conflict=}")
    # else:
    #     print ("--> No conflicting antennas")

    


    
if __name__ == "__main__":


    if "--help" in sys.argv or len (sys.argv) < 3:
        thisFile = os.path.basename (sys.argv[0])
        print (f"*** usage: {thisFile} oldFilename newFilename\n")
        print ("Using old locations file as a reference checks for ")
        print ("antennas missing in the new file. Also checks for duplicate")
        print ("antennas in the new file.")
        sys.exit (1)

    doIt(sys.argv[1], sys.argv[2])
