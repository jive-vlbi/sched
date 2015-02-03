C$Procedure      ZZEKTRAP ( EK tree, append item )
 
      SUBROUTINE ZZEKTRAP ( HANDLE, TREE, VALUE, KEY )
 
C$ Abstract
C
C     Append an item to a tree.  The key indicating the location of
C     the new item is returned.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     EK
C
C$ Keywords
C
C     EK
C     PRIVATE
C
C$ Declarations
 
 
      INTEGER               HANDLE
      INTEGER               TREE
      INTEGER               VALUE
      INTEGER               KEY
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       I   Root of tree.
C     VALUE      I   Value to append.
C     KEY        O   Key pointing to new value.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     TREE           is the root node number of the tree of interest.
C
C     VALUE          is an integer value to be appended to the
C                    specified tree.
C
C$ Detailed_Output
C
C     KEY            is an absolute key indicating the insertion
C                    location.  In EK trees, absolute keys are just
C                    ordinal positions relative to the leftmost element
C                    of the tree, with the leftmost element having
C                    position 1.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If HANDLE is invalid, the error will be diagnosed by routines
C         called by this routine.  The file will not be modified.
C
C     2)  If an I/O error occurs while reading or writing the indicated
C         file, the error will be diagnosed by routines called by this
C         routine.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine appends a new value to an EK tree; this action is
C     equivalent to inserting the value at position (NKEYS+1), where
C     NKEYS is the number of keys in the tree prior to the insertion.
C
C     The tree is balanced and satisfies all invariants at the
C     completion of the appending.
C
C$ Examples
C
C     See EKBSEG.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume
C         3/Sorting and Searching" 1973, pp 471-479.
C
C         EK trees are closely related to the B* trees described by
C         Knuth.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 22-SEP-2004 (EDW)
C
C        Edited 1.0.1 Version entry to not include
C        the token used to mark the $Procedure section.
C
C-    Beta Version 1.0.1, 14-OCT-1996 (NJB)
C
C        $Procedure line was corrected.
C
C-    Beta Version 1.0.0, 22-OCT-1995 (NJB)
C
C-&
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKTRSZ
 
      KEY  =  ZZEKTRSZ ( HANDLE, TREE )   +   1
 
      CALL ZZEKTRIN ( HANDLE, TREE, KEY, VALUE )
 
      END
