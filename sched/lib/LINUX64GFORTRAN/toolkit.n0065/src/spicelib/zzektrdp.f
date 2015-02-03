C$Procedure      ZZEKTRDP ( EK tree, return data pointer )
 
      SUBROUTINE ZZEKTRDP ( HANDLE, TREE, KEY, PTR )
 
C$ Abstract
C
C     Return the data pointer from a specified location in an EK tree.
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
      INTEGER               KEY
      INTEGER               PTR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       I   Root of tree.
C     KEY        I   Key corresponding to pointer.
C     PTR        O   Data pointer.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     TREE           is the root node number of the tree of interest.
C
C     KEY            is an absolute key indicating the location of
C                    the desired pointer.  In EK trees, absolute keys
C                    are just ordinal positions relative to the leftmost
C                    element of the tree, with the leftmost element
C                    having position 1.  So setting KEY to 10, for
C                    example, indicates that the output PTR is the 10th
C                    item in the tree.
C
C                    KEY must be in the range 1 : NKEYS, where
C                    NKEYS is the number of keys in the tree.
C
C$ Detailed_Output
C
C     PTR            is the integer value associated with the input key.
C                    Normally, this value is a data pointer.  However,
C                    the EK tree system makes no use of this fact, so
C                    PTR need not actually be a pointer.
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
C     3)  If the input key is out of range, the error is diagnosed by
C         routines called by this routine.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine obtains the value associated with a key, without
C     returning the metadata supplied by the lower-level lookup routine
C     ZZEKTRLK.
C
C$ Examples
C
C     See EKDELR.
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
C-    Beta Version 1.0.0, 26-OCT-1995 (NJB)
C
C-&
 
C
C     Local variables
C
      INTEGER               IDX
      INTEGER               NODE
      INTEGER               NOFFST
      INTEGER               LEVEL
 
C
C     Use discovery check-in in this puppy.
C
      CALL ZZEKTRLK ( HANDLE, TREE,   KEY,   IDX,
     .                NODE,   NOFFST, LEVEL, PTR  )
 
      RETURN
      END
