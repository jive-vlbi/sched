C$Procedure      ZZEKTRLS ( EK tree, linear search )
 
      INTEGER FUNCTION ZZEKTRLS ( HANDLE, TREE, IVAL )
 
C$ Abstract
C
C     Search an EK tree linearly to find a specified data value.  The
C     function returns the index at which the value is found.
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
 
      INCLUDE 'ektree.inc'
 
      INTEGER               HANDLE
      INTEGER               TREE
      INTEGER               IVAL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       I   Tree root.
C     IVAL       I   Value to search for.
C
C     The function returns the lowest index at which the input value
C     is found, or zero if the value is not found.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for read or write
C                    access.
C
C     TREE           is the root node of the tree to search.
C
C     IVAL           is the value to search for.
C
C$ Detailed_Output
C
C     The function returns the lowest index at which the input value
C     is found, or zero if the value is not found.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If HANDLE is invalid, the error will be diagnosed by routines
C         called by this routine.
C
C     2)  If TREE is invalid, strange errors may result.
C
C     3)  If an I/O error occurs while reading the indicated file,
C         the error will be diagnosed by routines called by this
C         routine.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This searches a tree for a specified value.  It is an approximate
C     inverse of ZZEKTRLK.  However, ZZEKTRLK operates in logarithmic
C     time (as a function of the tree's size), while this function
C     plods along in linear time.
C
C$ Examples
C
C     See ZZEKRP2N.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
C
C-&
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKTRSZ
C
C     Local variables
C
      INTEGER               I
      INTEGER               N
      INTEGER               VALUE
 
C
C     Use discovery check-in.
C
      ZZEKTRLS  =  0
      N         =  ZZEKTRSZ ( HANDLE, TREE )
 
      DO I = 1, N
 
         CALL ZZEKTRDP ( HANDLE, TREE, I, VALUE )
 
         IF ( IVAL .EQ. VALUE ) THEN
            ZZEKTRLS = I
            RETURN
         END IF
 
      END DO
 
      RETURN
      END
