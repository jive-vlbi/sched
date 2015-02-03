C$Procedure      ZZEKTRSZ ( EK, tree size )
 
      INTEGER FUNCTION ZZEKTRSZ ( HANDLE, TREE )
 
C$ Abstract
C
C     Return the number of keys in a tree.
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
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       I   Root of tree.
C
C     The function returns the total number of keys in the specified
C     EK tree.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for read or write
C                    access.
C
C     TREE           is the root node number of the tree of interest.
C
C$ Detailed_Output
C
C     The function returns the total number of keys in the specified
C     EK tree.
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
C     2)  If an I/O error occurs while reading the indicated file, the
C         error will be diagnosed by routines called by this routine.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine should be used to determine the current size of an
C     EK tree; the tree's metadata should not be accessed directly to
C     extract this information.
C
C$ Examples
C
C     See EKNSEG.
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
C-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
C
C-&
 
 
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKTRBS
 
C
C     Local variables
C
      INTEGER               ADDRSS
 
C
C     Go straight to the address at which the key count is stored.
C
      ADDRSS  =  ZZEKTRBS ( TREE )  +  TRNKEY
 
      CALL DASRDI ( HANDLE, ADDRSS, ADDRSS, ZZEKTRSZ )
 
      RETURN
      END
