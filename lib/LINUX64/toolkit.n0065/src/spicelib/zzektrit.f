C$Procedure      ZZEKTRIT ( EK tree, initialize )
 
      SUBROUTINE ZZEKTRIT ( HANDLE, TREE )
 
C$ Abstract
C
C     Initialize an EK tree, returning the root of the tree.
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
 
      INCLUDE 'ekpage.inc'
      INCLUDE 'ektree.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               TREE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     TREE       O   Root of tree.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C$ Detailed_Output
C
C     TREE           is the root node number of the tree created by
C                    this routine.  The root node number is used by the
C                    EK tree routines to identify the tree.
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
C     This routine is used to create a new, empty EK tree.  The
C     tree has a root node, but no keys are contained in the root.
C     The metadata area of the tree is initialized.
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
C-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               BASE
      INTEGER               P
      INTEGER               PAGE  ( PGSIZI )
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKTRIT' )
      END IF
 
C
C     Start out by allocating a DAS integer page.  We'll write the root
C     node out to this page.
C
      CALL ZZEKPGAL ( HANDLE, INT, P, BASE )
 
      PAGE ( TRTYPE )  =  TRVERS
      PAGE ( TRNNOD )  =  1
      PAGE ( TRNKEY )  =  0
      PAGE ( TRNKR  )  =  0
      PAGE ( TRDPTH )  =  1
 
C
C     Set all keys to zero; set all child and data pointers to null.
C
      CALL CLEARI (  MXKEYR,  PAGE(TRKEYR+1)  )
      CALL CLEARI (  MXKEYR,  PAGE(TRDATR+1)  )
      CALL CLEARI (  MXKIDR,  PAGE(TRKIDR+1)  )
 
C
C     Write out the page.
C
      CALL ZZEKPGWI ( HANDLE, P, PAGE )
 
C
C     The identifier we return is just the page number of the tree's
C     root.
C
      TREE  =  P
 
 
      CALL CHKOUT ( 'ZZEKTRIT' )
      RETURN
      END
