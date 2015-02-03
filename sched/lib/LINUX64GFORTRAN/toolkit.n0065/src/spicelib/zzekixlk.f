C$Procedure ZZEKIXLK ( EK, look up record pointer in EK index )
 
      SUBROUTINE ZZEKIXLK ( HANDLE, COLDSC, KEY, RECPTR )
 
C$ Abstract
C
C     Look up a specified record pointer from an EK index.
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
 
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekpage.inc'
 
      INTEGER               HANDLE
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               KEY
      INTEGER               RECPTR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     COLDSC     I   Column descriptor.
C     KEY        I   Key.
C     RECPTR     O   Record pointer.
C
C$ Detailed_Input
C
C     HANDLE         is an EK file handle.  The file may be open for
C                    reading or writing.
C
C     COLDSC         is the column descriptor of the column to which
C                    the index of interest belongs.
C
C     KEY            is the key of the record pointer of interest.  This
C                    key is the ordinal position of the record pointer
C                    in the index.
C
C$ Detailed_Output
C
C     RECPTR         is the record pointer corresponding to the input
C                    key.  This pointer gives the base address of
C                    the record pointer structure for the record having
C                    ordinal position KEY within the specified column,
C                    where the order is defined by the column's index.
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
C     2)  If KEY is out of range, the error will be diagnosed by
C         routines called by this routine.
C
C     3)  If an I/O error occurs while reading or the indicated
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
C     This routine finds the record pointer for a record having a
C     specified ordinal position in a column, where the order is
C     defined by the column's index.
C
C$ Examples
C
C     See EKSRCH.
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
C-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
C
C-&
 
 
C
C     Local variables
C
      INTEGER               ADDRSS
      INTEGER               BASE
      INTEGER               ITYPE
      INTEGER               Q
      INTEGER               R
      INTEGER               TREE
 
C
C     Use discovery check-in.
C
      ITYPE  =  COLDSC ( IXTIDX )
 
 
      IF ( ITYPE .EQ. 1 ) THEN
C
C        For type 1 indexes, the index pointer is the root node of
C        a B*-tree.  Just use the tree look up routine.
C
         TREE  = COLDSC ( IXPIDX )
 
         CALL ZZEKTRDP ( HANDLE, TREE, KEY, RECPTR )
 
 
      ELSE IF ( ITYPE .EQ. 2 ) THEN
C
C        For type 2 indexes, the index pointer is the base address
C        of the index.  We must compute the offset from this base to
C        the index element having ordinal position KEY.
C
         BASE    =    COLDSC(IXPIDX)
         Q       =    (KEY-1)  / IPSIZE
         R       =    KEY  - Q * IPSIZE
         ADDRSS  =    BASE + Q * PGSIZI + R
 
         CALL DASRDI ( HANDLE, ADDRSS, ADDRSS, RECPTR )
 
      ELSE
C
C        Sorry, no other types of indexes are supported.
C
         CALL CHKIN  ( 'ZZEKIXLK'                           )
         CALL SETMSG ( 'The index type # is not supported.' )
         CALL ERRINT ( '#',   ITYPE                         )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                 )
         CALL CHKOUT ( 'ZZEKIXLK'                           )
         RETURN
 
      END IF
 
 
      END
