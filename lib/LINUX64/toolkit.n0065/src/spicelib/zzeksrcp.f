C$Procedure      ZZEKSRCP ( EK, set record companion pointer )
 
      SUBROUTINE ZZEKSRCP ( HANDLE, RECPTR, RECNO )
 
C$ Abstract
C
C     Set the companion pointer of a specified EK record.
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
 
      INCLUDE 'ekrecptr.inc'
 
      INTEGER               HANDLE
      INTEGER               RECPTR
      INTEGER               RECNO
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     RECPTR     I   Record pointer.
C     RECNO      I   Record number of companion of specified EK record.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     RECPTR         is a pointer to the record whose companion pointer
C                    is to be set.
C
C     RECNO          is the `companion record pointer' of the record
C                    designated by RECPTR.  This name is a little
C                    misleading:  we identify the companion record
C                    by its record number, not by an address.  So RECNO
C                    is a pointer only in an abstract sense.
C
C                    If the input record belongs to a shadowed EK, the
C                    companion record is the corresponding backup
C                    record.  If the input record is a backup record,
C                    the companion record is the corresponding source
C                    record.
C
C$ Detailed_Output
C
C     None.  See $Particulars for a description of the action of this
C     routine.
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
C     This routine sets the companion record pointer of a specified EK
C     record.  As noted in $Detailed_Input, the `pointer' is simply
C     the number of the companion record.
C
C$ Examples
C
C     See ZZEKRBCK.
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
C-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Local variables
C
      INTEGER               LOC
 
C
C     Use discovery check-in.
C
C
C     Is this file handle valid--is the file open for paged write
C     access?  Signal an error if not.
C
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
C
C     Compute the companion pointer and set the pointer.
C
      LOC  =  RECPTR + RCPIDX
 
      CALL DASUDI ( HANDLE, LOC, LOC, RECNO )
 
      RETURN
      END
