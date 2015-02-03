C$Procedure ZZEKRPLK ( EK, look up record pointer )
 
      SUBROUTINE ZZEKRPLK ( HANDLE, SEGDSC, N, RECPTR )
 
C$ Abstract
C
C     Look up the record pointer of an EK record having a specified
C     ordinal position in a specified EK segment.
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
 
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'eksegdsc.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               N
      INTEGER               RECPTR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     SEGDSC     I   Segment descriptor.
C     N          I   Ordinal position of record.
C     RECPTR     O   Record pointer.
C
C$ Detailed_Input
C
C     HANDLE         is an EK file handle.  The file may be open for
C                    reading or writing.
C
C     SEGDSC         is the descriptor of the segment to which the
C                    record of interest belongs.
C
C     N              is the ordinal position of the record in the
C                    segment.
C
C$ Detailed_Output
C
C     RECPTR         is the record pointer corresponding to the input
C                    key.  This pointer identifies the record of
C                    interest.  The interpretation of RECPTR depends
C                    on the type of segment to which the record belongs.
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
C     2)  If N is out of range, the error will be diagnosed by
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
C     specified ordinal position in a segment.
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
      INTEGER               STYPE
      INTEGER               TREE
 
C
C     Use discovery check-in.
C
      STYPE  =  SEGDSC ( EKTIDX )
 
 
      IF ( STYPE .EQ. 1 ) THEN
C
C        For type 1 segments, the record pointer is obtained from
C        the record tree.
C
         TREE  = SEGDSC ( RTIDX )
 
         CALL ZZEKTRDP ( HANDLE, TREE, N, RECPTR )
 
 
      ELSE IF ( STYPE .EQ. 2 ) THEN
C
C        For type 2 segments, the record pointer *is* the ordinal
C        position of the record.
C
         RECPTR = N
 
      ELSE
C
C        Sorry, no other types of segments are supported.
C
         CALL CHKIN  ( 'ZZEKRPLK'                             )
         CALL SETMSG ( 'The segment type # is not supported.' )
         CALL ERRINT ( '#',   STYPE                           )
         CALL SIGERR ( 'SPICE(INVALIDTYPE)'                   )
         CALL CHKOUT ( 'ZZEKRPLK'                             )
         RETURN
 
      END IF
 
 
      END
