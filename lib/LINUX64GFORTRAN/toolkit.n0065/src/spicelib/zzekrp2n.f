C$Procedure      ZZEKRP2N ( EK, record pointer to number )
 
      INTEGER FUNCTION ZZEKRP2N ( HANDLE, SEGNO, RECPTR )
 
C$ Abstract
C
C     Find the EK record number corresponding to a specified record
C     pointer.  Beware, for type 1 segments, this is done by linear
C     searching.
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
 
      INCLUDE 'eksegdsc.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGNO
      INTEGER               RECPTR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     SEGNO      I   Segment number.
C     RECTPR     I   Record pointer.
C
C     The function returns the number of the record having the
C     specified record pointer.
C
C$ Detailed_Input
C
C     HANDLE         is a file handle of an EK open for read or write
C                    access.
C
C     SEGNO          is the number of the segment containing the
C                    record of interest.
C
C     RECPTR         is a record pointer.  The number of the record
C                    having this pointer is sought.
C
C$ Detailed_Output
C
C     The function returns the number of the record having the
C     specified record pointer.  The record should always be found.
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
C     2)  If SEGNO is invalid, the error will be diagnosed by routines
C         called by this routine.
C
C     3)  If an I/O error occurs while reading the indicated file,
C         the error will be diagnosed by routines called by this
C         routine.
C
C     4)  This routine should never be passed an input record pointer
C         that is not known to be valid.  If this error is trapped,
C         it is evidence of a bug.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This searches an EK record tree for the record number
C     corresponding to a specified record pointer.  Caution:  this
C     routine plods along in linear time.  It is intended primarily
C     for use in error handling.
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
C     Non-SPICELIB functions
C
      INTEGER               ZZEKTRLS
      LOGICAL               FAILED
 
C
C     Local variables
C
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               STYPE
      INTEGER               UNIT
 
C
C     Use discovery check-in.
C
      ZZEKRP2N  =  0
 
      CALL ZZEKSDSC ( HANDLE, SEGNO, SEGDSC )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
 
      STYPE = SEGDSC ( EKTIDX )
 
      IF ( STYPE .EQ. 1 ) THEN
 
         ZZEKRP2N  =  ZZEKTRLS ( HANDLE, SEGDSC(RTIDX), RECPTR )
 
         IF ( ZZEKRP2N .EQ. 0 ) THEN
 
            CALL DASHLU ( HANDLE, UNIT )
 
            CALL CHKIN  ( 'ZZEKRP2N'                              )
            CALL SETMSG ( 'Record having pointer # not found in ' //
     .                    'segment # of file #'                   )
            CALL ERRINT ( '#',  RECPTR                            )
            CALL ERRINT ( '#',  SEGNO                             )
            CALL ERRFNM ( '#',  UNIT                              )
            CALL SIGERR ( 'SPICE(BUG)'                            )
            CALL CHKOUT ( 'ZZEKRP2N'                              )
 
         END IF
 
 
      ELSE IF ( STYPE .EQ. 2 ) THEN
 
         ZZEKRP2N = RECPTR
 
      ELSE
 
         CALL DASHLU ( HANDLE, UNIT )
 
         CALL CHKIN  ( 'ZZEKRP2N'                                    )
         CALL SETMSG ( 'Segment type # is not supported.  SEGNO = #.'//
     .                 ' File = #.'                                  )
         CALL ERRINT ( '#',  STYPE                                   )
         CALL ERRINT ( '#',  SEGNO                                   )
         CALL ERRFNM ( '#',  UNIT                                    )
         CALL SIGERR ( 'SPICE(BUG)'                                  )
         CALL CHKOUT ( 'ZZEKRP2N'                                    )
 
      END IF
 
      RETURN
      END
