C$Procedure     ZZEKFRX ( EK, find record in index )
 
      SUBROUTINE ZZEKFRX ( HANDLE, SEGDSC, COLDSC, RECPTR, POS )
 
C$ Abstract
C
C     Find the ordinal position of a specified record in a specified,
C     indexed EK column.
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
C     None.
C
C$ Keywords
C
C     PRIVATE
C
C$ Declarations
 
 
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECPTR
      INTEGER               POS
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     RECPTR     I   Pointer to record to locate.
C     POS        O   Ordinal position of record.
C
C$ Detailed_Input
C
C     HANDLE         is an EK file handle.  The file may be open for
C                    reading or writing.
C
C     SEGDSC         is the segment descriptor of the segment
C                    containing the column to be searched.
C
C     COLDSC         is the column descriptor of the column to be
C                    searched.
C
C     RECPTR         is a pointer to the record whose ordinal position
C                    is to be found.
C
C$ Detailed_Output
C
C     POS            is the ordinal position in the specified column
C                    of the input record, where the order relation is
C                    specified by the column's index.
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
C     2)  If an I/O error occurs while reading the indicated
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
C     Various EK write operations require the capability of locating
C     the index key that maps to a given record number.  An example is
C     updating a column's index to reflect deletion of a specified
C     record:  the key that maps to the record must be deleted.
C     Locating this key is the inverse of the problem that the index
C     is meant to solve.
C
C$ Examples
C
C     See ZZEKIXDL.
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
C-    SPICELIB Version 2.0.0, 31-MAY-2010 (NJB)
C
C        Bug fix: substring bound out-of-range violation
C        in reference to local variable CVAL has been 
C        corrected. This error could occur if the a 
C        class 3 column entry had length exceeding MAXSTR.
C
C-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKRP2N
 
C
C     Local variables
C
      CHARACTER*(MAXSTR)    CVAL
 
      DOUBLE PRECISION      DVAL

      INTEGER               CMPLEN
      INTEGER               CVLEN
      INTEGER               DTYPE
      INTEGER               IVAL
      INTEGER               PRVPTR
      INTEGER               RECNO
      INTEGER               UNIT
 
      LOGICAL               FOUND
      LOGICAL               ISNULL
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKFRX' )
      END IF
 
C
C     Determine the data type of the column, and look up the value
C     associated with RECPTR.
C
      DTYPE  =  COLDSC ( TYPIDX )
 
      IF ( DTYPE .EQ. CHR ) THEN
 
         CALL ZZEKRSC (  HANDLE,  SEGDSC,  COLDSC,  RECPTR,
     .                   1,       CVLEN,   CVAL,    ISNULL,  FOUND  )
         
 
         IF (  FOUND  .AND. ( .NOT. ISNULL )  ) THEN

            CMPLEN = MIN ( CVLEN, MAXSTR )
         ELSE
            CMPLEN = 0
         END IF


      ELSE IF (  ( DTYPE .EQ. DP ) .OR. ( DTYPE .EQ. TIME )  ) THEN
 
         CALL ZZEKRSD (  HANDLE,  SEGDSC,  COLDSC,  RECPTR,
     .                   1,       DVAL,    ISNULL,  FOUND  )
 
 
      ELSE IF ( DTYPE .EQ. INT ) THEN
 
         CALL ZZEKRSI (  HANDLE,  SEGDSC,  COLDSC,  RECPTR,
     .                   1,       IVAL,    ISNULL,  FOUND  )
 
      ELSE
 
         CALL DASHLU ( HANDLE,  UNIT  )
         CALL SETMSG ( 'File = #; COLIDX = #. Unrecognized data ' //
     .                 'type code # found in descriptor.'           )
         CALL ERRFNM ( '#',     UNIT                                )
         CALL ERRINT ( '#',     COLDSC(ORDIDX)                      )
         CALL ERRINT ( '#',     DTYPE                               )
         CALL SIGERR ( 'SPICE(ITEMNOTFOUND)'                        )
         CALL CHKOUT ( 'ZZEKFRX'                                    )
         RETURN
 
      END IF
 
 
 
      IF ( .NOT. FOUND ) THEN
C
C        We have a most heinous situation.  We should always be able
C        to find the value associated with a record.
C
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
         CALL DASHLU (  HANDLE,  UNIT  )
 
         CALL SETMSG ( 'File = #; RECNO = #; COLIDX = #. Column '     //
     .                 'entry was not found.  This probably indicates'//
     .                 ' a corrupted file or a bug in the EK code.'    )
         CALL ERRFNM ( '#',     UNIT                                   )
         CALL ERRINT ( '#',     RECNO                                  )
         CALL ERRINT ( '#',     COLDSC(ORDIDX)                         )
         CALL SIGERR ( 'SPICE(ITEMNOTFOUND)'                           )
         CALL CHKOUT ( 'ZZEKFRX'                                       )
         RETURN
 
      END IF
 
C
C     Find the last column entry less than or equal to the one
C     associated with the input record, where the order relation is
C     dictionary ordering on (<column value>, <record number>) pairs.
C     These ordered pairs are distinct, even if the column entries
C     are not.  Therefore, the ordinal position POS will actually be
C     the ordinal position of our record.
C
      IF ( DTYPE .EQ. CHR ) THEN
 
         CALL ZZEKLERC ( HANDLE,  SEGDSC,  COLDSC,  CVAL(:CMPLEN),
     .                   RECPTR,  ISNULL,  POS,     PRVPTR         )
 
 
      ELSE IF (  ( DTYPE .EQ. DP ) .OR. ( DTYPE .EQ. TIME )  ) THEN
 
         CALL ZZEKLERD ( HANDLE,  SEGDSC,  COLDSC,  DVAL,
     .                   RECPTR,  ISNULL,  POS,     PRVPTR  )
 
 
      ELSE
C
C        The data type is INT.  (We've already checked for invalid
C        types.)
C
 
         CALL ZZEKLERI ( HANDLE,  SEGDSC,  COLDSC,  IVAL,
     .                   RECPTR,  ISNULL,  POS,     PRVPTR  )
 
      END IF
 
 
      IF ( PRVPTR .NE. RECPTR ) THEN
C
C        Big problem.  This should never happen.
C
         RECNO  =  ZZEKRP2N ( HANDLE, SEGDSC(SNOIDX), RECPTR )
         CALL DASHLU (  HANDLE,  UNIT  )
 
         CALL SETMSG ( 'File = #; RECNO = #; COLIDX = #.  Record ' //
     .                 'that was last less than or equal to RECNO '//
     .                 'was not equal to RECNO.  This probably '   //
     .                 'indicates  a corrupted file or a bug in '  //
     .                 'the EK code.'                               )
         CALL ERRFNM ( '#',     UNIT                                )
         CALL ERRINT ( '#',     RECNO                               )
         CALL ERRINT ( '#',     COLDSC(ORDIDX)                      )
         CALL SIGERR ( 'SPICE(ITEMNOTFOUND)'                        )
         CALL CHKOUT ( 'ZZEKFRX'                                    )
         RETURN
 
      END IF
 
 
      CALL CHKOUT ( 'ZZEKFRX' )
      RETURN
      END
