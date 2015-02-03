C$Procedure      ZZEKECMP ( EK, column entry element comparison )
 
      INTEGER FUNCTION ZZEKECMP ( HANS,  SGDSCS,  CLDSCS,  ROWS,  ELTS )
 
C$ Abstract
C
C     Compare two column entry elements, and return the relation of the
C     first to the second:  LT, EQ, or GT.
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
C     COMPARE
C     EK
C     UTILITY
C
C$ Declarations
 
 
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               HANS   ( * )
      INTEGER               SGDSCS ( SDSCSZ, * )
      INTEGER               CLDSCS ( CDSCSZ, * )
      INTEGER               ROWS   ( * )
      INTEGER               ELTS   ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANS       I   EK handles.
C     SGDSCS     I   Segment descriptors.
C     CLDSCS     I   Column descriptors.
C     ROWS       I   Row numbers.
C     ELTS       I   Element indices.
C
C     The function returns a parameter indicating the order relation
C     satisfied by the input arguments.  Possible values are LT, EQ,
C     and GT.
C
C$ Detailed_Input
C
C     HANS           is an array containing file handles of two EKs
C                    containing column entry elements to be compared.
C
C     SGDSCS         is an array containing segment descriptors of
C                    the segments that contain the elements to be
C                    compared.
C
C     CLDSCS         is an array containing column descriptors for the
C                    columns containing the elements to be compared.
C
C     ROWS           is an array containing row numbers of the
C                    elements to be compared.
C
C     ELTS           is an array containing element indices of the
C                    elements to be compared.  These indices locate
C                    an element within the column entry it belongs to.
C
C$ Detailed_Output
C
C     The function returns a parameter indicating the order relation
C     satisfied by the input arguments.  Possible values are LT, EQ,
C     and GT.  If OP is the returned value, the scalar values
C     specified by the input arguments satisfy the relation
C
C        <row 1> OP <row 2>
C
C$ Parameters
C
C     See the include file ekopcd.inc.
C
C$ Exceptions
C
C     1)  If the either of input file handles is invalid, the error
C         will be diagnosed by routines called by this routine.
C         The function value is EQ in this case.
C
C     2)  If an I/O error occurs while attempting to look up
C         the specified column entry elements, the error will
C         be diagnosed by routines called by this routine.  The
C         function value is EQ in this case.
C
C     3)  If any of the input segment descriptors, column descriptors,
C         or row numbers are invalid, this routine may fail in
C         unpredictable, but possibly spectacular, ways.  Except
C         as described in this header section, no attempt is made to
C         handle these errors.
C
C     4)  If the data type code in the input column descriptor is not
C         recognized, the error SPICE(INVALIDDATATYPE) is signalled.
C         The function value is EQ in this case.
C
C$ Files
C
C     See the descriptions of the arguments HAN(1) and HAN(2) in
C     $Detailed_Input.
C
C$ Particulars
C
C     This routine is an EK utility intended to centralize a frequently
C     performed comparison operation.
C
C$ Examples
C
C     See ZZEKRCMP, ZZEKVCMP, ZZEKVMCH.
C
C$ Restrictions
C
C     1)  This routine must execute quickly.  Therefore, it checks in
C         only if it detects an error.  If an error is signalled by a
C         routine called by this routine, this routine will not appear
C         in the SPICELIB traceback display.  Also, in the interest
C         of speed, this routine does not test the value of the SPICELIB
C         function RETURN upon entry.
C
C     2)  This routine depends on the requested comparison to have
C         been semantically checked. Semantically invalid comparisons
C         are treated as bugs.
C
C     3)  Only the first MAXSTR characters of character strings are
C         used in comparisons.
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
C-    SPICELIB Version 1.1.0, 26-MAY-2010 (NJB)
C
C        Bug fix: subscript out of range error caused by
C        column entry strings longer than MAXLEN has been
C        corrected. Also updated Restrictions header section.
C
C-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
C
C-&
 
C
C     Local variables
C
      CHARACTER*(MAXSTR)    CVAL   ( 2 )
 
      DOUBLE PRECISION      DVAL   ( 2 )
 
      INTEGER               CMPLEN ( 2 )
      INTEGER               CVLEN  ( 2 )
      INTEGER               I
      INTEGER               IVAL   ( 2 )
      INTEGER               LHSTYP
      INTEGER               RHSTYP
      INTEGER               UNIT
 
      LOGICAL               FOUND
      LOGICAL               NULL   ( 2 )
 
 
C
C     Use discovery check-in for speed.
C
 
C
C     The function value defaults to `equal'.
C
      ZZEKECMP  =  EQ
 
      LHSTYP    =  CLDSCS( TYPIDX, 1 )
      RHSTYP    =  CLDSCS( TYPIDX, 2 )
 
 
      IF (  LHSTYP .EQ. INT ) THEN
C
C        The entities we're comparing are supposed to be
C        scalar.  The left hand side has integer type.  Either
C        integer or double precision types are acceptable on
C        the right hand side.
C
         CALL ZZEKRSI (  HANS( 1),  SGDSCS(1,1),  CLDSCS(1,1),  ROWS(1),
     .                   ELTS(1),   IVAL(1),      NULL(1),      FOUND  )
 
 
         IF ( .NOT. FOUND ) THEN
 
            CALL DASHLU ( HANS(1), UNIT )
 
            CALL CHKIN  ( 'ZZEKECMP'                                  )
            CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ELTIDX = #. '//
     .                    'Column entry element was not found.'       )
            CALL ERRFNM ( '#',  UNIT                                  )
            CALL ERRINT ( '#',  CLDSCS(ORDIDX,1)                      )
            CALL ERRINT ( '#',  ROWS(1)                               )
            CALL ERRINT ( '#',  ELTS(1)                               )
            CALL SIGERR ( 'SPICE(INVALIDINDEX)'                       )
            CALL CHKOUT ( 'ZZEKECMP'                                  )
            RETURN
 
         END IF
 
 
         IF ( RHSTYP .EQ. INT ) THEN
 
 
            CALL ZZEKRSI ( HANS(2), SGDSCS(1,2), CLDSCS(1,2), ROWS(2),
     .                     ELTS(1), IVAL(2),     NULL(2),     FOUND   )
 
            IF ( .NOT. FOUND ) THEN
 
               CALL DASHLU ( HANS(2), UNIT )
 
               CALL CHKIN  ( 'ZZEKECMP'                                )
               CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ELTIDX  '  //
     .                       '= #.Column entry element was not found.' )
               CALL ERRFNM ( '#',  UNIT                                )
               CALL ERRINT ( '#',  CLDSCS(ORDIDX,2)                    )
               CALL ERRINT ( '#',  ROWS(2)                             )
               CALL ERRINT ( '#',  ELTS(2)                             )
               CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
               CALL CHKOUT ( 'ZZEKECMP'                                )
               RETURN
 
            END IF
 
C
C           Null values precede all others.
C
            IF ( NULL(1) .OR. NULL(2) )  THEN
 
               IF ( .NOT. NULL(2) ) THEN
                  ZZEKECMP = LT
               ELSE IF ( .NOT. NULL(1) ) THEN
                  ZZEKECMP = GT
               END IF
 
            ELSE
 
               IF ( IVAL(1) .LT. IVAL(2) ) THEN
                  ZZEKECMP = LT
               ELSE IF ( IVAL(1) .GT. IVAL(2) ) THEN
                  ZZEKECMP = GT
               END IF
 
            END IF
 
 
         ELSE IF ( RHSTYP .EQ. DP ) THEN
 
            CALL ZZEKRSD ( HANS(2), SGDSCS(1,2), CLDSCS(1,2), ROWS(2),
     .                     ELTS(1), DVAL(2),     NULL(2),     FOUND   )
 
            IF ( .NOT. FOUND ) THEN
 
               CALL DASHLU ( HANS(2), UNIT )
 
               CALL CHKIN  ( 'ZZEKECMP'                                )
               CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ELTIDX  '  //
     .                       '= #.Column entry element was not found.' )
               CALL ERRFNM ( '#',  UNIT                                )
               CALL ERRINT ( '#',  CLDSCS(ORDIDX,2)                    )
               CALL ERRINT ( '#',  ROWS(2)                             )
               CALL ERRINT ( '#',  ELTS(2)                             )
               CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
               CALL CHKOUT ( 'ZZEKECMP'                                )
               RETURN
 
            END IF
 
 
            IF ( NULL(1) .OR. NULL(2) )  THEN
 
               IF ( .NOT. NULL(2) ) THEN
                  ZZEKECMP = LT
               ELSE IF ( .NOT. NULL(1) ) THEN
                  ZZEKECMP = GT
               END IF
 
            ELSE
 
               IF ( IVAL(1) .LT. DVAL(2) ) THEN
                  ZZEKECMP = LT
               ELSE IF ( IVAL(1) .GT. DVAL(2) ) THEN
                  ZZEKECMP = GT
               END IF
 
            END IF
 
 
         ELSE
C
C           This is a big-time semantic error.  We should
C           never arrive here.
C
            CALL CHKIN  ( 'ZZEKECMP'                         )
            CALL SETMSG ( 'LHS data type is #; RHSTYP is #.' )
            CALL ERRINT ( '#', LHSTYP                        )
            CALL ERRINT ( '#', RHSTYP                        )
            CALL SIGERR ( 'SPICE(BUG)'                       )
            CALL CHKOUT ( 'ZZEKECMP'                         )
            RETURN
 
         END IF
 
 
 
      ELSE IF ( LHSTYP .EQ. DP ) THEN
C
C        This is a mirror image of the INT case.
C
         CALL ZZEKRSD ( HANS(1),  SGDSCS(1,1),  CLDSCS(1,1),  ROWS(1),
     .                  ELTS(1),  DVAL(1),      NULL(1),      FOUND   )
 
 
         IF ( .NOT. FOUND ) THEN
 
            CALL DASHLU ( HANS(1), UNIT )
 
            CALL CHKIN  ( 'ZZEKECMP'                                  )
            CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ELTIDX = #. '//
     .                    'Column entry element was not found.'       )
            CALL ERRFNM ( '#',  UNIT                                  )
            CALL ERRINT ( '#',  CLDSCS(ORDIDX,1)                      )
            CALL ERRINT ( '#',  ROWS(1)                               )
            CALL ERRINT ( '#',  ELTS(1)                               )
            CALL SIGERR ( 'SPICE(INVALIDINDEX)'                       )
            CALL CHKOUT ( 'ZZEKECMP'                                  )
            RETURN
 
         END IF
 
 
         IF ( RHSTYP .EQ. INT ) THEN
 
            CALL ZZEKRSI ( HANS(2), SGDSCS(1,2), CLDSCS(1,2), ROWS(2),
     .                     ELTS(1), IVAL(2),     NULL(2),     FOUND   )
 
            IF ( .NOT. FOUND ) THEN
 
               CALL DASHLU ( HANS(2), UNIT )
 
               CALL CHKIN  ( 'ZZEKECMP'                                )
               CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ELTIDX  '  //
     .                       '= #.Column entry element was not found.' )
               CALL ERRFNM ( '#',  UNIT                                )
               CALL ERRINT ( '#',  CLDSCS(ORDIDX,2)                    )
               CALL ERRINT ( '#',  ROWS(2)                             )
               CALL ERRINT ( '#',  ELTS(2)                             )
               CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
               CALL CHKOUT ( 'ZZEKECMP'                                )
               RETURN
 
            END IF
 
C
C           Null values precede all others.
C
            IF ( NULL(1) .OR. NULL(2) )  THEN
 
               IF ( .NOT. NULL(2) ) THEN
                  ZZEKECMP = LT
               ELSE IF ( .NOT. NULL(1) ) THEN
                  ZZEKECMP = GT
               END IF
 
            ELSE
 
               IF ( DVAL(1) .LT. IVAL(2) ) THEN
                  ZZEKECMP = LT
               ELSE IF ( DVAL(1) .GT. IVAL(2) ) THEN
                  ZZEKECMP = GT
               END IF
 
            END IF
 
 
         ELSE IF ( RHSTYP .EQ. DP ) THEN
 
 
            CALL ZZEKRSD ( HANS(2), SGDSCS(1,2), CLDSCS(1,2), ROWS(2),
     .                     ELTS(1), DVAL(2),     NULL(2),     FOUND   )
 
            IF ( .NOT. FOUND ) THEN
 
               CALL DASHLU ( HANS(2), UNIT )
 
               CALL CHKIN  ( 'ZZEKECMP'                                )
               CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ELTIDX  '  //
     .                       '= #.Column entry element was not found.' )
               CALL ERRFNM ( '#',  UNIT                                )
               CALL ERRINT ( '#',  CLDSCS(ORDIDX,2)                    )
               CALL ERRINT ( '#',  ROWS(2)                             )
               CALL ERRINT ( '#',  ELTS(2)                             )
               CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
               CALL CHKOUT ( 'ZZEKECMP'                                )
               RETURN
 
            END IF
 
 
            IF ( NULL(1) .OR. NULL(2) )  THEN
 
               IF ( .NOT. NULL(2) ) THEN
                  ZZEKECMP = LT
               ELSE IF ( .NOT. NULL(1) ) THEN
                  ZZEKECMP = GT
               END IF
 
            ELSE
 
               IF ( DVAL(1) .LT. DVAL(2) ) THEN
                  ZZEKECMP = LT
               ELSE IF ( DVAL(1) .GT. DVAL(2) ) THEN
                  ZZEKECMP = GT
               END IF
 
            END IF
 
 
         ELSE
C
C           This is a big-time semantic error.  We should
C           never arrive here.
C
            CALL CHKIN  ( 'ZZEKECMP'                         )
            CALL SETMSG ( 'LHS data type is #; RHSTYP is #.' )
            CALL ERRINT ( '#', LHSTYP                        )
            CALL ERRINT ( '#', RHSTYP                        )
            CALL SIGERR ( 'SPICE(BUG)'                       )
            CALL CHKOUT ( 'ZZEKECMP'                         )
            RETURN
 
         END IF
 
 
 
      ELSE IF ( LHSTYP .EQ. TIME ) THEN
C
C        The entities we're comparing are supposed to be time values.
C
         IF ( RHSTYP .NE. TIME ) THEN
C
C           This is a big-time semantic error.  We should
C           never arrive here.
C
            CALL CHKIN  ( 'ZZEKECMP'                         )
            CALL SETMSG ( 'LHS data type is #; RHSTYP is #.' )
            CALL ERRINT ( '#', LHSTYP                        )
            CALL ERRINT ( '#', RHSTYP                        )
            CALL SIGERR ( 'SPICE(BUG)'                       )
            CALL CHKOUT ( 'ZZEKECMP'                         )
            RETURN
 
         END IF
 
 
         DO I = 1, 2
 
            CALL ZZEKRSD ( HANS(I),  SGDSCS(1,I), CLDSCS(1,I), ROWS(I),
     .                     ELTS(I),  DVAL(I),     NULL(I),     FOUND   )
 
 
            IF ( .NOT. FOUND ) THEN
 
               CALL DASHLU ( HANS(I), UNIT )
 
               CALL CHKIN  ( 'ZZEKECMP'                                )
               CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ELTIDX  '  //
     .                       '= #.Column entry element was not found.' )
               CALL ERRFNM ( '#',  UNIT                                )
               CALL ERRINT ( '#',  CLDSCS(ORDIDX,I)                    )
               CALL ERRINT ( '#',  ROWS(I)                             )
               CALL ERRINT ( '#',  ELTS(I)                             )
               CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
               CALL CHKOUT ( 'ZZEKECMP'                                )
               RETURN
 
            END IF
 
         END DO
 
 
         IF ( NULL(1) .OR. NULL(2) )  THEN
 
            IF ( .NOT. NULL(2) ) THEN
               ZZEKECMP = LT
            ELSE IF ( .NOT. NULL(1) ) THEN
               ZZEKECMP = GT
            END IF
 
         ELSE
 
            IF ( DVAL(1) .LT. DVAL(2) ) THEN
               ZZEKECMP = LT
            ELSE IF ( DVAL(1) .GT. DVAL(2) ) THEN
               ZZEKECMP = GT
            END IF
 
         END IF
 
 
      ELSE IF ( LHSTYP .EQ. CHR ) THEN
C
C        The entities we're comparing are supposed to be scalar.
C
         IF ( RHSTYP .NE. CHR ) THEN
C
C           You know what kind of semantic error this is.
C
            CALL CHKIN  ( 'ZZEKECMP'                         )
            CALL SETMSG ( 'LHS data type is #; RHSTYP is #.' )
            CALL ERRINT ( '#', LHSTYP                        )
            CALL ERRINT ( '#', RHSTYP                        )
            CALL SIGERR ( 'SPICE(BUG)'                       )
            CALL CHKOUT ( 'ZZEKECMP'                         )
            RETURN
 
         END IF
 
 
         DO I = 1, 2
 
            CALL ZZEKRSC (  HANS  (  I),
     .                      SGDSCS(1,I),
     .                      CLDSCS(1,I),
     .                      ROWS(I),
     .                      ELTS(I),
     .                      CVLEN(I),
     .                      CVAL(I),
     .                      NULL(I),
     .                      FOUND           )
 
 
            IF ( .NOT. FOUND ) THEN
 
               CALL DASHLU ( HANS(I), UNIT )
 
               CALL CHKIN  ( 'ZZEKECMP'                                )
               CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ELTIDX  '  //
     .                       '= #.Column entry element was not found.' )
               CALL ERRFNM ( '#',  UNIT                                )
               CALL ERRINT ( '#',  CLDSCS(ORDIDX,I)                    )
               CALL ERRINT ( '#',  ROWS(I)                             )
               CALL ERRINT ( '#',  ELTS(I)                             )
               CALL SIGERR ( 'SPICE(INVALIDINDEX)'                     )
               CALL CHKOUT ( 'ZZEKECMP'                                )
               RETURN
 
            END IF

C
C           Let CMPLEN(I) be the string length to use in comparisons.
C
            CMPLEN(I) = MIN ( CVLEN(I), MAXSTR )
 
         END DO
 
 
         IF ( NULL(1) .OR. NULL(2) )  THEN
 
            IF ( .NOT. NULL(2) ) THEN
               ZZEKECMP = LT
            ELSE IF ( .NOT. NULL(1) ) THEN
               ZZEKECMP = GT
            END IF
 
         ELSE
 
 
            IF (   LLT (   CVAL(1)( :CMPLEN(1) ),
     .                     CVAL(2)( :CMPLEN(2) )  )   ) THEN
 
               ZZEKECMP = LT
 
            ELSE IF (   LGT (  CVAL(1)( :CMPLEN(1) ),
     .                         CVAL(2)( :CMPLEN(2) )  )   ) THEN
               ZZEKECMP = GT
            ELSE
               ZZEKECMP = EQ
            END IF
 
         END IF
 
 
      ELSE
C
C        Something untoward has happened in our descriptor.
C
         CALL CHKIN  ( 'ZZEKECMP'                        )
         CALL SETMSG ( 'The data type code # was not '  //
     .                 'recognized.'                     )
         CALL ERRINT ( '#',  LHSTYP                      )
         CALL SIGERR ( 'SPICE(INVALIDDATATYPE)'          )
         CALL CHKOUT ( 'ZZEKECMP'                        )
         RETURN
 
      END IF
 
 
      RETURN
      END
