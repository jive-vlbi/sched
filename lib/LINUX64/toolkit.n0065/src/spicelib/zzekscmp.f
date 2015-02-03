C$Procedure      ZZEKSCMP ( EK, scalar value comparison )
 
      LOGICAL FUNCTION ZZEKSCMP ( OP,
     .                            HANDLE, SEGDSC, COLDSC, ROW,   ELTIDX,
     .                            DTYPE,  CVAL,   DVAL,   IVAL,  NULL  )
 
C$ Abstract
C
C     Compare a specified scalar EK column entry with a scalar value.
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
C     PRIVATE
C     EK
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
      INCLUDE 'ekwild.inc'
 
      INTEGER               OP
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               ROW
      INTEGER               ELTIDX
      INTEGER               DTYPE
      CHARACTER*(*)         CVAL
      DOUBLE PRECISION      DVAL
      INTEGER               IVAL
      LOGICAL               NULL
 
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     OP         I   Relational operator code.
C     HANDLE     I   EK file handle.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     ROW        I   ID of row containing column entry to compare.
C     ELTIDX     I   Index of element in array-valued column entry.
C     DTYPE      I   Data type of input value.
C     CVAL       I   Character string to compare with column entry.
C     DVAL       I   D.p. value to compare with column entry.
C     IVAL       I   Integer value to compare with column entry.
C     NULL       I   Flag indicating whether scalar is null.
C
C     The function returns .TRUE. if and only if the specified column
C     entry and input value of the corresponding data type satisfy the
C     relation specified by the input argument OP.
C
C$ Detailed_Input
C
C     OP             is an integer code representing a binary relational
C                    operator.  The possible values of OP are the
C                    parameters
C
C                       EQ
C                       GE
C                       GT
C                       LE
C                       LIKE
C                       LT
C                       NE
C                       ISNULL
C                       NOTNUL
C
C
C     HANDLE         is an EK file handle.  The file may be open for
C                    reading or writing.
C
C     SEGDSC         is the EK segment descriptor of the column entry
C                    to be compared.
C
C     COLDSC         is an EK column descriptor for the column
C                    containing the entry to be compared.
C
C     ROW            is the identifier of the row containing the column
C                    entry to be compared. Note that these identifiers
C                    are polymorphic: their meaning is a function of
C                    the class of column that contains the entry of
C                    interest.
C
C     ELTIDX         is the index of the column entry element to be
C                    compared, if the column is array-valued.  ELTIDX
C                    is ignored for scalar columns.
C
C     DTYPE          is the data type of the input scalar value.
C
C
C     CVAL,
C     DVAL,
C     IVAL           are, respectively, character, double precision,
C                    and integer scalar variables.  The column entry
C                    is compared against whichever of these has the
C                    same data type as the entry; the other two
C                    variables are ignored.  If the data type of the
C                    column entry is TIME, the entry is compared with
C                    the variable DVAL.
C
C     NULL
C
C$ Detailed_Output
C
C     The function returns .TRUE. if and only if the specified column
C     entry and input value of the corresponding data type satisfy the
C     relation specified by the input argument OP.
C
C     If the specified column entry is null, it is considered to
C     precede all non-null values, and the logical value of the
C     expression
C
C        <column element> OP <value>
C
C     is determined accordingly.  Null character values do not satisfy
C     the relation
C
C        <null column element> LIKE <character value>
C
C     for any character value.
C
C$ Parameters
C
C     Within the EK system, relational operators used in EK queries are
C     represented by integer codes.  The codes and their meanings are
C     listed below.
C
C     Relational expressions in EK queries have the form
C
C        <column name> <operator> <value>
C
C     For columns containing numeric values, the operators
C
C        EQ,  GE,  GT,  LE,  LT,  NE
C
C     may be used; these operators have the same meanings as their
C     Fortran counterparts.  For columns containing character values,
C     the list of allowed operators includes those in the above list,
C     and in addition includes the operator
C
C        LIKE
C
C     which is used to compare strings to a template.  In the character
C     case, the meanings of the parameters
C
C        GE,  GT,  LE,  LT
C
C     match those of the Fortran lexical functions
C
C        LGE, LGT, LLE, LLT
C
C     Null values are considered to precede all non-null values.
C
C$ Exceptions
C
C     1)  If the input file handle is invalid, the error will be
C         diagnosed by routines called by this routine.
C         The function value is .FALSE. in this case.
C
C     2)  If an I/O error occurs while attempting to find the address
C         range of the specified column entry element, the error will
C         be diagnosed by routines called by this routine.  The
C         function value is .FALSE. in this case.
C
C     3)  If any of SEGDSC, COLDSC, or ROW are invalid, this routine
C         may fail in unpredictable, but possibly spectacular, ways.
C         Except as described in this header section, no attempt is
C         made to handle these errors.
C
C     4)  If the data type code in the input column descriptor is not
C         recognized, the error SPICE(INVALIDDATATYPE) is signalled.
C         The function value is .FALSE. in this case.
C
C     5)  If the specified column entry cannot be found, the error
C         SPICE(INVALIDINDEX) is signalled.  The function value is
C         .FALSE. in this case.
C
C     6)  If the relational operator code OP is not recognized, the
C         error SPICE(UNNATURALRELATION) is signalled.  The function
C         value is .FALSE. in this case.
C
C
C$ Files
C
C     See the description of the argument HANDLE in $Detailed_Input.
C
C$ Particulars
C
C     This routine is an EK utility intended to centralize a frequently
C     performed comparison operation.
C
C$ Examples
C
C     See ZZEKRMCH.
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
C-    SPICELIB Version 1.2.0, 31-MAY-2009 (NJB)
C
C        Bug fix: routine failed to account for the possibility
C        that scalar string column entries can have unlimited
C        length. Now at most the first MAXSTR characters of such
C        an entry are used in comparisons.
C
C-    SPICELIB Version 1.1.0, 21-DEC-2001 (NJB)
C
C        Bug fix:  routine now indicates "no match" when operator
C        is LIKE or UNLIKE and column entry is null.
C
C-    SPICELIB Version 1.0.0, 17-OCT-1995 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               MATCHI
 
C
C     Local variables
C
      CHARACTER*(MAXSTR)    ELTC
 
      DOUBLE PRECISION      ELTD
      DOUBLE PRECISION      NUMVAL
 
      INTEGER               CMPLEN
      INTEGER               CVLEN
      INTEGER               COLTYP
      INTEGER               ELTI
      INTEGER               REL
      INTEGER               STRLEN
      INTEGER               UNIT
 
      LOGICAL               ENULL
      LOGICAL               FOUND
 
 
C
C     Use discovery check-in for speed.
C
 
C
C     The function value defaults to .FALSE.
C
      ZZEKSCMP = .FALSE.
 
C
C     Look up the specified column element.
C
      COLTYP   =  COLDSC(TYPIDX)
 
      IF ( COLTYP .EQ. CHR ) THEN
C
C        We'll use at most the first MAXSTR characters of the input
C        string.
C
         CVLEN = MIN ( LEN(CVAL), MAXSTR )
 
C
C        Fetch the column entry to be compared. Note that ROW
C        is a polymorphic identifier. See ZZEKRSC for details
C        on how ROW is used.
C
         CALL ZZEKRSC (  HANDLE,  SEGDSC,  COLDSC,  ROW,
     .                   ELTIDX,  STRLEN,  ELTC,    ENULL, FOUND )

         IF ( FAILED() ) THEN
C
C           Don't check out here because we haven't checked in.
C
            RETURN
         END IF

C
C        Let CMPLEN be the string length to use in comparisons.
C
         IF (  FOUND  .AND.  ( .NOT. ENULL )  ) THEN

            CMPLEN = MIN ( STRLEN, MAXSTR )
         ELSE
            CMPLEN = 0
         END IF
 
      ELSE IF ( ( COLTYP .EQ. DP ) .OR. ( COLTYP .EQ. TIME )  ) THEN
 
         CALL ZZEKRSD (  HANDLE,  SEGDSC,  COLDSC, ROW,
     .                   ELTIDX,  ELTD,    ENULL,  FOUND )
 
 
      ELSE IF ( COLTYP .EQ. INT ) THEN
 
         CALL ZZEKRSI (  HANDLE,  SEGDSC,  COLDSC, ROW,
     .                   ELTIDX,  ELTI,    ENULL,  FOUND )
 
 
      ELSE
 
         CALL CHKIN  ( 'ZZEKSCMP'                         )
         CALL SETMSG ( 'Data type code # not recognized.' )
         CALL ERRINT ( '#',  COLTYP                       )
         CALL SIGERR ( 'SPICE(INVALIDDATATYPE)'           )
         CALL CHKOUT ( 'ZZEKSCMP'                         )
         RETURN
 
      END IF
 
 
      IF ( .NOT. FOUND ) THEN
 
         CALL DASHLU ( HANDLE, UNIT )
 
         CALL CHKIN  ( 'ZZEKSCMP'                                  )
         CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ELTIDX = #. '//
     .                 'Column entry element was not found.'       )
         CALL ERRFNM ( '#',  UNIT                                  )
         CALL ERRINT ( '#',  COLDSC(ORDIDX)                        )
         CALL ERRINT ( '#',  ROW                                   )
         CALL ERRINT ( '#',  ELTIDX                                )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                       )
         CALL CHKOUT ( 'ZZEKSCMP'                                  )
         RETURN
 
      END IF

C
C     Handle the ISNULL and NOTNUL operators, if perchance we see them.
C
      IF ( OP .EQ. ISNULL ) THEN
 
         ZZEKSCMP  =  ENULL
         RETURN
 
      ELSE IF ( OP .EQ. NOTNUL ) THEN
 
         ZZEKSCMP  =  .NOT. ENULL
         RETURN
 
      END IF
  
C
C     Find the order relation that applies to the input values.
C
C     Null values precede all others.
C
      IF ( ENULL )  THEN
 
         IF ( NULL ) THEN
            REL  =  EQ
         ELSE
            REL  =  LT
         END IF
 
 
      ELSE IF ( NULL ) THEN
 
         IF ( ENULL ) THEN
            REL  =  EQ
         ELSE
            REL  =  GT
         END IF
 
 
      ELSE
C
C
C        Compare the value we looked up with the input scalar value.
C
         IF (  COLTYP .EQ. CHR ) THEN
 
            IF ( DTYPE .NE. CHR ) THEN
 
               CALL CHKIN  ( 'ZZEKSCMP'                          )
               CALL SETMSG ( 'Column type is #; value type is #.')
               CALL ERRINT ( '#', COLTYP                         )
               CALL ERRINT ( '#', DTYPE                          )
               CALL SIGERR ( 'SPICE(BUG)'                        )
               CALL CHKOUT ( 'ZZEKSCMP'                          )
               RETURN
 
            END IF
 
            IF (  LLT ( ELTC(:CMPLEN), CVAL(:CVLEN) )  )THEN
               REL = LT
            ELSE IF (  LGT ( ELTC(:CMPLEN), CVAL(:CVLEN) )  ) THEN
               REL = GT
            ELSE
               REL = EQ
            END IF
 
 
         ELSE IF ( COLTYP .EQ. TIME ) THEN
 
            IF (  ( DTYPE .NE. TIME ) .AND. ( DTYPE .NE. DP )  ) THEN
 
               CALL CHKIN  ( 'ZZEKSCMP'                          )
               CALL SETMSG ( 'Column type is #; value type is #.')
               CALL ERRINT ( '#', COLTYP                         )
               CALL ERRINT ( '#', DTYPE                          )
               CALL SIGERR ( 'SPICE(BUG)'                        )
               CALL CHKOUT ( 'ZZEKSCMP'                          )
               RETURN
 
            END IF
 
            IF ( ELTD .LT. DVAL ) THEN
               REL = LT
            ELSE IF ( ELTD .GT. DVAL ) THEN
               REL = GT
            ELSE
               REL = EQ
            END IF
 
 
         ELSE IF ( COLTYP .EQ. DP ) THEN
 
            IF ( DTYPE .EQ. INT ) THEN
 
               NUMVAL = IVAL
 
 
            ELSE IF ( ( DTYPE .EQ. DP ) .OR. ( DTYPE .EQ. TIME ) ) THEN
 
               NUMVAL = DVAL
 
            ELSE
 
               CALL CHKIN  ( 'ZZEKSCMP'                          )
               CALL SETMSG ( 'Column type is #; value type is #.')
               CALL ERRINT ( '#', COLTYP                         )
               CALL ERRINT ( '#', DTYPE                          )
               CALL SIGERR ( 'SPICE(BUG)'                        )
               CALL CHKOUT ( 'ZZEKSCMP'                          )
               RETURN
 
            END IF
 
            IF ( ELTD .LT. NUMVAL ) THEN
               REL = LT
            ELSE IF ( ELTD .GT. NUMVAL ) THEN
               REL = GT
            ELSE
               REL = EQ
            END IF
 
 
         ELSE IF ( COLTYP .EQ. INT ) THEN
 
            IF ( DTYPE .EQ. INT ) THEN
 
               NUMVAL = IVAL
 
 
            ELSE IF ( DTYPE .EQ. DP ) THEN
 
               NUMVAL = DVAL
 
            ELSE
 
               CALL CHKIN  ( 'ZZEKSCMP'                          )
               CALL SETMSG ( 'Column type is #; value type is #.')
               CALL ERRINT ( '#', COLTYP                         )
               CALL ERRINT ( '#', DTYPE                          )
               CALL SIGERR ( 'SPICE(BUG)'                        )
               CALL CHKOUT ( 'ZZEKSCMP'                          )
               RETURN
 
            END IF
 
            IF ( ELTI .LT. NUMVAL ) THEN
               REL = LT
            ELSE IF ( ELTI .GT. NUMVAL ) THEN
               REL = GT
            ELSE
               REL = EQ
            END IF
 
 
         ELSE
C
C           Something untoward has happened in our column descriptor
C           argument.
C
            CALL CHKIN  ( 'ZZEKSCMP'                                )
            CALL SETMSG ( 'The data type code # was not recognized.')
            CALL ERRINT ( '#',  COLTYP                              )
            CALL SIGERR ( 'SPICE(INVALIDDATATYPE)'                  )
            CALL CHKOUT ( 'ZZEKSCMP'                                )
            RETURN
 
         END IF
 
      END IF
 
C
C     Determine the truth of the input relational expression.
C
      IF ( OP .EQ. EQ )   THEN
 
         ZZEKSCMP  =  REL .EQ. EQ
 
 
      ELSE IF ( OP .EQ. LT )   THEN
 
         ZZEKSCMP  =  REL .EQ. LT
 
 
      ELSE IF ( OP .EQ. LE )   THEN
 
         ZZEKSCMP  =  REL .NE. GT
 
 
      ELSE IF ( OP .EQ. GT )   THEN
 
         ZZEKSCMP  =  REL .EQ. GT
 
 
      ELSE IF ( OP .EQ. GE )   THEN
 
         ZZEKSCMP  =  REL .NE. LT
 
 
      ELSE IF ( OP .EQ. NE )   THEN
 
         ZZEKSCMP  =  REL .NE. EQ
 
 
      ELSE IF (  ( OP .EQ. LIKE ) .AND. ( DTYPE .EQ. CHR )  ) THEN
 
         IF ( NULL .OR. ENULL ) THEN
            ZZEKSCMP = .FALSE.
         ELSE
            ZZEKSCMP = MATCHI( ELTC(:CMPLEN), CVAL(:CVLEN), WSTR, WCHR )
         END IF
 
 
      ELSE IF (  ( OP .EQ. UNLIKE ) .AND. ( DTYPE .EQ. CHR )  ) THEN
 
         IF ( NULL .OR. ENULL ) THEN
            ZZEKSCMP = .FALSE.
         ELSE
            ZZEKSCMP = .NOT. 
     .                 MATCHI( ELTC(:CMPLEN), CVAL(:CVLEN), WSTR, WCHR )
         END IF
 
      ELSE
C
C        Sorry, we couldn't resist.
C
         CALL CHKIN  ( 'ZZEKSCMP'                                      )
         CALL SETMSG ( 'The relational operator # was not recognized '//
     .                 'or was not applicable for data type #.'        )
         CALL ERRINT ( '#',  OP                                        )
         CALL ERRINT ( '#',  DTYPE                                     )
         CALL SIGERR ( 'SPICE(UNNATURALRELATION)'                      )
         CALL CHKOUT ( 'ZZEKSCMP'                                      )
         RETURN
 
      END IF
 
      RETURN
      END
