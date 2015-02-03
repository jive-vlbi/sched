C$Procedure  ZZEKTRES ( Private: EK, resolve times in encoded query )
 
      SUBROUTINE ZZEKTRES ( QUERY,  EQRYI,  EQRYC,   EQRYD,
     .                              ERROR,  ERRMSG,  ERRPTR  )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Resolve time values in an encoded EK query.
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
 
      INCLUDE 'ekattdsc.inc'
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         QUERY
      INTEGER               EQRYI  ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      DOUBLE PRECISION      EQRYD  ( * )
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG
      INTEGER               ERRPTR
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     EQRYI     I-O  Integer component of query.
C     EQRYC     I-O  Character component of query.
C     EQRYD     I-O  Numeric component of query.
C     ERROR      O   Error flag.
C     ERRMSG     O   Error message.
C     ERRPTR     O   Position in query where error was detected.
C
C$ Detailed_Input
C
C     QUERY          is the original query from which EQRYI and EQRYC
C                    were obtained.  QUERY is used only for
C                    construction of error messages.
C
C     EQRYI          is the integer portion of an encoded EK query.
C                    The query must have been parsed.
C
C     EQRYC          is the character portion of an encoded EK query.
C
C     EQRYD          is the numeric portion of an encoded EK query.
C
C$ Detailed_Output
C
C     EQRYI          is the integer portion of an encoded EK query.
C                    On output, all valid time values will have been
C                    converted from strings to equivalent numeric
C                    values which represent times as ephemeris
C                    seconds past J2000 (TDB).
C
C     EQRYC          is the character portion of an encoded EK query.
C
C     ERROR          is a logical flag indicating whether an error was
C                    detected.  Note that a time string might be
C                    syntactically valid, but incapable of being
C                    converted to ET if the appropriate time kernels
C                    (Leapseconds or SCLK) are not loaded.
C
C     ERRMSG         is an error message describing an error in the
C                    input query, if one was detected.  If ERROR is
C                    returned .FALSE., then ERRPTR is undefined.
C
C     ERRPTR         is the character position in the original query
C                    at which an error was detected, if an error was
C                    found.  This index refers to the offending lexeme's
C                    position in the original query represented by the
C                    input encoded query.  If ERROR is returned .FALSE.,
C                    ERRPTR is undefined.
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input query is not initialized, the error will be
C         diagnosed by routines called by this routine.  The outputs
C         will not be modified.
C
C     2)  If names have not been resolved in the input query, the error
C         SPICE(NAMESNOTRESOLVED) will be signalled.  The outputs
C         will not be modified.
C
C     3)  If any sort of time conversion error occurs, the output flag
C         ERROR is set, and an error message is returned.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Strings representing time values are interpreted as follows:
C
C        1)  The string is first examined to see whether it's an
C            SCLK string for a recognized clock; if it is, the
C            string is converted to the equivalent ET.
C
C        2)  If the string is not a SCLK string, it is expected
C            to be some sort of UTC representation.  The string is
C            checked to see whether it's an ISO format UTC time that
C            ISO2UTC can handle.
C
C        3)  If the string does not conform to an ISO format, the
C            last chance is to try to get the string through
C            TPARSE.  If TPARSE can't deal with it, it's considered
C            to be invalid.
C
C
C     This routine assumes that encoded EK query architecture version
C     1 is to be used with the query to be initialized; this routine
C     will not work with any other architecture version.
C
C$ Examples
C
C     See EKFIND.
C
C$ Restrictions
C
C     1) A leapseconds kernel must be loaded at the time this routine
C        is called.
C
C     2) In order to convert SCLK strings, an appropriate SCLK kernel
C        must be loaded at the time this routine is called.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Local parameters
C
      INTEGER               SHORT
      PARAMETER           ( SHORT  = 32 )
 
C
C     Local variables
C
      CHARACTER*(TNAMSZ)    ALIAS  ( MAXTAB )
      CHARACTER*(CNAMSZ)    COLNAM
      CHARACTER*(TNAMSZ)    TABLE  ( MAXTAB )
      CHARACTER*(SHORT)     TIMSTR
      CHARACTER*(1)         TOUCHC
 
      DOUBLE PRECISION      ET
 
      INTEGER               ATTDSC ( ADSCSZ )
      INTEGER               BASE
      INTEGER               CNSTYP
      INTEGER               COLIDX
      INTEGER               DESCR  ( EQVDSZ )
      INTEGER               DTYPE
      INTEGER               I
      INTEGER               IRSOLV
      INTEGER               LXB
      INTEGER               LXE
      INTEGER               NCNS
      INTEGER               NTAB
      INTEGER               OPCODE
      INTEGER               SB
      INTEGER               SE
      INTEGER               TABIDX
 
C
C     No error to start with.
C
      ERROR  =  .FALSE.
      ERRMSG =  ' '
      ERRPTR =  0
 
      TOUCHC =  QUERY(1:1)
 
C
C     The query must have had names resolved at this point, or it's no
C     go.
C
      CALL ZZEKREQI ( EQRYI, 'NAMES_RESOLVED', IRSOLV )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
      IF ( IRSOLV .EQ. IFALSE ) THEN
 
         CALL CHKIN  ( 'ZZEKTRES'                                  )
         CALL SETMSG ( 'Names are not resolved in encoded query.'  )
         CALL SIGERR ( 'SPICE(NAMESNOTRESOLVED)'                   )
         CALL CHKOUT ( 'ZZEKTRES'                                  )
         RETURN
 
      END IF
 
C
C     Time strings occur only on the right sides of constraints.
C     Examine each constraint that compares a column and a value.
C
      CALL ZZEKREQI ( EQRYI, 'NUM_TABLES',      NTAB )
      CALL ZZEKREQI ( EQRYI, 'NUM_CONSTRAINTS', NCNS )
 
      DO I = 1, NCNS
C
C        Calculate the base address of the constraint.
C
         BASE    =  EQVBAS + (NTAB * 2 * EQVDSZ) + (I-1)*EQCDSZ
 
C
C        Obtain the constraint type.  If the RHS is not a value or if
C        the RHS is null (as indicated by the opcode), we can skip it.
C
         CNSTYP  =  EQRYI ( BASE + EQCTYP )
         OPCODE  =  EQRYI ( BASE + EQOPCD )
 
         IF (        ( CNSTYP .EQ. EQVAL  )
     .        .AND.  ( OPCODE .NE. ISNULL )
     .        .AND.  ( OPCODE .NE. NOTNUL )  ) THEN
C
C           Get the index of the table containing the LHS column, and
C           get the index of this column within that table.  Get the
C           table name, then get the column's attributes.
C
            TABIDX  =  EQRYI ( BASE + EQLTAB - 1 + EQTORD )
            COLIDX  =  EQRYI ( BASE + EQLCOL - 1 + EQCIDX )
 
            CALL ZZEKQTAB ( EQRYI, EQRYC, TABIDX, TABLE, ALIAS )
 
            CALL EKCII ( TABLE, COLIDX, COLNAM, ATTDSC )
 
            DTYPE  =  ATTDSC(2)
 
 
            IF ( DTYPE .EQ. TIME ) THEN
C
C              The RHS points to a string representing a time
C              value.
C
               LXB    =  EQRYI ( BASE + EQBVAL - 1 + EQBLEX )
               LXE    =  EQRYI ( BASE + EQBVAL - 1 + EQBLEX )
               SB     =  EQRYI ( BASE + EQBVAL - 1 + EQBSTR )
               SE     =  EQRYI ( BASE + EQBVAL - 1 + EQESTR )
               TIMSTR =  EQRYC ( SB:SE )
 
C
C              Convert the time to ET, if possible.
C
               CALL ZZEKTCNV ( TIMSTR, ET, ERROR, ERRMSG )
 
               IF ( ERROR ) THEN
 
                  ERRPTR = SB
                  RETURN
 
               END IF
C
C              Insert the ET value into the query, and replace the
C              value descriptor for the time string.
C
               CALL ZZEKINQN ( ET, TIME, LXB, LXE, EQRYI, EQRYD, DESCR )
 
               CALL MOVEI ( DESCR, EQVDSZ, EQRYI(BASE+EQBVAL) )
 
            END IF
C
C           We've parsed a time string, if the current column's type
C           was TIME.
C
 
         END IF
C
C        We've examined the current constraint, if it compares a
C        column with a value.
C
 
      END DO
 
C
C     Indicate completion of time resolution.
C
      CALL ZZEKWEQI (  'TIMES_RESOLVED',  ITRUE,  EQRYI  )
 
 
      RETURN
      END
