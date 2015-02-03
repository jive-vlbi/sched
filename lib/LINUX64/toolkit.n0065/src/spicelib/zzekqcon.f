C$Procedure  ZZEKQCON ( Private: EK, read constraints from query )
 
      SUBROUTINE ZZEKQCON ( EQRYI,   EQRYC,   EQRYD,   N,
     .                      CNSTYP,
     .                      LTNAME,  LTIDX,   LCNAME,  LCIDX,
     .                      OPCODE,
     .                      RTNAME,  RTIDX,   RCNAME,  RCIDX,
     .                      DTYPE,
     .                      CBEG,    CEND,    DVAL,    IVAL   )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Return elements of a specified constraint from an encoded EK
C     query.
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
 
      IMPLICIT NONE

      INCLUDE 'ekquery.inc'
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               EQRYI ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      DOUBLE PRECISION      EQRYD ( * )
      INTEGER               N
      INTEGER               CNSTYP
      CHARACTER*(*)         LTNAME
      INTEGER               LTIDX
      CHARACTER*(*)         LCNAME
      INTEGER               LCIDX
      INTEGER               OPCODE
      CHARACTER*(*)         RTNAME
      INTEGER               RTIDX
      CHARACTER*(*)         RCNAME
      INTEGER               RCIDX
      INTEGER               DTYPE
      INTEGER               CBEG
      INTEGER               CEND
      DOUBLE PRECISION      DVAL
      INTEGER               IVAL
 
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     EQRYI      I   Integer component of query.
C     EQRYC      I   Character component of query.
C     EQRYD      I   Numeric component of query.
C     N          I   Index of constraint to read.
C     CNSTYP     O   Type of constraint (column or value comparison).
C     LTNAME     O   LHS table name.
C     LTIDX      O   LHS table index in FROM clause.
C     LCNAME     O   LHS column name.
C     LCIDX      O   LHS column index in virtual parent table.
C     OPCODE     O   Operation code.
C     RTNAME     O   RHS table name.
C     RTIDX      O   RHS table index in FROM clause.
C     RCNAME     O   RHS column name.
C     RCIDX      O   RHS column index in virtual parent table.
C     DTYPE      O   Data type of RHS value.
C     CBEG       O   Character begin pointer for RHS value.
C     CEND       O   Character end pointer for RHS value.
C     DVAL       O   RHS double precision value.
C     IVAL       O   RHS integer value.
C
C$ Detailed_Input
C
C     EQRYI,
C     EQRYC,
C     EQRYD          are, respectively, the integer, character, and
C                    numeric components of an encoded EK query.
C                    The query must have names and values resolved and
C                    must have been semantically checked.
C
C     N              is the index, within the WHERE clause of the query,
C                    of the constraint to be fetched.
C
C$ Detailed_Output
C
C     CNSTYP         is the constraint type.  Possible values are
C
C                       EQCOL  ...  constraint compares two columns
C                       EQVAL  ...  constraint compares column and value
C
C     LTNAME         is the table name for the LHS of the constraint.
C                    If an alias was supplied in the query, that
C                    alias is returned.  If the column was unqualified,
C                    LTNAME is returned blank.
C
C     LTIDX          is the index of the LHS table in the FROM clause.
C
C     LCNAME         is the name of the LHS column.
C
C     LCIDX          is the index of the LHS column in the virtual
C                    table containing the column.
C
C     OPCODE         is the operator code used in the constraint.
C
C     RTNAME         is the table name for the RHS of the constraint.
C                    RTNAME is meaningful only if the constraint
C                    compares two columns, as indicated by CNSTYP.
C                    If an alias was supplied in the query, that
C                    alias is returned.  If the column was unqualified,
C                    RTNAME is returned blank.
C
C     RTIDX          is the index of the RHS table in the FROM clause.
C                    RTIDX is meaningful only if the constraint
C                    compares two columns, as indicated by CNSTYP.
C
C     RCNAME         is the name of the RHS column.  RCNAME is
C                    meaningful only if the constraint compares two
C                    columns, as indicated by CNSTYP.
C
C     RCIDX          is the index of the RHS column in the virtual
C                    table containing the column.  RCIDX is
C                    meaningful only if the constraint compares two
C                    columns, as indicated by CNSTYP.
C
C     DTYPE          is the data type of the value on the RHS of the
C                    constraint.  DTYPE is meaningful only if the
C                    constraint compares a column against a value,
C                    as indicated by CNSTYP.
C
C     CBEG,
C     CEND           are, respectively, begin and end character pointers
C                    into the EQRYC array; these pointers give the
C                    location of a character value on the RHS of a
C                    query constraint.  CBEG and CEND are meaningful
C                    only if the constraint compares a column against a
C                    value, as indicated by CNSTYP, and if the value's
C                    data type is CHR, as indicated by DTYPE.
C
C     IVAL           is an integer value on the RHS of the constraint.
C                    IVAL is meaningful only if the constraint compares
C                    a column against a value, as indicated by CNSTYP,
C                    and if the value's data type is INT, as indicated
C                    by DTYPE.
C
C     DVAL           is a double precision value on the RHS of the
C                    constraint.  DVAL is meaningful only if the
C                    constraint compares a column against a value, as
C                    indicated by CNSTYP, and if the value's data type
C                    is DP or TIME, as indicated by DTYPE.
C
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
C     2)  If the input query has not been semantically checked, the
C         error SPICE(NOTSEMCHECKED) will be signaled.  The outputs
C         will not be modified.
C
C     3)  If the index N is less than 1 or greater than the number of
C         constraints in the query, the error SPICE(INVALIDINDEX)
C         will be signaled.  The outputs
C         will not be modified.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The call
C
C        CALL ZZEKREQI (  EQRYI,  'NUM_CONSTRAINTS',  N  )
C
C     may be used to get the constraint count from an encoded query.
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
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 29-APR-2009 (NJB)
C
C        Bug fix: this routine now does not attempt to
C        read constraint RHS value parameters from the
C        encoded query when the RHS value is NULL, as
C        indicated by the opcode.
C
C-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
 
C
C     Local variables
C
      INTEGER               BASE
      INTEGER               CB
      INTEGER               CE
      INTEGER               ICHECK
      INTEGER               NCNS
      INTEGER               NTAB
      INTEGER               PTR
      INTEGER               TB
      INTEGER               TE
 
C
C     Use discovery check-in.
C
 
      CALL ZZEKREQI ( EQRYI, 'SEM_CHECKED', ICHECK )
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
 
      IF ( ICHECK .EQ. IFALSE ) THEN
 
         CALL CHKIN  ( 'ZZEKQCON'                                )
         CALL SETMSG ( 'Encoded query has not been semantically '//
     .                 'checked.'                                )
         CALL SIGERR ( 'SPICE(NOTSEMCHECKED)'                    )
         CALL CHKOUT ( 'ZZEKQCON'                                )
         RETURN
 
      END IF
 
      CALL ZZEKREQI ( EQRYI, 'NUM_CONSTRAINTS',   NCNS   )
      CALL ZZEKREQI ( EQRYI, 'NUM_TABLES',        NTAB   )
 
 
      IF (  ( N .LT. 1 ) .OR. ( N .GT. NCNS )  ) THEN
 
         CALL CHKIN  ( 'ZZEKQCON'                                      )
         CALL SETMSG ( 'Constraint index # is out of valid range 1:#.' )
         CALL ERRINT ( '#',  N                                         )
         CALL ERRINT ( '#',  NCNS                                      )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                           )
         CALL CHKOUT ( 'ZZEKQCON'                                      )
         RETURN
 
      END IF
 
C
C     Compute the base address of the Nth constraint.
C
      BASE   =      EQVBAS
     .            + NTAB  * 2 * EQVDSZ
     .            + (N-1)     * EQCDSZ
 
C
C     Get the constraint type.
C
      CNSTYP  =  EQRYI ( BASE + EQCTYP )
 
C
C     Get the LHS items.
C
      LTIDX  =  EQRYI ( BASE + EQLTAB - 1 + EQTORD )
      TB     =  EQRYI ( BASE + EQLTAB - 1 + EQBSTR )
      TE     =  EQRYI ( BASE + EQLTAB - 1 + EQESTR )
 
      IF ( TB .NE. 0 ) THEN
         LTNAME =  EQRYC ( TB:TE )
      ELSE
         LTNAME = ' '
      END IF
 
      LCIDX  =  EQRYI ( BASE + EQLCOL - 1 + EQCIDX )
      CB     =  EQRYI ( BASE + EQLCOL - 1 + EQBSTR )
      CE     =  EQRYI ( BASE + EQLCOL - 1 + EQESTR )
      LCNAME =  EQRYC ( CB:CE )
 
C
C     Next, the opcode.
C
      OPCODE =  EQRYI ( BASE + EQOPCD )
 
C
C     If the constraint compares two columns, get the RHS table and
C     column info.
C
      IF ( CNSTYP .EQ. EQCOL ) THEN
 
         RTIDX  =  EQRYI ( BASE + EQRTAB - 1 + EQTORD )
         TB     =  EQRYI ( BASE + EQRTAB - 1 + EQBSTR )
         TE     =  EQRYI ( BASE + EQRTAB - 1 + EQESTR )
 
         IF ( TB .NE. 0 ) THEN
            RTNAME =  EQRYC ( TB:TE )
         ELSE
            RTNAME = ' '
         END IF
 
         RCIDX  =  EQRYI ( BASE + EQRCOL - 1 + EQCIDX )
         CB     =  EQRYI ( BASE + EQRCOL - 1 + EQBSTR )
         CE     =  EQRYI ( BASE + EQRCOL - 1 + EQESTR )
         RCNAME =  EQRYC ( CB:CE )
 
C
C        ...and clear out the scalar outputs.
C
         CBEG   =  1
         CEND   =  1
         DVAL   =  0.D0
         IVAL   =  0
 
 
      ELSE
C
C        The constraint compares a column and a value.  Set the
C        appropriate scalar output, and clear out the other outputs.
C
         IF ( ( OPCODE .EQ. ISNULL ) .OR. ( OPCODE .EQ. NOTNUL ) ) THEN
C
C           There's no output value; the opcode implies the value NULL.
C           Set the outputs to innocuous defaults.
C
            CBEG = 1
            CEND = 1
            DVAL = 0.D0
            IVAL = 0
            
         ELSE
C
C           This is the normal case; set the scalar output values
C           according to the RHS data type.
C
            DTYPE  =  EQRYI ( BASE + EQBVAL - 1 + EQDTYP )
 
            IF ( DTYPE .EQ. CHR ) THEN

               CBEG   =  EQRYI ( BASE + EQBVAL - 1 + EQBSTR )
               CEND   =  EQRYI ( BASE + EQBVAL - 1 + EQESTR )
               DVAL   =  0.D0
               IVAL   =  0

            ELSE IF ( DTYPE .EQ. INT ) THEN

               PTR    =  EQRYI ( BASE + EQBVAL - 1 + EQVPTR )
               IVAL   =  NINT  ( EQRYD(PTR) )
               DVAL   =  0.D0
               CBEG   =  1
               CEND   =  1

            ELSE
C
C              The data type is DP or TIME.
C
               PTR    =  EQRYI ( BASE + EQBVAL - 1 + EQVPTR )
               DVAL   =  EQRYD ( PTR )
               IVAL   =  0
               CBEG   =  1
               CEND   =  1

            END IF
 
         END IF

C
C        Set the RHS table and column outputs.
C
         RTIDX  =  0
         RTNAME =  ' '
         RCIDX  =  0
         RTNAME =  ' '
 
      END IF
 
 
      RETURN
      END
