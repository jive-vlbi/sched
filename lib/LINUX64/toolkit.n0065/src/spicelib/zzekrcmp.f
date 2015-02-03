C$Procedure ZZEKRCMP ( EK, row comparison )
 
      LOGICAL FUNCTION ZZEKRCMP ( OP,
     .                            NCOLS,
     .                            HAN1, SGDSC1, CDLST1, ROW1, ELTS1,
     .                            HAN2, SGDSC2, CDLST2, ROW2, ELTS2  )
 
C$ Abstract
C
C     Compare two EK rows, using as the order relation dictionary
C     ordering on a specified list of columns.
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
C     UTILITY
C
C$ Declarations
 
 
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      INTEGER               OP
      INTEGER               NCOLS
      INTEGER               HAN1
      INTEGER               SGDSC1 ( SDSCSZ )
      INTEGER               CDLST1 ( CDSCSZ, * )
      INTEGER               ROW1
      INTEGER               ELTS1  ( * )
      INTEGER               HAN2
      INTEGER               SGDSC2 ( SDSCSZ )
      INTEGER               CDLST2 ( CDSCSZ, * )
      INTEGER               ROW2
      INTEGER               ELTS2  ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     OP         I   Code for relational operator.
C     NCOLS      I   Number of columns used to define order relation.
C     HAN1       I   Handle of EK containing first row to compare.
C     SGDSC1     I   Descriptor of segment containing first row.
C     ROW1       I   Number of first row (relative to segment).
C     ELTS1      I   List of element indices for first row.
C     CDLST1     I   List of column descriptors for first row.
C     HAN2       I   Handle of EK containing second row to compare.
C     SGDSC2     I   Descriptor of segment containing second row.
C     ROW2       I   Number of second row (relative to segment).
C     CDLST2     I   List of column descriptors for second row.
C     ELTS2      I   List of element indices for second row.
C
C     The function returns .TRUE. if and only if the two rows
C     satisfy the order relation specified by the input arguments.
C
C$ Detailed_Input
C
C     OP             is an integer code representing a binary operator
C                    that expresses an order relation.  The allowed
C                    values of OP are the parameters
C
C                       EQ
C                       GE
C                       GT
C                       LE
C                       LT
C                       NE
C
C                    This routine test whether the input rows satisfy
C                    the order relation
C
C                       <row1> OP <row2>
C
C
C     NCOLS          is the number of columns used to define a
C                    dictionary ordering.
C
C     HAN1           is the file handle of the EK containing the first
C                    row.
C
C     SGDSC1         is the segment descriptor of the EK segment
C                    containing the first of the two rows to be
C                    compared.
C
C     CDLST1         is a list of column descriptors.  These descriptors
C                    identify the columns that define the dictionary
C                    ordering used to compare the input rows.
C
C     ROW1           is the row number of the first row to be compared.
C
C     ELTS1          is a list of column entry element indices for the
C                    first row.  These indices identify the elements
C                    to be used in the row comparison.  The value of
C                    ELTS1(I) is used only if the column specified by
C                    the Ith column descriptor of CDLST1 is
C                    array-valued.
C
C     HAN2           is the file handle of the EK containing the second
C                    row to be compared.
C
C     SGDSC2         is the segment descriptor of the EK segment
C                    containing the second row.
C
C     CDLST2         is a list of column descriptors for the second row.
C                    This list parallels CDLST1:  the Nth descriptor
C                    in CDLST2 is for a column having the same name and
C                    attributes as that designated by the Nth descriptor
C                    in CDLST1.
C
C     ROW2           is the row number of the second row.
C
C     ELTS2          is a list of column entry element indices for the
C                    second row.  These indices identify the elements
C                    to be used in the row comparison.  The value of
C                    ELTS2(I) is used only if the column specified by
C                    the Ith column descriptor of CDLST2 is
C                    array-valued.
C
C$ Detailed_Output
C
C     The function returns .TRUE. if and only if the two rows satisfy
C     the order relation specified by the input arguments:
C
C        <row 1> OP <row 2>
C
C     The ordering used to compare the rows is a dictionary ordering
C     defined by the column descriptor lists CDLST1 and CDLST2.  The
C     order relationship between the columns is determined by comparing
C     the entries in both rows in the column identified by CDLST1(*,1)
C     and CDLST2(*,1); if these column entries are equal, the entries
C     identified by CDLST1(*,2) and CDLST2(*,2) are compared, and so
C     on, until the tie is broken or all of the specified column entries
C     have been compared.
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
C     Fortran counterparts.  In the character case, the same operators
C     may be used; the meanings of the parameters
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
C     1)  If the either of input file handles is invalid, the error
C         will be diagnosed by routines called by this routine.
C         The function value is .FALSE. in this case.
C
C     2)  If an I/O error occurs while attempting to find the address
C         range of the specified column entry element, the error will
C         be diagnosed by routines called by this routine.  The
C         function value is .FALSE. in this case.
C
C     3)  If any of the input segment descriptors, column descriptors,
C         or row numbers are invalid, this routine may fail in
C         unpredictable, but possibly spectacular, ways.  Except
C         as described in this header section, no attempt is made to
C         handle these errors.
C
C     4)  If the data type code in the input column descriptor is not
C         recognized, the error SPICE(INVALIDDATATYPE) is signalled.
C         The function value is .FALSE. in this case.
C
C     5)  If the relational operator code OP is not recognized, the
C         error SPICE(UNNATURALRELATION) is signalled.
C         The function value is .FALSE. in this case.
C
C$ Files
C
C     See the descriptions of the arguments HAN1 and HAN2 in
C     $Detailed_Input.
C
C$ Particulars
C
C     This routine is an EK utility intended to centralize a frequently
C     performed comparison operation.
C
C$ Examples
C
C     See EKSRCH.
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
C-    SPICELIB Version 1.0.1, 02-JAN-2007 (EDW)
C
C        Edited to remove typo in function declaration.
C        Declaration included an extraneous continutation
C        marker.
C
C-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
C
C-&
 
 
C
C     Non-SPICELIB functions
C
      INTEGER               ZZEKECMP
 
C
C     Local variables
C
      INTEGER               CLDSCS ( CDSCSZ, 2 )
      INTEGER               COL
      INTEGER               ELTS   ( 2 )
      INTEGER               HANS   ( 2 )
      INTEGER               REL
      INTEGER               ROWS   ( 2 )
      INTEGER               SGDSCS ( SDSCSZ, 2 )
 
C
C     Use discovery check-in for speed.
C
C     The function value defaults to .FALSE.
C
      ZZEKRCMP = .FALSE.
 
C
C     The input column descriptors identify the columns to be used
C     to define an order relation on the input rows.  The order
C     relation is `dictionary' ordering:  if the elements of the
C     first n columns of both rows are equal, the corresponding
C     elements in the (n+1)st columns are compared to attempt to
C     break the tie.
C
C     The first step is to determine the relation that holds between
C     the rows.  We start out assuming we have equality.
C
      HANS(1)  =  HAN1
      HANS(2)  =  HAN2
 
      CALL MOVEI ( SGDSC1, SDSCSZ, SGDSCS(1,1) )
      CALL MOVEI ( SGDSC2, SDSCSZ, SGDSCS(1,2) )
 
      ROWS(1)  =  ROW1
      ROWS(2)  =  ROW2
 
      REL      =  EQ
      COL      =  1
 
      DO WHILE (  ( COL .LE. NCOLS ) .AND. ( REL .EQ. EQ )  )
C
C        Compare the entries in the two rows in the columns indicated
C        by the Nth column descriptor pair.
C
         CALL MOVEI ( CDLST1(1,COL), CDSCSZ, CLDSCS(1,1) )
         CALL MOVEI ( CDLST2(1,COL), CDSCSZ, CLDSCS(1,2) )
 
         ELTS(1)  =  ELTS1(COL)
         ELTS(2)  =  ELTS2(COL)
 
         REL      =  ZZEKECMP ( HANS,  SGDSCS,  CLDSCS, ROWS,  ELTS  )
 
C
C        We've completed the comparison for the column numbered COL.
C
         COL      =  COL + 1
 
      END DO
 
 
C
C     Determine the truth of the input relational expression.
C
      IF ( OP .EQ. EQ )   THEN
 
         ZZEKRCMP  =  REL .EQ. EQ
 
 
      ELSE IF ( OP .EQ. LT )   THEN
 
         ZZEKRCMP  =  REL .EQ. LT
 
 
      ELSE IF ( OP .EQ. LE )   THEN
 
         ZZEKRCMP  =  REL .NE. GT
 
 
      ELSE IF ( OP .EQ. GT )   THEN
 
         ZZEKRCMP  =  REL .EQ. GT
 
 
      ELSE IF ( OP .EQ. GE )   THEN
 
         ZZEKRCMP  =  REL .NE. LT
 
 
      ELSE IF ( OP .EQ. NE )   THEN
 
         ZZEKRCMP  =  REL .NE. EQ
 
 
      ELSE
C
C        Sorry, we couldn't resist.
C
         ZZEKRCMP  =  .FALSE.
 
         CALL CHKIN  ( 'ZZEKRCMP'                                      )
         CALL SETMSG ( 'The relational operator # was not recognized.' )
         CALL ERRINT ( '#',  OP                                        )
         CALL SIGERR ( 'SPICE(UNNATURALRELATION)'                      )
         CALL CHKOUT ( 'ZZEKRCMP'                                      )
         RETURN
 
      END IF
 
 
      RETURN
      END
