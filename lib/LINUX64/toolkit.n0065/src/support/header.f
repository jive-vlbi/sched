C$Procedure      HEADER (HEADER for a report)
 
      SUBROUTINE HEADER ( N, COMP, VALUE, WDTH )
 
C$ Abstract
C
C    This is an umbrella routine for setting up headers
C    on tabular reports.
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
C     REPORTS
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               N
      INTEGER               COMP
      CHARACTER*(*)         VALUE
      INTEGER               WDTH
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     N         I/O  Column number
C     COMP      I/O  Component number
C     VALUE     I/O  String Value.
C     WDTH       O   Non-blank width of VALUE
C
C$ Detailed_Input
C
C     N         specifies which column is being defined.
C               Legitimate values are 1 to 40.
C
C     COMP      specifies which column component is being
C               specified.  Legitimate values are 1 to 10.
C
C     VALUE     specifies the column component value. It should
C               be 800 or fewer characters in length.
C
C$ Detailed_Output
C
C     N         specifies which column to fetch information from.
C               Legitimate values are 1 to 40.
C
C     COMP      specifies which column component is to obtain
C               information for. Legitimate values are 1 to 10.
C
C     VALUE     Value of requested column component.
C
C     WDTH      is the non-blank width of VALUE.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     If N or NCOMP is out of range:
C
C        SCOLMN simply returns.  No data is buffered. No warning
C        or error is issued.
C
C        GCOLMN returns a blank.  WDTH will be set to 1.
C
C
C$ Particulars
C
C     This is a routine designed to work with the routine
C     TABRPT when creating tabular outputs.  It is primarily
C     intended for creating the header portion of a report.
C
C     For example, the area marked with the arrow below.
C
C        Name       Phone       Address    <<=============
C        =================================
C        Bill       555-1212    Pasadena, CA
C        Bob        555-2121    Flint, Michigan
C        Ian        555-1234    San Jose, CA
C
C     You could use this to fill out the contents of the report
C     if you don't have something already  that fetches
C     string values.
C
C$ Examples
C
C     Suppose you wanted to create the header above and have
C     it appear on your reports.  Here's all you need to do.
C
C        CALL SCOLMN ( 1, 1, 'Name'    )
C        CALL SCOLMN ( 2, 1, 'Phone'   )
C        CALL SCOLMN ( 3, 1, 'Address' )
C
C     Then simply pass the entry point GCOLMN to TABRPT to construct
C     the header portion of the report.
C
C        CALL TABRPT ( 3, item,   size,
C    .                    width,  justr,
C    .                    presrv, spcial,
C    .                    lmarge, space,
C    .                    GCOLMN   )
C
C     filling out the various items as is appropriate for the
C     table you plan to create.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C       W.L. Taber      (JPL)
C
C$ Literature_References
C
C       None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 3-AUG-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Setting and getting values for reports
C
C-&
C
C     SPICELIB Functions
C
      INTEGER               RTRIM
 
C
C     Buffer declarations
C
 
      INTEGER               NCOL
      PARAMETER           ( NCOL  = 40 )
 
      INTEGER               NCOMP
      PARAMETER           ( NCOMP = 10 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 1600 )
 
      CHARACTER*(LNSIZE)    BUFFER ( NCOL, NCOMP )
 
      INTEGER               I
      INTEGER               J
 
      LOGICAL               FIRST
 
      SAVE
 
      DATA                  FIRST / .TRUE. /
 
 
 
      RETURN
 
C
C     Set a column component value.
C
      ENTRY SCOLMN ( N, COMP, VALUE )
 
         IF ( FIRST ) THEN
 
            DO I = 1, NCOL
               DO J = 1, NCOMP
                  BUFFER(I,J) = ' '
               END DO
            END DO
 
            FIRST = .FALSE.
 
         END IF
 
         IF (      N    .GE. 1
     .       .AND. N    .LE. NCOL
     .       .AND. COMP .GE. 1
     .       .AND. COMP .LE. NCOMP) THEN
 
            BUFFER( N, COMP ) = VALUE
 
         END IF
 
         RETURN
 
C
C     Get a column component value.
C
      ENTRY GCOLMN ( N, COMP, VALUE, WDTH )
 
         IF ( FIRST ) THEN
 
            DO I = 1, NCOL
               DO J = 1, NCOMP
                  BUFFER(I,J) = ' '
               END DO
            END DO
 
            FIRST = .FALSE.
 
         END IF
 
         IF (      N    .GE. 1
     .       .AND. N    .LE. NCOL
     .       .AND. COMP .GE. 1
     .       .AND. COMP .LE. NCOMP) THEN
 
            VALUE = BUFFER( N, COMP )
            WDTH  = RTRIM ( VALUE   )
 
         ELSE
 
            VALUE = ' '
            WDTH  = 1
 
         END IF
 
         RETURN
 
      ENTRY CCOLMN
 
         DO I = 1, NCOL
            DO J = 1, NCOMP
               BUFFER(I,J) = ' '
            END DO
         END DO
         RETURN
 
 
      END
