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
C
C     Include Section:  EK Operator Codes
C
C        ekopcd.inc  Version 1  30-DEC-1994 (NJB)
C
C
C     Within the EK system, operators used in EK queries are
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
C     and in addition includes the operators
C
C        LIKE,  UNLIKE
C
C     which are used to compare strings to a template.  In the character
C     case, the meanings of the parameters
C
C        GE,  GT,  LE,  LT
C
C     match those of the Fortran lexical functions
C
C        LGE, LGT, LLE, LLT
C
C
C     The additional unary operators
C
C        ISNULL, NOTNUL
C
C     are used to test whether a value of any type is null.
C
C
      INTEGER               EQ
      PARAMETER           ( EQ     =         1 )
 
      INTEGER               GE
      PARAMETER           ( GE     =  EQ  +  1 )
 
      INTEGER               GT
      PARAMETER           ( GT     =  GE  +  1 )
 
      INTEGER               LE
      PARAMETER           ( LE     =  GT  +  1 )
 
      INTEGER               LT
      PARAMETER           ( LT     =  LE  +  1 )
 
      INTEGER               NE
      PARAMETER           ( NE     =  LT  +  1 )
 
      INTEGER               LIKE
      PARAMETER           ( LIKE   =  NE     +  1 )
 
      INTEGER               UNLIKE
      PARAMETER           ( UNLIKE =  LIKE   +  1 )
 
      INTEGER               ISNULL
      PARAMETER           ( ISNULL =  UNLIKE +  1 )
 
      INTEGER               NOTNUL
      PARAMETER           ( NOTNUL =  ISNULL +  1 )
 
C
C     End Include Section:  EK Operator Codes
C
