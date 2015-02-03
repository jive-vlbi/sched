 
C$Procedure SBINIT ( String buffer, initialize )
 
      SUBROUTINE SBINIT_1 ( NSIZE, PSIZE, VDIM, NAMES,
     .                                        PTRS,
     .                                        BUFFER )
      IMPLICIT NONE
 
 
C$ Abstract
C
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
C     CB, LB, SB
C
C$ Keywords
C
C     ASCII
C     CHARACTER
C     STRING
C     TEXT
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               LBCBUF
      PARAMETER           ( LBCBUF =  0 )
 
      INTEGER               NSIZE
      INTEGER               PSIZE
      INTEGER               VDIM
      CHARACTER*(*)         NAMES       ( LBCELL:NSIZE )
      INTEGER               PTRS        ( LBCELL:PSIZE )
      CHARACTER*(*)         BUFFER      ( LBCBUF:VDIM  )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NSIZE      I   Name size.
C     PSIZE      I   Pointer size.
C     VDIM       I   Value dimension
C     NAMES,
C     PTRS,
C     BUFFER    I,O  String buffer.
C
C$ Detailed_Input
C
C     NAMES       is a character cell array to be used as the name
C                 component of a string buffer.
C
C     NSIZE       is the declared dimension of NAMES.
C
C     PTRS        is an integer cell array to be used as the pointer
C                 component of a string buffer.
C
C     PSIZE       is the declared dimension of PTRS.
C
C     BUFFER      is a character buffer array to be used as the
C                 character component of a string buffer.
C
C     VDIM        is the declared dimension of BUFFER.
C
C$ Detailed_Output
C
C     NAMES,
C     PTRS,
C     BUFFER      together are an initialized string buffer.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If the size of the pointer array is not sufficient to
C        hold pointers for the maximum number of strings, the
C        error 'SPICE(SBINSUFPTRSIZE)' is signalled.
C
C$ Particulars
C
C     A string buffer must be initialized to allow subsequent
C     operations on the buffer to detect possible overflows.
C     All three components of the buffer are initialized by a
C     single call to SBINIT.
C
C     In order to make full use of the name cell of the string buffer,
C     the arrays and name buffers should be declared as shown below.
C
C       INTEGER              LBCELL
C       PARAMETER          ( LBCELL = -5 )
C
C       INTEGER              MAXNL
C       PARAMETER          ( MAXNL  = maximum name length )
C
C       INTEGER              MAXN
C       PARAMETER          ( MAXN   = maximum number of names )
C
C       CHARACTER*(MAXNL)    NAMES ( MAXN         )
C       INTEGER              PTRS  ( MAXN * 4 + 4 )
C
C     The character buffer portion of the string buffer should be
C     declared as shown below.
C
C       INTEGER              MAXL
C       PARAMETER          ( MAXL = maximum expected string length )
C
C       INTEGER              AVGL
C       PARAMETER          ( AVGL = average expected string length )
C
C       INTEGER              LBCBUF
C       PARAMETER          ( LBCBUF = 0  )
C
C       CHARACTER*(MAXL)     BUFFER ( LBCBUF:(MAXN * AVGL) / MAXL + 1 )
C
C$ Examples
C
C     The following code fragment illustrates the initialization
C     of a typical string buffer.
C
C        INTEGER               LBCELL
C        PARAMETER           ( LBCELL =    -5 )
C
C        INTEGER               LBCBUF
C        PARAMETER           ( LBCBUF =     0 )
C
C        CHARACTER*(32)        NAMES    ( LBCELL:1000    )
C        INTEGER               PTRS     ( LBCELL:4004    )
C        CHARACTER*(250)       BUFFER   ( LBCBUF:100     )
C         .
C         .
C
C        CALL SBINIT ( MAXN, PSIZE, BUFDIM, NAMES, PTRS, BUFFER )
C
C     In this example, the buffer may be used to store up to 1000
C     strings averaging 25 characters per string, or 25,000 total
C     characters. The length of any particular string may range from
C     a single character to the entire 25,000 characters. The names
C     used to identify the strings may contain up to 32 characters.
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
C     Dagny Taggart, (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 19-JAN-1989 (DT)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               MAXPTR
 
 
C
C     Standard error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SBINIT_1' )
      END IF
 
C
C     Make sure that the line buffer is large enough (but ONLY large
C     enough) to hold the maximum number of strings. The name list
C     should be empty. The LB should be initialized as a unit.
C
      MAXPTR = 4 * ( NSIZE + 1 )
 
      IF ( PSIZE .LT. MAXPTR ) THEN
         CALL SIGERR ( 'SPICE(SBINSUFPTRSIZE)' )
 
      ELSE
         CALL SSIZEC ( NSIZE,        NAMES         )
         CALL LBINIT_1 ( MAXPTR, VDIM, PTRS, BUFFER )
      END IF
 
      CALL CHKOUT ( 'SBINIT_1' )
      RETURN
      END
