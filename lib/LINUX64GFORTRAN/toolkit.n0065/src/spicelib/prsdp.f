C$Procedure   PRSDP   ( Parse d.p. number with error checking )
 
      SUBROUTINE PRSDP ( STRING, DPVAL )
      IMPLICIT NONE
 
C$ Abstract
C
C     Parse a string as a double precision number, encapsulating error
C     handling.
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
C     NUMBER
C     PARSING
C
C$ Declarations
 
      CHARACTER*(*)         STRING
      DOUBLE PRECISION      DPVAL
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     STRING     I   String representing a d.p. number.
C     DPVAL      O   D.p. value obtained by parsing STRING.
C
C$ Detailed_Input
C
C     STRING         is a string representing a double precision
C                    number.  Any string acceptable to the SPICELIB
C                    routine NPARSD is allowed.
C
C$ Detailed_Output
C
C     DPVAL          is the double precision number obtained by parsing
C                    STRING.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input string cannot be parsed, the error
C        SPICE(NOTADPNUMBER) is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The purpose of this routine is to enable safe parsing of double
C     precision numbers without the necessity of in-line error checking.
C     This routine is based on the SPICELIB routine NPARSD.
C
C$ Examples
C
C     See the routine NPARSD for an examples of allowed strings.
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
C-    SPICELIB Version 1.1.0, 15-SEP-1997 (NJB)
C
C        Bug fix:  output argument declaration changed from INTEGER
C        to DOUBLE PRECISION.
C
C-    SPICELIB Version 1.0.0, 22-JUL-1997 (NJB)
C
C-&
 
C$ Index_Entries
C
C     parse d.p. number with encapsulated error handling
C
C-&


C
C     Local parameters
C
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 320 )
 
C
C     Local variables
C
      CHARACTER*(MSGLEN)    ERRMSG
      INTEGER               PTR
 
 
C
C     Use discovery check-in.
C
      CALL NPARSD ( STRING, DPVAL, ERRMSG, PTR )
 
      IF ( ERRMSG .NE. ' ' ) THEN
 
         CALL CHKIN  ( 'PRSDP'               )
         CALL SETMSG ( ERRMSG                )
         CALL SIGERR ( 'SPICE(NOTADPNUMBER)' )
         CALL CHKOUT ( 'PRSDP'               )
         RETURN
 
      END IF
 
      RETURN
      END
