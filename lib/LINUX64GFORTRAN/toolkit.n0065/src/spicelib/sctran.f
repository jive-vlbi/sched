C$Procedure  SCTRAN  ( SCLK name/ID code translation )
 
      SUBROUTINE SCTRAN ( CLKNAM, CLKID, FOUND )
      IMPLICIT NONE

C$ Abstract
C
C     Convert between SCLK name strings and ID codes.
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
C     SCLK
C
C$ Keywords
C
C     CONVERSION
C     PARSING
C     SCLK
C     TIME
C     UTILITY
C
C$ Declarations
 
 
      CHARACTER*(*)         CLKNAM
      INTEGER               CLKID
      LOGICAL               FOUND
 
      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 32 )
 
C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     CLKNAM    I-O  SCID2N, SCN2ID
C     CLKID     I-O  SCID2N, SCN2ID
C     FOUND      O   SCID2N, SCN2ID
C     MAXLEN     P   All
C
C$ Detailed_Input
C
C     See the entry points for a discussion of their arguments.
C
C$ Detailed_Output
C
C     See the entry points for a discussion of their arguments.
C
C$ Parameters
C
C     MAXLEN         is the maximum allowed length, in characters, of a
C                    string containing the name of a spacecraft clock.
C
C$ Exceptions
C
C     1)  This is an umbrella subroutine that contains declarations
C         for its entry points.  This routine should never be called
C         directly.  If it is, the error SPICE(BOGUSENTRY) will be
C         signaled.
C
C
C     See the entry points for a discussion of exceptions specific to
C     those routines.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This set of subroutines centralizes the mapping between
C     spacecraft clock names and their corresponding NAIF integer
C     codes. Translation between these names and codes is frequently
C     required by user interface functions.
C
C     The set of supported clocks is identical to the set of spacecraft
C     supported by BODTRN.  The mapping may be extended by calling
C     BODDEF. 
C
C$ Examples
C
C     See the entry points for examples of their usage.
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
C-    SPICELIB Version 1.2.0, 29-OCT-2001 (NJB)
C
C        Bug fix:  modified algorithm to handle case where string
C        "SCLK" appears in SCLK name.
C
C-    SPICELIB Version 1.1.0, 25-FEB-2000 (NJB)
C
C        Updated to use BODTRN for SCLK name/code mapping.
C
C-    Beta Version 1.0.0, 17-NOV-1995 (NJB)
C
C-&
 
C$ Index_Entries
C
C     convert between SCLK ID codes and names
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0, 12-AUG-2001 (NJB)
C
C        Bug fix:  modified algorithm to handle case where string
C        "SCLK" appears in SCLK name.  SCN2ID now uses POSR to locate 
C        the substring "SCLK" in the input string.
C     
C-&
 
C
C     SPICELIB functions
C
      INTEGER               POSR
      INTEGER               RTRIM

      LOGICAL               RETURN
  
C
C     Local variables
C
      CHARACTER*(MAXLEN)    TMPNAM

      INTEGER               LOC

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCTRAN' )
      END IF
 
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
 
      CALL CHKOUT ( 'SCTRAN' )
      RETURN
 
 
 
 
 
C$Procedure  SCN2ID  ( SCLK name to ID code )
 
      ENTRY SCN2ID ( CLKNAM, CLKID, FOUND )
 
C$ Abstract
C
C     Convert an SCLK name string to a NAIF integer code.
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
C     SCLK
C
C$ Keywords
C
C     CONVERSION
C     PARSING
C     SCLK
C     TIME
C     UTILITY
C
C$ Declarations
C
C     CHARACTER*(*)         CLKNAM
C     INTEGER               CLKID
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     CLKNAM     I   String giving spacecraft clock name.
C     CLKID      O   NAIF integer code of spacecraft clock.
C     FOUND      O   Flag indicating whether item was found.
C
C$ Detailed_Input
C
C     CLKNAM         is a short string identifying the spacecraft
C                    clock of interest.  The form of the string
C                    is:
C
C                       <spacecraft name or acronym> SCLK
C                   
C                    for example
C
C                       VGR1 SCLK
C                       VOYAGER 1 SCLK
C                       GLL SCLK
C                       GALILEO ORBITER SCLK
C
C                    Case and white space (including embedded white
C                    space) are not significant.
C
C$ Detailed_Output
C
C     CLKID         is the NAIF integer code associated with the
C                   input clock.  CLKID is defined only if the
C                   output flag FOUND is returned .TRUE.
C
C     FOUND         is a logical flag indicating whether the input
C                   string specified a clock known to this routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If the input name is not recognized, FOUND is set to .FALSE.
C         CLKID is not modified.
C
C     2)  If the input name is recognized but does not refer to a
C         spacecraft, no error is signaled.  For example, the string 
C         'JUPITER BARYCENTER SCLK' maps to the code 5.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      SCN2ID provides a means of mapping human-readable clock names
C      to integer codes used by the SPICELIB SCLK routines to
C      identify spacecraft clocks.
C
C$ Examples
C
C     1)  Look up the spacecraft clock code for the Galileo orbiter.
C
C            CALL SCN2ID ( 'GLL SCLK', CLKID, FOUND )
C
C         The outputs will be
C
C            CLKID  =  -77
C            FOUND  =  .TRUE.
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
C-    SPICELIB Version 1.2.0, 12-AUG-2001 (NJB)
C
C        Bug fix:  modified algorithm to handle case where string
C        "SCLK" appears in SCLK name.
C
C-    SPICELIB Version 1.1.0, 25-FEB-2000 (NJB)
C
C        Updated to use BODTRN for SCLK name/code mapping.
C
C-    Beta Version 1.0.0, 17-NOV-1995 (NJB)
C
C
C-&
 
C$ Index_Entries
C
C     convert an SCLK name to an SCLK ID code
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0, 29-OCT-2001 (NJB)
C
C        Bug fix:  modified algorithm to handle case where string
C        "SCLK" appears in SCLK name.  SCN2ID now uses POSR to locate 
C        the substring "SCLK" in the input string.
C     
C-&

C
C     Convert name to upper case.
C  
      CALL UCASE ( CLKNAM, TMPNAM )
      
C
C     Remove the final occurrence of the  string 'SCLK' from 
C     the input name.
C
      LOC = POSR ( TMPNAM, 'SCLK', RTRIM(TMPNAM) )

      
      IF ( LOC .GT. 0 ) THEN
         TMPNAM(LOC:LOC+3) = ' '
      END IF

      CALL BODN2C ( TMPNAM, CLKID, FOUND )
       
      RETURN
 
 
 
 
 
 
 
C$Procedure  SCID2N  ( SCLK ID code to name )
 
      ENTRY SCID2N ( CLKID, CLKNAM, FOUND )
 
C$ Abstract
C
C     Convert a NAIF integer code for a spacecraft clock to an SCLK name
C     string.
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
C     SCLK
C
C$ Keywords
C
C     CONVERSION
C     PARSING
C     SCLK
C     TIME
C     UTILITY
C
C$ Declarations
C
C     INTEGER               CLKID
C     CHARACTER*(*)         CLKNAM
C     LOGICAL               FOUND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     CLKID      I   NAIF integer code of spacecraft clock.
C     CLKNAM     O   String giving spacecraft clock name.
C     FOUND      O   Flag indicating whether item was found.
C
C$ Detailed_Input
C
C     CLKID          is the NAIF integer code of a spacecraft clock of
C                    interest.
C
C$ Detailed_Output
C
C     CLKNAM         is a short, human-readable string identifying
C                    the specified spacecraft clock.  The returned 
C                    string has the form
C
C                       <spacecraft name or acronym> SCLK
C                    
C                    where the spacecraft name is the same string
C                    returned by BODC2N when CLKID is supplied as the
C                    input code.
C
C                    CLKNAM is defined only if the output flag FOUND is
C                    returned .TRUE.
C
C     FOUND          is a logical flag indicating whether the input
C                    code specified a clock known to this routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If the input code is not recognized, FOUND is set to .FALSE.
C         CLKNAM is not modified.
C
C     2)  If the input code is recognized but does not refer to a
C         spacecraft, no error is signaled.  For example, the code 
C         5 maps to the string 'JUPITER BARYCENTER SCLK'.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine converts a NAIF spacecraft clock code to a human-
C     readable string.  This function is useful for constructing
C     messages.
C
C$ Examples
C
C     1)  Look up the spacecraft clock name for code -77.
C
C            CALL SCID2N ( -77, CLKNAM, FOUND )
C
C         The outputs will be
C
C            CLKNAM  =  'GALILEO ORBITER SCLK'
C            FOUND   =  .TRUE.
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
C-    SPICELIB Version 1.1.0, 25-FEB-2000 (NJB)
C
C        Updated to use BODTRN for SCLK name/code mapping.
C
C-    Beta Version 1.0.0, 17-NOV-1995 (NJB)
C
C
C-&
 
C$ Index_Entries
C
C     convert an SCLK name to an SCLK ID code
C
C-&
 
      CALL BODC2N ( CLKID, CLKNAM, FOUND )
      
      IF ( .NOT. FOUND ) THEN
         RETURN
      END IF
      
      CALL SUFFIX ( 'SCLK', 1, CLKNAM )
 
      RETURN
      END
