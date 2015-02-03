C$Procedure ERRHAN ( Insert DAF/DAS file name into long error message )
 
      SUBROUTINE ERRHAN ( MARKER, HANDLE )
      IMPLICIT NONE
 
C$ Abstract
C
C     Substitute the first occurrence of a marker in the current long
C     error message with the file name associated with a given
C     DAF/DAS handle.  (Works for DAF only for N0052.)
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
C     ERROR
C
C$ Keywords
C
C     DAF
C     DAS
C     ERROR
C     STRING
C
C$ Declarations
 
      INCLUDE 'zzddhman.inc'
 
      CHARACTER*(*)         MARKER
      INTEGER               HANDLE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     MARKER     I   A substring in the long error message to be
C                    replaced.
C     HANDLE     I   DAF/DAS handle associated with a file.
C     FILEN      P   Maximum length of filename.
C
C$ Detailed_Input
C
C     MARKER     is a character string that marks a position in
C                the long error message where a file name is to be
C                substituted.  Leading and trailing blanks in MARKER
C                are not significant.
C
C                Case IS significant;  'XX' is considered to be
C                a different marker from 'xx'.
C
C     HANDLE     is the DAF/DAS handle associated with the file of
C                interest.  HANDLE must be associated with a currently
C                loade DAF or DAS file.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     FILEN      is the maximum file name length that can be
C                accommodated by this routine.  Currently this
C                parameter is defined in the include file 
C                zzddhman.inc.
C
C$ Exceptions
C
C     Error free.
C
C
C     1) If HANDLE refers to a scratch DAS file, the string inserted
C        into the long error message is
C
C           'DAS SCRATCH FILE'
C
C     2) If HANDLE is not associated with a loaded DAF or DAS file,
C        the string inserted into the long error message is:
C
C           '<No name found for handle #>'
C
C        where the handle number is substituted for the marker '#'.
C
C$ Files
C
C     See "Detailed_Input" description of the variable HANDLE.
C
C$ Particulars
C
C     This routine provides a convenient and error-free mechanism
C     for inserting a DAF or DAS file name into an error message,
C     given the file handle associated with the file of interest.
C
C$ Examples
C
C     1) Create an error message pertaining to an SPK file
C        designated by HANDLE, then signal an error.
C
C           CALL SETMSG ( 'SPK file # contains a type 3 segment ' //
C          .              'with invalid polynomial degree #. '    //
C          .              'Segment index in file is #.'            )
C           CALL ERRHAN ( '#',  HANDLE                             )
C           CALL ERRINT ( '#',  DEGREE                             )
C           CALL ERRINT ( '#',  I                                  )
C           CALL SIGERR ( 'SPICE(INVALIDDEGREE)'                   )
C
C$ Restrictions
C
C     1) This routine works only for DAF files in the N0052 Toolkit
C        version.  It will for for both DAF and DAS files for later
C        Toolkit versions.
C
C     2) The supported filename length is limited by the parameter 
C        FILEN.
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
C-    SPICELIB Version 1.0.0, 04-JAN-2002 (NJB)
C
C-&
 
C$ Index_Entries
C
C     insert filename into long error message
C
C-&
 
C
C     Local parameters
C
      INTEGER               MAXNUM
      PARAMETER           ( MAXNUM = 32 )

C
C     Local variables
C
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(MAXNUM)    NUMSTR
 
      INTEGER               INTARC
      INTEGER               INTBFF
      INTEGER               INTAMH
 
      LOGICAL               FOUND
 
C
C     Get the name of the file designated by the input handle.
C
      CALL ZZDDHNFO ( HANDLE, FNAME, INTARC, INTBFF, INTAMH, FOUND )
  
      IF ( .NOT. FOUND ) THEN

         CALL INTSTR ( HANDLE, NUMSTR )

         FNAME = '<No name found for handle '
         CALL SUFFIX ( NUMSTR, 1, FNAME )
         CALL SUFFIX ( '>',    0, FNAME )
 
      END IF
 
C
C     Insert the file name string into the long error message.
C
      CALL ERRCH ( MARKER, FNAME )
 
      RETURN
      END
