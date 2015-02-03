C$Procedure ZZNSPPOK (Private Routine -- NSPIO Port)
 
      INTEGER FUNCTION ZZNSPPOK ( PORT, NPORTS, PORTS )
 
C$ Abstract
C
C     Find the integer associated with an NSPIO PORT string.
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
C     TEXT
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
 
      CHARACTER*(*)         PORT
      INTEGER               NPORTS
      CHARACTER*(*)         PORTS ( * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I   is a string indicating the port to find.
C     NPORTS     I   is the number of ports in the PORTS array.
C     PORTS      I   an array of strings containing the possible ports.
C
C     The function returns an integer that represents the position
C     of PORT in the PORTS array.
C
C$ Detailed_Input
C
C     PORT       is the name of a port supported by the NSPIO
C                umbrella, and is an entry in the PORTS array.
C
C     NPORTS     is the number of entries in the PORTS arrray.
C
C     PORTS      is a list of acceptable PORTs supported by the
C                NSPIO umbrella.
C
C$ Detailed_Output
C
C     The function returns an integer that represents the position
C     of PORT in the PORTS array.  This integer is used in NSPIO
C     to access information in parallel arrays.
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
C     1) If PORT is not found in the PORTS array, then the error
C        NSPIO(UNKNOWNPORT) is signaled.
C
C$ Particulars
C
C     This private routine is simply a place to consolidate the
C     PORT to integer code conversion.
C
C$ Examples
C
C     This routine is a simple private routine. See NSPIO and its
C     entry points for samples of its usage.
C
C$ Restrictions
C
C     1) NPORTS must not be greater than the number of available
C        members of the PORTS array, else memory violation will
C        occur.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    NSPIO Version 2.0.0, 01-FEB-2000 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               ISRCHC
 
C
C     Local Variables
C
      INTEGER               ID
 
C
C     Find PORT in the PORTS array.
C
      ID = ISRCHC ( PORT, NPORTS, PORTS )
 
C
C     Set ZZNSPPOK to the return value.
C
      ZZNSPPOK = ID
 
C
C     Check to see if we were able to find the integer ID of PORT.
C     If not, use discovery check in/out and signal an error.
C
      IF ( ID .EQ. 0 ) THEN
 
         CALL CHKIN  ( 'ZZNSPPOK'                   )
         CALL SETMSG ( '$ is an unrecognized port.' )
         CALL ERRCH  ( '$', PORT                    )
         CALL SIGERR ( 'NSPIO(UNKNOWNPORT)'         )
         CALL CHKOUT ( 'ZZNSPPOK'                   )
 
      END IF
 
      RETURN
 
      END
