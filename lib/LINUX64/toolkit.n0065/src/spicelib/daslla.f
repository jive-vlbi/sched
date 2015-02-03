C$Procedure      DASLLA ( DAS, last logical addresses )
 
      SUBROUTINE DASLLA ( HANDLE, LASTC, LASTD, LASTI )
 
C$ Abstract
C
C     Return last DAS logical addresses of character, double precision
C     and integer type that are currently in use in a specified DAS
C     file.
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
C     DAS
C
C$ Keywords
C
C     ARRAY
C     DAS
C     UTILITY
C
C$ Declarations
 
      INTEGER               HANDLE
      INTEGER               LASTC
      INTEGER               LASTD
      INTEGER               LASTI
 
      INTEGER               CHR
      PARAMETER           ( CHR    =  1  )
 
      INTEGER               DP
      PARAMETER           ( DP     =  2  )
 
      INTEGER               INT
      PARAMETER           ( INT    =  3  )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   DAS file handle.
C     LASTC      O   Last character address in use.
C     LASTD      O   Last double precision address in use.
C     LASTI      O   Last integer address in use.
C     CHR        P   Parameter indicating character data type.
C     DP         P   Parameter indicating double precision data type.
C     INT        P   Parameter indicating integerer data type.
C
C$ Detailed_Input
C
C     HANDLE         is the file handle of a DAS file whose active
C                    logical address ranges are desired.
C
C$ Detailed_Output
C
C     LASTC,
C     LASTD,
C     LASTI          are, respectively, the last logical addresses of
C                    character, double precision, and integer type in
C                    use in the specified DAS file.
C
C$ Parameters
C
C     CHR,
C     DP,
C     INT            are data type specifiers which indicate
C                    `character', `double precision', and `integer'
C                    respectively.  These parameters are used in
C                    all DAS routines that require a data type
C                    specifier as input.
C
C$ Exceptions
C
C     1)  If the input file handle is invalid, the error will be
C         diagnosed by routines called by this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is a utility that allows a calling program to
C     find the range of logical addresses currently in use in any
C     DAS file.
C
C$ Examples
C
C     1)  Create a DAS file containing 10 integers, 5 double precision
C         numbers, and 4 characters, then use DASLLA to find the logical
C         address ranges in use.
C
C            C
C            C     Use a scratch file, since there's no reason to keep
C            C     the file.
C            C
C            C
C                  CALL DASOPS ( HANDLE )
C
C                  DO I = 1, 10
C                     CALL DASADI ( HANDLE, 1, I )
C                  END DO
C
C                  DO I = 1, 5
C                     CALL DASADD ( HANDLE, 1, DBLE(I) )
C                  END DO
C
C                  CALL DASADC ( HANDLE, 1, 'SPUD' )
C
C            C
C            C     Now check the logical address ranges.
C            C
C                  CALL DASLLA ( HANDLE, LASTC, LASTD, LASTI )
C
C                  WRITE (*,*) 'Last character address in use: ', LASTC
C                  WRITE (*,*) 'Last d.p. address in use:      ', LASTD
C                  WRITE (*,*) 'Last integer address in use:   ', LASTI
C
C
C         The output of this code fragment should be:
C
C                  Last character address in use:  4
C                  Last d.p. address in use:       5
C                  Last integer address in use:    10
C
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
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     return last logical addresses in DAS file
C     return logical address range of DAS file
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               FREE
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               LASTLA ( 3 )
      INTEGER               LASTRC ( 3 )
      INTEGER               LASTWD ( 3 )
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASLLA' )
      END IF
 
C
C     The file summary for the indicated DAS file contains all of the
C     information we need.
C
      CALL DASHFS ( HANDLE,
     .              NRESVR,
     .              NRESVC,
     .              NCOMR,
     .              NCOMC,
     .              FREE,
     .              LASTLA,
     .              LASTRC,
     .              LASTWD )
 
      LASTC = LASTLA(CHR)
      LASTD = LASTLA(DP )
      LASTI = LASTLA(INT)
 
      CALL CHKOUT ( 'DASLLA' )
      RETURN
      END
