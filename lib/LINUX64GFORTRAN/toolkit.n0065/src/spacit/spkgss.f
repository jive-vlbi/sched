
C$Procedure      SPKGSS ( SPK get segment summary )

      SUBROUTINE SPKGSS ( SEGID,  SEGTGT, SEGCEN, SEGFRM, SEGTYP, 
     .                    SEGBTM, SEGETM, SEGBAD, SEGEAD          )

C$ Abstract
C
C     Get the summary for the current segment in a SPK file.
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
C      SPK
C
C$ Keywords
C
C     None.
C
C$ Declarations
     
      CHARACTER*(*)         SEGID
      INTEGER               SEGTGT
      INTEGER               SEGCEN
      INTEGER               SEGFRM
      INTEGER               SEGTYP
      DOUBLE PRECISION      SEGBTM
      DOUBLE PRECISION      SEGETM
      INTEGER               SEGBAD
      INTEGER               SEGEAD
      
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      SEGID     O   Segment ID for the segment in the SPK file.
C      SEGTGT    O   Target body for the segment in the SPK file.
C      SEGCEN    O   Center body for the segment in the SPK file.
C      SEGFRM    O   Reference frame for the segment in the SPK file.
C      SEGTYP    O   Ephemeris type for the segment in the SPK file.
C      SEGBTM    O   Begin time (ET) for the segment in the SPK file.
C      SEGETM    O   End time (ET) for the segment in the SPK file.
C      SEGBAD    O   Begin address in the SPK file of the segment.
C      SEGEAD    O   End address in the SPK file of the segment.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C      SEGID    Segment ID for the current segment in an SPK file.
C      
C      SEGTGT   Target body for the current segment in an SPK file.
C               This is the NAIF integer code for the target body.
C      
C      SEGCEN   Center body for the current segment in an SPK file.
C               This is the NAIF integer code for the center body.
C      
C      SEGFRM   Inertial reference frame for the current segment in
C               an SPK file. this is the NAIF integer code for the
C               inertial reference frame.
C               
C      SEGTYP   Ephemeris type for the current segment in an SPK file.
C               This is an integer code which represents the SPK segment
C               data type.
C      
C      SEGBTM   Begin time (ET) for the current segment in an SPK file.
C      
C      SEGETM   End time (ET) for the current segment in an SPK file.
C      
C      SEGBAD   Beginning DAF address for the data of the current
C               segment in an SPK file.
C               
C      SEGEAD   Ending DAF address for the data of the current segment
C               in an SPK file.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If any errors occur, they will be signalled by routines called 
C        by this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine provides the summary of the current segment in the
C     current SPK file. An SPK file must have been opened, and a
C     search must have been initiated before this routine may called.
C
C$ Examples
C
C     None.
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
C     K.R. Gehringer (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 22-JAN-1993 (KRG) 
C
C-&

C$ Index_Entries
C
C      get the summary of the current spk segment
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED
C
C     Local parameters
C
C     Length of the double precision portion of an SPK segment summary.
C     
      INTEGER               DSLEN
      PARAMETER           ( DSLEN = 2 )
C
C     Length of the integer portion of an SPK segment summary.
C     
      INTEGER               ISLEN
      PARAMETER           ( ISLEN = 6 )
C
C     Length of a segment summary in an SPK file.
C     
      INTEGER               SUMLEN
      PARAMETER           ( SUMLEN = DSLEN + (ISLEN +1 ) / 2 )
C
C     Length of an SPK segment ID.
C     
      INTEGER               IDLEN
      PARAMETER           ( IDLEN = 8 * SUMLEN )
C
C     Set up mnemonic names for the segment begin and end time indices.
C     
      INTEGER               BTMIDX
      PARAMETER           ( BTMIDX = 1          )
      
      INTEGER               ETMIDX
      PARAMETER           ( ETMIDX = BTMIDX + 1 )
C
C     Set up mnemonic names for the target body, the center body, the
C     inertial reference frame, the ephemeris type, the begin address
C     of the segment, and the end address of the segment.
C     
      INTEGER               TGTIDX
      PARAMETER           ( TGTIDX = 1          )
      
      INTEGER               CENIDX
      PARAMETER           ( CENIDX = TGTIDX + 1 )
      
      INTEGER               FRMIDX
      PARAMETER           ( FRMIDX = CENIDX + 1 )
      
      INTEGER               TYPIDX
      PARAMETER           ( TYPIDX = FRMIDX + 1 )
      
      INTEGER               BADIDX
      PARAMETER           ( BADIDX = TYPIDX + 1 )
      
      INTEGER               EADIDX
      PARAMETER           ( EADIDX = BADIDX + 1 )
C
C     Local variables
C
      CHARACTER*(IDLEN)     TMPID
      
      DOUBLE PRECISION      DSUM  (DSLEN )
      DOUBLE PRECISION      SUMRY (SUMLEN)
      
      INTEGER               ISUM  (ISLEN)
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKGSS' )
      END IF
C
C     Get the segment ID, the segment summary, and unpack the segment
C     summary. If an error occurred during any of these operations, 
C     checkout and return to the caller, as an appropriate error message
C     will already be set. We use a temporary variable to read the
C     segment ID so that we do not modify the input value unless we get
C     one.
C     
      CALL DAFGN  ( TMPID                           )
      CALL DAFGS  ( SUMRY                           )
      CALL DAFUS  ( SUMRY, DSLEN, ISLEN, DSUM, ISUM )
            
      IF ( FAILED() ) THEN
               
         CALL CHKOUT ( 'SPKGSS' )
         RETURN
               
      END IF
C
C     Set the output values for the begin and end ET times for the
C     segment.
C     
      SEGBTM = DSUM(BTMIDX)
      SEGETM = DSUM(ETMIDX)
C
C     Set the output values for: the segment ID, the target body, the
C     center body, the inertial reference frame code, the segment data
C     type code, the begin address of the segment data, and the end
C     address of the segment data.
C     
      SEGID  = TMPID
      SEGTGT = ISUM(TGTIDX)
      SEGCEN = ISUM(CENIDX)
      SEGFRM = ISUM(FRMIDX)
      SEGTYP = ISUM(TYPIDX)
      SEGBAD = ISUM(BADIDX)
      SEGEAD = ISUM(EADIDX)
         
      CALL CHKOUT ( 'SPKGSS' )
      RETURN
      END
