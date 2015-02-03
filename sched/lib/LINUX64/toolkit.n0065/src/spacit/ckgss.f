
C$Procedure      CKGSS ( CK get segment summary )

      SUBROUTINE CKGSS ( SEGID,  SEGINS, SEGFRM, SEGTYP, SEGRTS, 
     .                   SEGBTM, SEGETM, SEGBAD, SEGEAD          )

C$ Abstract
C
C     Get the summary for the current segment in a CK file.
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
C      None.
C
C$ Keywords
C
C     None.
C
C$ Declarations
     
      CHARACTER*(*)         SEGID
      INTEGER               SEGINS
      INTEGER               SEGFRM
      INTEGER               SEGTYP
      INTEGER               SEGRTS
      DOUBLE PRECISION      SEGBTM
      DOUBLE PRECISION      SEGETM
      INTEGER               SEGBAD
      INTEGER               SEGEAD
      
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      SEGID     O   Segment ID for the segment in the CK file.
C      SEGINS    O   ID for the instrument having data in the segment.
C      SEGFRM    O   Reference frame for a segment in the cK file.
C      SEGTYP    O   Data type for a segment in the CK file.
C      SEGRTS    O   Flag for velocity info in the CK segment.
C      SEGBTM    O   Begin time (SCLK) for a segment in the CK file.
C      SEGETM    O   End time (SCLK) for a segment in the CK file.
C      SEGBAD    O   Begin address in the CK file of a segment.
C      SEGEAD    O   End address in the CK file of a segment.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C      SEGID    Segment ID for the current segment in a CK file.
C      
C      SEGINS   NAIF integer ID code for the instrument having data
C               in the current segment in a CK file.
C               
C      SEGFRM   Inertial reference frame for the current segment in a
C               CK file. This is the NAIF integer code for the inertial
C               reference frame.
C               
C      SEGTYP   Data type for the current segment in a CK file. This
C               is an integer code which specifies the type of the data
C               in the current segment.
C               
C      SEGRTS   Integer flag which indicates whether the segment
C               contains angular velocity data in addition to pointing
C               data, SEGRTS .EQ. 1, or just pointing data, SEGRTS .EQ.
C               0.
C               
C      SEGBTM   The beginning encoded SCLK time for the data in the
C               current segment in a CK file.
C               
C      SEGETM   The ending encoded SCLK time for the data in the
C               current segment in a CK file.
C      
C      SEGBAD   Begining DAF address for the data in the current
C               segment of a CK file.
C               
C      SEGEAD   Ending DAF address for the data in the current segment
C               of a CK file.
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
C     This routine will provide the summary of the current segment
C     in the current CK file. A search must have been initiated
C     before this routine is called.
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
C-    Beta Version 1.0.0, 22-APR-1994 (KRG) 
C
C-&

C$ Index_Entries
C
C      summarize the current segment in a ck file
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
C     Length of the double precision portion of a CK segment summary.
C     
      INTEGER               DSLEN
      PARAMETER           ( DSLEN = 2 )
C
C     Length of the integer portion of a CK segment summary.
C     
      INTEGER               ISLEN
      PARAMETER           ( ISLEN = 6 )
C
C     Length of a segment summary in an CK file.
C     
      INTEGER               SUMLEN
      PARAMETER           ( SUMLEN = DSLEN + (ISLEN + 1) / 2 )
C
C     Length of an CK segment ID.
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
C     Set up mnemonic names for the instrument, the inertial reference
C     frame, the data type, the rates flag, the begin address of the
C     segment, and the end address for the segment.
C     
      INTEGER               INSIDX
      PARAMETER           ( INSIDX = 1          )
      
      INTEGER               FRMIDX
      PARAMETER           ( FRMIDX = INSIDX + 1 )
      
      INTEGER               TYPIDX
      PARAMETER           ( TYPIDX = FRMIDX + 1 )
      
      INTEGER               RTSIDX
      PARAMETER           ( RTSIDX = TYPIDX + 1 )
      
      INTEGER               BADIDX
      PARAMETER           ( BADIDX = RTSIDX + 1 )
      
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
         CALL CHKIN ( 'CKGSS' )
      END IF
C
C     Get the segment ID, the segment summary, and unpack the segment
C     summary. If an error occurrs during any of these operations 
C     checkout and return to the caller, as an appropriate error message
C     will already be set. We use a temporary variable to read the
C     segment ID so that we do not modify the input value unless we get
C     an ID.
C     
      CALL DAFGN  ( TMPID                           )
      CALL DAFGS  ( SUMRY                           )
      CALL DAFUS  ( SUMRY, DSLEN, ISLEN, DSUM, ISUM )
            
      IF ( FAILED() ) THEN
               
         CALL CHKOUT ( 'CKGSS' )
         RETURN
               
      END IF
C
C     Set the output values for the begin and end encoded SCLK times
C     for the segment.
C     
      SEGBTM = DSUM(BTMIDX)
      SEGETM = DSUM(ETMIDX)
C
C     Set the output values for: the segment ID, the instrument, the
C     inertial reference frame code, the segment data type code, the
C     rates flag, the begin address of the segment data, and the end
C     address of the segment data.
C     
      SEGID  = TMPID
      SEGINS = ISUM(INSIDX)
      SEGFRM = ISUM(FRMIDX)
      SEGTYP = ISUM(TYPIDX)
      SEGRTS = ISUM(RTSIDX)
      SEGBAD = ISUM(BADIDX)
      SEGEAD = ISUM(EADIDX)
         
      CALL CHKOUT ( 'CKGSS' )
      RETURN
      END
      
