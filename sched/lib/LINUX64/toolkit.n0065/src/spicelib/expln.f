C$Procedure      EXPLN ( Get Explanation for Short Error Message )
 
       SUBROUTINE EXPLN ( MSG, EXPL )
 
C$ Abstract
C
C      Return the explanation of a short error message.
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
C      ERROR
C
C$ Keywords
C
C      ERROR
C
C$ Declarations
 
       CHARACTER*(*)                 MSG
       CHARACTER*(*)                 EXPL
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      MSG        I   A short error message.
C      EXPL       O   The explanation of the short error message.
C
C$ Detailed_Input
C
C      MSG     A ``short'' error message.
C              MSG indicates the type of error that has occurred.
C
C              The exact format that MSG must follow is
C              described in the required reading file, error.req.
C
C$ Detailed_Output
C
C      EXPL    is a character string containing an one-line
C              explanation of the short error message, MSG.
C
C              If there is no explanatory text corresponding
C              to the input string, MSG, EXPL is blank.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C
C      This routine does not detect any errors.
C
C      However, this routine is part of the interface to the
C      SPICELIB error handling mechanism.  For this reason,
C      this routine does not participate in the trace scheme,
C      even though it has external references.
C
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      None.
C
C$ Examples
C
C
C      C
C      C     We want to find the explanation corresponding to
C      C     the short message, 'SPICE(ZERORADIUS)' :
C      C
C
C             CALL EXPLN ( 'SPICE(ZERORADIUS)', EXPL )
C
C
C      Now, EXPL  =
C
C      'Invalid Radius--Equatorial or Polar Radius is Zero'
C
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 18-APR-2014 (BVS)
C
C        Minor header edits.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     get explanation for short error message
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 1.1.0, 27-OCT-1988 (NJB)
C
C        Removed code used to create upper case, left-justified
C        copy of the short error message.  The resulting message
C        was not used.
C
C-&
 
 
 
C
C     Executable Code:
C
 
C
C     Note: the short error messages should be ordered
C     alphabetically.
C
 
      IF      (  MSG  .EQ.  'SPICE(BADENDPOINTS)' ) THEN
 
         EXPL  = 'Invalid Endpoints--Left Endpoint Exceeds Right'     //
     .           ' Endpoint'
 
      ELSE IF (  MSG  .EQ.  'SPICE(BADGEFVERSION)'  ) THEN
 
         EXPL  =  'Version Identification of GEF File is Invalid'
 
      ELSE IF (  MSG  .EQ.  'SPICE(BLANKMODULENAME)' ) THEN
 
         EXPL  =  'A blank string was used as a module name'
 
      ELSE IF (  MSG  .EQ.  'SPICE(BOGUSENTRY)'   ) THEN
 
         EXPL  = 'This Entry Point Contains No Executable Code'
 
      ELSE IF (  MSG  .EQ.  'SPICE(CELLTOOSMALL)' ) THEN
 
         EXPL  = 'Cardinality of Output Cell is Too Small'
 
      ELSE IF (  MSG  .EQ.  'SPICE(CLUSTERWRITEERROR)' ) THEN
 
         EXPL  = 'Error Writing to Ephemeris File'
 
      ELSE IF (  MSG  .EQ.  'SPICE(DATATYPENOTRECOG)' )  THEN
 
         EXPL  = 'Unrecognized Data Type Specification was Encountered'
 
 
      ELSE IF (  MSG  .EQ.  'SPICE(DATEEXPECTED)'  ) THEN
 
         EXPL  = 'The Value in the Kernel File was Expected to be a ' //
     .           'date.'
 
      ELSE IF (  MSG  .EQ.  'SPICE(DEVICENAMETOOLONG)' ) THEN
 
         EXPL  = 'Name of Device Exceeds 128-Character Limit'
 
      ELSE IF (  MSG  .EQ.  'SPICE(EMBEDDEDBLANK)' ) THEN
 
         EXPL  = 'Invalid embedded blank was found in character string'
 
      ELSE IF (  MSG  .EQ.  'SPICE(FILEALREADYOPEN)' ) THEN
 
         EXPL  = 'File Open Failed Because the File was Already Open'
 
      ELSE IF (  MSG  .EQ.  'SPICE(FILEOPENFAILED)'  ) THEN
 
         EXPL  = 'An Attempt to Open a File Failed'
 
      ELSE IF (  MSG  .EQ.  'SPICE(FILEREADFAILED)'  ) THEN
 
         EXPL  = 'An Attempt to Read a File Failed'
 
      ELSE IF (  MSG  .EQ.  'SPICE(FILEWRITEFAILED)' ) THEN
 
         EXPL  = 'An Attempt to Write a File Failed'
 
      ELSE IF (  MSG  .EQ.  'SPICE(INCOMPATIBLEUNITS)' )  THEN
 
         EXPL  = 'The Input and Output Units are Incompatible'
 
      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDACTION)'  ) THEN
 
         EXPL  = 'An Invalid Action Value Was Supplied'
 
      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDARGUMENT)'  ) THEN
 
         EXPL  = 'An Invalid Function Argument was Supplied'
 
      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDCHECKOUT)' )  THEN
 
         EXPL  = 'Checkout Was Attempted When No Routines Were'       //
     .           ' Checked In'
 
      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDCLUSTERNUM)'  ) THEN
 
         EXPL  = 'Invalid Cluster Number -- Cluster Numbers Must'     //
     .           ' Exceed 1 '
 
      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDEPOCH)' ) THEN
 
         EXPL  = 'An Invalid Epoch Type Specification Was Supplied'
 
      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDINDEX)' ) THEN
 
         EXPL  = 'There Is No Element Corresponding to the'           //
     .           ' Supplied Index'
 
      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDTIMESTRING)'  ) THEN
 
         EXPL  = 'Time String Could Not Be Parsed'
 
      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDLISTITEM)' )  THEN
 
         EXPL  = 'An Invalid Item Was Found in a List'
 
      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDMSGTYPE)'  )  THEN
 
         EXPL  = 'An Invalid Error Message Type Was Specified'
 
      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDOPERATION)' ) THEN
 
         EXPL  = 'An Invalid Operation Value Was Supplied'
 
      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDOPTION)'  )  THEN
 
         EXPL  = 'An Invalid Option Value Was Supplied'
 
      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDTIMEFORMAT)')  THEN
 
         EXPL  = 'Specification of Time String Format Was Not '       //
     .           'Recognized'
 
      ELSE IF (  MSG  .EQ.  'SPICE(KERNELVARNOTFOUND)' )  THEN
 
         EXPL  = 'The Variable Was not Found in the Kernel Pool.'
 
      ELSE IF (  MSG  .EQ.  'SPICE(NAMETABLEFULL)')  THEN
 
         EXPL  = 'No Further Symbols Can be Inserted; the Name Table'//
     .           ' is Full'
 
      ELSE IF (  MSG  .EQ.  'SPICE(NOFREELOGICALUNIT)' ) THEN
 
         EXPL  = 'No More Logical Units are Available for Allocation'
 
      ELSE IF (  MSG  .EQ.  'SPICE(NOINTERVAL)'  )  THEN
 
         EXPL  = 'Window Does Not Contain Interval Corresponding'     //
     .           ' to the Supplied Index'
 
      ELSE IF (  MSG  .EQ.  'SPICE(NOSEGMENT)'  )  THEN
 
         EXPL  = 'No Applicable Segment Found in Ephemeris File'
 
      ELSE IF (  MSG  .EQ.  'SPICE(NOSUCHSYMBOL)' ) THEN
 
         EXPL  = 'The Symbol Does Not Exist in the Symbol Table'
 
      ELSE IF (  MSG  .EQ.  'SPICE(NOTDISTINCT)' ) THEN
 
         EXPL  = 'The Elements Must Be Distinct'
 
      ELSE IF (  MSG  .EQ.  'SPICE(NUMBEREXPECTED)'  )  THEN
 
         EXPL  = 'The Value in the Kernel File was Expected '         //
     .           'to be a Number.'
 
      ELSE IF (  MSG  .EQ.  'SPICE(POINTERTABLEFULL)' )   THEN
 
         EXPL  = 'No Further Symbols Can be Inserted; the Pointer '   //
     .           'Table is Full'
 
      ELSE IF (  MSG  .EQ.  'SPICE(REFNOTREC)'    ) THEN
 
         EXPL  = 'A Reference Frame Specification was Not'            //
     .           ' Recognized'
 
      ELSE IF (  MSG  .EQ.  'SPICE(SETEXCESS)'    ) THEN
 
         EXPL  = 'Cardinality of Set Is Too Small to Contain'         //
     .           ' Result of the Requested Operation'
 
      ELSE IF (  MSG  .EQ.  'SPICE(TOOMANYFILESOPEN)'  ) THEN
 
         EXPL  = 'The SPICELIB Limit for Number of Open Files'        //
     .           ' Has Already Been Reached'
 
      ELSE IF (  MSG  .EQ.  'SPICE(TRACEBACKOVERFLOW)'  ) THEN
 
         EXPL  = 'No More Entries Can Be Added to the Traceback'      //
     .           ' Representation'
 
      ELSE IF (  MSG  .EQ.  'SPICE(UNITSNOTREC)'  ) THEN
 
         EXPL  = 'The Input or Output Units Were Not Recognized'
 
      ELSE IF (  MSG  .EQ.  'SPICE(UNMATCHENDPTS)') THEN
 
         EXPL  = 'Window Does Not Have an Even Number of Endpoints'
 
      ELSE IF (  MSG  .EQ.  'SPICE(VALUETABLEFULL)' )  THEN
 
         EXPL  = 'No Further Symbols Can be Inserted; the Value '     //
     .           'Table is Full'
 
      ELSE IF (  MSG  .EQ.  'SPICE(WINDOWEXCESS)' ) THEN
 
         EXPL  = 'Cardinality of Window Is Too Small to Contain'      //
     .           ' Result of the Requested Operation'
 
      ELSE IF (  MSG  .EQ.  'SPICE(WINDOWTOOSMALL)' )  THEN
 
         EXPL  = 'Cardinality of Output Window is Too Small'
 
      ELSE IF (  MSG  .EQ.  'SPICE(WRITEERROR)'  ) THEN
 
 
         EXPL  = 'An Attempt to write to a specified unit failed.'
 
 
      ELSE IF (  MSG  .EQ.  'SPICE(ZERORADIUS)'   ) THEN
 
         EXPL  = 'Invalid Radius--Equatorial or Polar Radius is Zero'
 
 
      ELSE IF (  MSG  .EQ.  'SPICE(ZEROVECTOR)'  ) THEN
 
         EXPL  = 'Input Vector is the Zero Vector'
 
 
      ELSE IF (  MSG  .EQ.  'SPICE(ZEROAXISLENGTH)'  )  THEN
 
         EXPL  = 'Input Axis Length is Zero'
 
 
      ELSE
 
         EXPL  = ' '
 
      END IF
 
      END
 
 
