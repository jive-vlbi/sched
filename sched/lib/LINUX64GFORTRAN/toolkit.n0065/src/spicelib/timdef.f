C$Procedure      TIMDEF ( Time Software Defaults )
 
      SUBROUTINE TIMDEF ( ACTION, ITEM, VALUE )
 
C$ Abstract
C
C     Set and retrieve the defaults associated with calendar
C     input strings.
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
C      TIME
C
C$ Declarations
 
 
      IMPLICIT NONE
      CHARACTER*(*)         ACTION
      CHARACTER*(*)         ITEM
      CHARACTER*(*)         VALUE
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ACTION     I   is the kind of action to take 'SET' or 'GET'.
C     ITEM       I   is the default item of interest.
C     VALUE     I/O  is the value associated with the default item.
C
C$ Detailed_Input
C
C     ACTION     is a word that specifies whether TIMDEF sets the
C                value associated with ITEM or retrieves the value
C                associated with ITEM.  The allowed values for
C                ACTION are 'SET' and 'GET'.  The routine is not
C                sensitive to the case of the letters in ACTION.
C
C     ITEM       is the default items whose value should be set or
C                retrieved.  The items that may be requested are:
C
C                ITEM        Allowed Values
C                ---------   --------------
C                CALENDAR    GREGORIAN
C                            JULIAN
C                            MIXED
C
C                SYSTEM      TDB
C                            TDT
C                            UTC
C
C                ZONE        EST, EDT, CST, CDT, MST, MDT, PST, PDT
C                            UTC+HR
C                            UTC-HR       ( 0 <= HR < 13 )
C                            UTC+HR:MN    ( 0 <= MN < 60 )
C                            UTC-HR:MN
C
C                The case of ITEM is not significant.
C
C
C     VALUE      if the action is 'SET' then VALUE is an input and
C                is the value to be associated with ITEM.  Note that
C                VALUE is checked to ensure it is within the range
C                of allowed values for ITEM.  If it is not within
C                the expected range and appropriate error message
C                is signalled.  The case of VALUE is not significant.
C
C$ Detailed_Output
C
C     VALUE      if the action is 'GET' then VALUE will be the
C                value associated with the requested ITEM.  Note that
C                when time zones are set, they are translated to the
C                UTC offset form ( UTC(+/-)HR[:MN] ).  When VALUE is
C                an output it will be in upper case.
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
C     1) If the ACTION specified is not SET or GET the error
C        'SPICE(BADACTION)' is signalled.
C
C     2) If the ITEM specified is not one the recognized items
C        the error 'SPICE(BADTIMEITEM)' is signalled.
C
C     3) If the value associated with a 'SET', item input
C        is not one of the recognized items, the error
C        'SPICE(BADDEFAULTVALUE)' is signalled.
C
C$ Particulars
C
C     This routine exists to allow SPICE toolkit users to alter
C     the default interpretation of time strings made by the
C     routine STR2ET.
C
C     Normally, unlabelled time strings are assumed to belong to
C     the Gregorian Calendar and are UTC times.  However, you
C     may alter the default behavior by calling TIMDEF.
C
C     Calendar
C     --------
C
C     You may set the calendar to be one of the following
C
C     Gregorian   --- This is the calendar used daily the
C                     Western Hemisphere.  Leap years occur in this
C                     calendar every 4 years except on centuries
C                     such as 1900 that are not divisible by 400.
C
C     Julian      --- This is the calendar that was in use prior
C                     to October 15, 1582.  Leap years occur every
C                     4 years on the Julian Calendar (including all
C                     centuries.)  October 5, 1582 on the Julian
C                     calendar corresponds to October 15, 1582 of the
C                     Gregorian Calendar.
C
C     Mixed       --- This calendar uses the Julian calendar
C                     for days prior to October 15, 1582 and
C                     the Gregorian calendar for days on or after
C                     October 15, 1582.
C
C     To set the default calendar, select on of the above for VALUE
C     and make the following call.
C
C        CALL TIMDEF ( 'SET', 'CALENDAR', VALUE )
C
C
C     System
C     -------
C
C     You may set the system used for keeping time to be UTC (default)
C     TDB (barycentric dynamical time) or TDT (terrestrial dynamical
C     time).  Both TDB and TDT have no leapseconds.  As such the time
C     elapsed between any two epochs on these calendars does not depend
C     upon when leapseconds occur.
C
C     To set the default time system, select TDT, TDB or UTC for VALUE
C     and make the following call.
C
C        CALL TIMDEF ( 'SET', 'SYSTEM', VALUE )
C
C     Note that such a call has the side effect of setting the value
C     associated with ZONE to a blank.
C
C     Zone
C     -----
C
C     You may alter the UTC system by specifying a time zone (UTC
C     offset).  For example you may specify that epochs are referred
C     to Pacific Standard Time (PST --- UTC-7).  The standard
C     abbreviations for U.S. time zones are recognized:
C
C        EST   UTC-5
C        EDT   UTC-4
C        CST   UTC-6
C        CDT   UTC-5
C        MST   UTC-7
C        MDT   UTC-6
C        PST   UTC-8
C        PDT   UTC-7
C
C     In addition you may specify any commercial time zone by using
C     "offset" notation.  This notation starts with the letters 'UTC'
C     followed by a + for time zones east of Greenwich and - for
C     time zones west of Greenwich.  This is followed by the number
C     of hours to add or subtract from UTC.  This is optionally followed
C     by a colon ':' and the number of minutes to add or subtract (based
C     on the sign that follows 'UTC') to get the
C     local time zone.  Thus to specify the time zone of Calcutta you
C     would specify the time zone to be UTC+5:30.  To specify the
C     time zone of Newfoundland use the time zone UTC-3:30.
C
C     To set a default time zone, select one of the "built-in" U.S.
C     zones or construct an offset as discussed above.  Then make the
C     call
C
C        CALL TIMDEF ( 'SET', 'ZONE', VALUE )
C
C     If you 'GET' a 'ZONE' it will either be blank, or have the
C     form 'UTC+/-HR[:MN]'
C
C     Note that such a call has the side effect of setting the value
C     associated with SYSTEM to a blank.
C
C$ Examples
C
C     Suppose you wish to modify the behavior of STR2ET so that
C     it interprets unlabeled time strings as being times in
C     Pacific Daylight Time and that you want the calendar to use
C     to be the "Mixed" calendar.  The following two calls will
C     make the desired changes to the behavior of STR2ET
C
C         CALL TIMDEF ( 'SET', 'CALENDAR', 'MIXED' )
C         CALL TIMDEF ( 'SET', 'ZONE',     'PDT'   )
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 26-MAY-1998 (WLT)
C
C        The previous version did not check out and return
C        when an error was detected in the if block that 
C        begins with
C
C           ELSE IF ( MYITEM .EQ. 'ZONE' ) THEN
C
C        The routine did eventually check out and return so
C        that the trace stack was maintained correctly, but
C        the default time zone would be modified which was not
C        the desired behavior.
C
C-    SPICELIB Version 1.1.0, 27-JUN-1997 (WLT)
C
C        The previous version failed to check out when 
C        the default value was set.
C
C-    SPICELIB Version 1.0.0, 13-NOV-1996 (WLT)
C
C
C-&
 
 
C$ Index_Entries
C
C     Change time software defaults.
C     Time Zones
C     Gregorian and Julian Calendars
C
C-&
C
 
C
C     SPICELIB Functions
C
      INTEGER               ISRCHC
      LOGICAL               RETURN
 
C
C     Local Variables.
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 16 )
 
      INTEGER               NZONES
      PARAMETER           ( NZONES = 8 )
 
      CHARACTER*(WDSIZE)    DEFZON
      CHARACTER*(WDSIZE)    DEFSYS
      CHARACTER*(WDSIZE)    DEFCAL
 
      CHARACTER*(WDSIZE)    ZONES  ( NZONES )
      CHARACTER*(WDSIZE)    TRNSLT ( NZONES )
 
      CHARACTER*(WDSIZE)    MYACTN
      CHARACTER*(WDSIZE)    MYITEM
      CHARACTER*(WDSIZE)    MYVAL
 
      INTEGER               LAST
      INTEGER               ZONE
 
      DOUBLE PRECISION      HOFF
      DOUBLE PRECISION      MOFF
 
      LOGICAL               SUCCES
 
      SAVE
 
      DATA                  DEFSYS / 'UTC'       /
      DATA                  DEFZON / ' '         /
      DATA                  DEFCAL / 'GREGORIAN' /
 
      DATA                  ZONES   / 'EST',    'EDT',
     .                                'CST',    'CDT',
     .                                'MST',    'MDT',
     .                                'PST',    'PDT'  /
 
      DATA                  TRNSLT  / 'UTC-5',  'UTC-4',
     .                                'UTC-6',  'UTC-5',
     .                                'UTC-7',  'UTC-6',
     .                                'UTC-8',  'UTC-7'  /
 
 
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'TIMDEF' )
 
C
C     Normalize the input.
C
      CALL LJUST ( ACTION, MYACTN )
      CALL UCASE ( MYACTN, MYACTN )
 
      CALL LJUST ( ITEM,   MYITEM )
      CALL UCASE ( MYITEM, MYITEM )
 
      CALL LJUST ( VALUE,  MYVAL  )
      CALL UCASE ( MYVAL,  MYVAL  )
 
C
C     Admittedly, the decision making below is not very elegant.
C     However, this works and is simpler than anything that comes
C     to mind at the moment and allows us to give quite specific
C     diagnostic messages easily.
C
      IF ( MYACTN .EQ. 'SET' ) THEN
 
         IF      ( MYITEM .EQ. 'SYSTEM' ) THEN
 
            IF (       MYVAL .EQ. 'TDB'
     .            .OR. MYVAL .EQ. 'TDT'
     .            .OR. MYVAL .EQ. 'UTC' ) THEN
 
               DEFZON = ' '
               DEFSYS = MYVAL
 
            ELSE
               CALL SETMSG ( 'The default value assigned to the '
     .         //            'time system must be one of ''UTC'', '
     .         //            '''TDT'', or ''TDB''. The value '
     .         //            'supplied was ''#''. ' )
 
               CALL ERRCH  ( '#', VALUE )
               CALL SIGERR ( 'SPICE(BADDEFAULTVALUE)'  )
               CALL CHKOUT ( 'TIMDEF' )
               RETURN
            END IF
 
 
         ELSE IF ( MYITEM .EQ. 'ZONE' ) THEN
 
            ZONE = ISRCHC ( MYVAL, NZONES, ZONES )
 
C
C           If MYVAL was one of the recognized time zones, we
C           translate it to the UTC offset form.
C
            IF ( ZONE .GT. 0 ) THEN
               MYVAL = TRNSLT ( ZONE )
            END IF
 
            CALL PREFIX  ( '::',  0, MYVAL                    )
            CALL ZZUTCPM ( MYVAL, 1, HOFF, MOFF, LAST, SUCCES )
 
            IF ( .NOT. SUCCES ) THEN
 
               CALL SETMSG ( 'The input value for a time zone '
     .         //            '"#" was not recognized as known '
     .         //            'time zone and could not be parsed '
     .         //            'according to the pattern '
     .         //            'UTC(+/-)HR[:MN]. Known time zones '
     .         //            'are: ''EST'', ''EDT'', ''CST'', '
     .         //            '''CDT'', ''MST'', ''MDT'', '
     .         //            '''PST'', and ''PDT''. ' )
 
               CALL ERRCH  ( '#', VALUE               )
               CALL SIGERR ( 'SPICE(BADDEFAULTVALUE)' )
               CALL CHKOUT ( 'TIMDEF'                 )
               RETURN
 
            END IF
 
 
            DEFZON = MYVAL(3:)
            DEFSYS = ' '
 
         ELSE IF ( MYITEM .EQ. 'CALENDAR' )  THEN
 
            IF (       MYVAL .EQ. 'JULIAN'
     .            .OR. MYVAL .EQ. 'GREGORIAN'
     .            .OR. MYVAL .EQ. 'MIXED'   ) THEN
 
               DEFCAL = MYVAL
 
            ELSE
 
               CALL SETMSG ( 'The input value for ''#'' is not a '
     .         //            'recognized calendar type.  The '
     .         //            'recognized calendars are '
     .         //            '''GREGORIAN'', ''JULIAN'', and '
     .         //            '''MIXED''. ' )
               CALL ERRCH  ( '#', VALUE    )
               CALL SIGERR ( 'SPICE(BADDEFAULTVALUE)' )
               CALL CHKOUT ( 'TIMDEF'                 )
               RETURN
 
            END IF
 
 
         ELSE
 
            CALL SETMSG ( 'The specified item ''#'' is not a '
     .      //            'recognized time default item.  The '
     .      //            'items that you may "SET" via the '
     .      //            'routine TIMDEF are ''CALENDAR'', '
     .      //            '''SYSTEM'', or ''ZONE'' ' )
 
 
            CALL ERRCH  ( '#', ITEM            )
            CALL SIGERR ( 'SPICE(BADTIMEITEM)' )
            CALL CHKOUT ( 'TIMDEF'             )
            RETURN
 
 
         END IF
 
         CALL CHKOUT ( 'TIMDEF' )
         RETURN
 
      ELSE IF ( MYACTN .EQ. 'GET' ) THEN
 
         IF      ( MYITEM .EQ. 'CALENDAR' ) THEN
            VALUE = DEFCAL
         ELSE IF ( MYITEM .EQ. 'SYSTEM'   ) THEN
            VALUE = DEFSYS
         ELSE IF ( MYITEM .EQ. 'ZONE'     ) THEN
            VALUE = DEFZON
         ELSE
 
            CALL SETMSG ( 'The specified item ''#'' is not a '
     .      //            'recognized time default item.  The '
     .      //            'items that you may "SET" via the '
     .      //            'routine TIMDEF are ''CALENDAR'', '
     .      //            '''SYSTEM'', or ''ZONE'' ' )
            CALL ERRCH  ( '#', ITEM )
            CALL SIGERR ( 'SPICE(BADTIMEITEM)' )
            CALL CHKOUT ( 'TIMDEF' )
            RETURN
 
         END IF
 
      ELSE
 
         CALL SETMSG ( 'The action speficied to TIMDEF was '
     .   //            '''#''.  This is not a recognized action. '
     .   //            'The recognized actions are ''SET'' and '
     .   //            '''GET''. ' )
 
         CALL ERRCH  ( '#', ACTION )
         CALL SIGERR ( 'SPICE(BADACTION)' )
         CALL CHKOUT ( 'TIMDEF' )
         RETURN
 
 
      END IF
 
      CALL CHKOUT ( 'TIMDEF' )
      RETURN
      END
