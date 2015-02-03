/*

-Procedure  loctim_ ( Get the local time. )

-Abstract

   Get the current local time, if possible, and return the result
   as an array of six integers.

-Disclaimer

   THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
   CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
   GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
   ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
   PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
   TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
   WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
   PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
   SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
   SOFTWARE AND RELATED MATERIALS, HOWEVER USED.

   IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
   BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
   LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
   INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
   REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
   REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.

   RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
   THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
   CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
   ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.

-Required_Reading

   None.

*/

/*
   ANSI Include Files
*/

#include <time.h>

/*
   Local Include Files

*/

#include "naifdefs.h"

/*
   Function declaration
*/

void
loctim_ ( 
          naifInt *ymdhms,
          naifInt *status
        )

/*

-Keywords

   None.

-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------
    ymdhms    O   Array representing the current time.
    status    O   A status indicator: success = 0, failure = -1.

-Detailed_Input

   None.

-Detailed_Output

   ymdhms   This is a pointer to an array of six integers which
            is used to store the current time. The array elements
            store the date and time in the following order.

               ymdhms[0]  --- Current calendar year.
               ymdhms[1]  --- Current month.
               ymdhms[2]  --- Current day of month.
               ymdhms[3]  --- Current hour. Hours have a range from
                              0 to 23. 0 corresponds to system
                              midnight.
               ymdhms[4]  --- Current minutes.
               ymdhms[5]  --- Current seconds.

   status   This is an integer status indicator. It will have a
            value of zero (0) upon success and a value of negative
            one (-1) on failure.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This function exists for the SGI, thought it could be used
   on other UNIX/ANSI C platforms, to avoid a name conflict with
   the SPICELIB subroutine 'ltime'. This routine gets the current
   local time: year, month, day, hours, minutes, seconds.

-Examples

   See the Fortran subroutine CPUTIM.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   K.R. Gehringer     (JPL)
   B.V. Semenov       (JPL)

-Version

   -Beta Version 1.0.1, 18-FEB-2008 (BVS)

      Fixed a few formatting problems in the header to make 
      it fully compliant.

   -Beta Version 1.0.0, 10-APR-1996 (KRG)

-Index_Entries

   get the current cpu time

-&

*/

/* Begin loctim_ */
{

/*
   Local Variables
*/

   struct tm *tm_now;
   time_t    now;

/*
   Get the current time.
*/

   now = time(NULL);

/*
   time() returns a -1 if it fails.
*/

   if ( now == -1 )
      {
      *(status) = -1;
      return;
      } /* end if */

/*
   Convert the time into a time structure that has components
   that we can understand.
*/

   tm_now = localtime ( &now ); 

/*
   localtime() returns NULL if it fails, but we want to return
   a negative one (-1).
*/

   if ( tm_now == (struct tm *) NULL )
      {
      *(status) = -1;
      return;
      } /* end if */

   *(ymdhms)   = tm_now -> tm_year + 1900;
   *(ymdhms+1) = tm_now -> tm_mon  + 1;
   *(ymdhms+2) = tm_now -> tm_mday;
   *(ymdhms+3) = tm_now -> tm_hour;
   *(ymdhms+4) = tm_now -> tm_min;
   *(ymdhms+5) = tm_now -> tm_sec;

   *(status) = 0;
   return;


} /* End loctim_ */
