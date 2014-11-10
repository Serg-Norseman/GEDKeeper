/* CPGDate.cs
 * 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * History:  
 * 10Dec08 AlexC          Migrated from GEDmill 1.10
 *
 */

using System;

namespace GEDmill
{
    // Represents a date for presentation on a web page. This includes date ranges, uncertain dates, and various different calendars.
    public class CPGDate : IComparable 
    {
        // TODO: write ToDays() function and use it when comparing dates.

        public enum EType 
        {
            Single, 
            PeriodFrom, 
            PeriodTo, 
            PeriodFromTo, 
            RangeBefore, 
            RangeAfter, 
            RangeBetween, 
            ApproximatedAbout, 
            ApproximatedCalculated, 
            ApproximatedEstimate, 
            Interpreted,
            Phrase
        }

        public enum ECalendar 
        {
            Gregorian, 
            Julian, 
            Hebrew, 
            French, 
            Roman,
            Unknown
        }

        public EType m_etType; 
        public ECalendar m_ecCalendar;
        public CYear m_year;
        public uint m_uMonth;
        public uint m_uDay;
        public CYear m_yearTo;
        public uint m_uMonthTo;
        public uint m_uDayTo;
        public string m_sPhrase;

        // Default constructor
        public CPGDate()
        {
        }

        // Copy constructor
        public CPGDate( CPGDate date )
        {
            m_etType = date.m_etType;
            m_ecCalendar = date.m_ecCalendar;
            if( date.m_year != null )
                m_year = new CYear( date.m_year );
            else
                m_year = null;
            m_uMonth = date.m_uMonth;
            m_uDay = date.m_uDay;
            if( date.m_yearTo != null )
                m_yearTo = new CYear( date.m_yearTo );
            else
                m_yearTo = null;

            m_uMonthTo = date.m_uMonthTo;
            m_uDayTo = date.m_uDayTo;
            m_sPhrase = date.m_sPhrase;
        }

        public CPGDate( DateTime dt )
        {
            m_etType = EType.Single;
            m_ecCalendar = ECalendar.Gregorian;
            m_year = new CYear( dt.Year );
            m_uMonth = (uint)dt.Month;
            m_uDay = (uint)dt.Day;
            m_yearTo = null;
            m_uMonthTo = 0;
            m_uDayTo = 0;
            m_sPhrase = "";
        }

        // Parse a GEDCOM date string and create an appropriate CPGDate object.
        // Returns null if parsing failed.
        // Tries to parse the various representations in turn until one parses ok.
        public static CPGDate Parse( string sDate )
        {
            if (sDate == null)
            {
                return null;
            }

            string dateString = sDate.ToUpper();
            CGedcom.ParseWhitespace( ref dateString );

            if (dateString == null || dateString.Length == 0)
            {
                return null;
            }

            CPGDate date = null;

            date = ParseDatePeriod( ref dateString );
            if( date != null )
            {
                if( dateString != null && dateString.Length > 0 )
                {
                    LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Warning, "Extra characters after date : " + sDate );
                }
                else
                {
                    return date;
                }
            }

            date = ParseDateRange( ref dateString );
            if( date != null )
            {
                if( dateString != null && dateString.Length > 0 )
                {
                    LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Warning, "Extra characters after date : " + sDate );
                }
                else
                {
                    return date;
                }
            }

            date = ParseDateApproximated( ref dateString );
            if( date != null )
            {
                if( dateString != null && dateString.Length > 0 )
                {
                    LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Warning, "Extra characters after date : " + sDate );
                }
                else
                {
                    return date;
                }
            }

            date = ParseDateInterpreted( ref dateString );
            if( date != null )
            {
                if( dateString != null && dateString.Length > 0 )
                {
                    LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Warning, "Extra characters after date : " + sDate );
                }
                else
                {
                    return date;
                }
            }

            date = ParseDate( ref dateString );
            if( date != null )
            {
                if( dateString != null && dateString.Length > 0 )
                {
                    LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Warning, "Extra characters after date : \"" + sDate + "\"" );
                    dateString = sDate; // Reset to start ready to parse as phrase
                }
                else
                {
                    return date;
                }
            }

            date = ParseDatePhrase( ref dateString );

            return date;

        }

    // Returns a string representing the year with brackets or quotes if not a precise year, or "" if no year.
    public string QuotedYearString()
    {
      string year = YearString();
      switch( m_etType )
      {
        case CPGDate.EType.PeriodFromTo:
        case CPGDate.EType.RangeBetween:
          return String.Concat( "(", year, ")" );

        case CPGDate.EType.Phrase:
          return String.Concat( "\"", year, "\"" );

        case CPGDate.EType.PeriodFrom:
        case CPGDate.EType.PeriodTo:
        case CPGDate.EType.RangeBefore:
        case CPGDate.EType.RangeAfter:
        case CPGDate.EType.ApproximatedAbout:
        case CPGDate.EType.ApproximatedCalculated:
        case CPGDate.EType.ApproximatedEstimate:
        case CPGDate.EType.Interpreted:
        case CPGDate.EType.Single:  
        default:
          return year;
      }
    }

    // Returns a string representing the year, or "" if no year.
    public string YearString()
    {
      string fromYear = "";
      if (m_year != null)
      {
          fromYear = m_year.ToString();
      }
      string toYear = "";
      if (m_yearTo != null)
      {
          toYear = m_yearTo.ToString();
      }

      switch( m_etType )
      {
        case EType.PeriodFrom:
          return "from " + fromYear;
        case EType.RangeAfter:
          return "aft " + fromYear;
        case EType.PeriodTo:
          return "to " + fromYear;
        case EType.RangeBefore:
          return "bef " + fromYear;
        case EType.PeriodFromTo:
          if( fromYear != toYear )
            return fromYear + "-" + toYear;
          return fromYear;
        case EType.RangeBetween:
          if( fromYear != toYear )
          {
              if (m_year.m_nYear > 0 && !m_year.m_bAltValid && m_yearTo.m_nYear > 0 && !m_yearTo.m_bAltValid)
              {
                  // Converts 1851,1864 to 1851-64.
                  return Strokeise(m_year.m_nYear, m_yearTo.m_nYear, "-"); 
              }
            return fromYear + "-" + toYear;
          }
          else
            return fromYear;
        case EType.Single:
          return fromYear;
        case EType.ApproximatedAbout:
          return "abt " + fromYear;
        case EType.ApproximatedCalculated:
          return "c " + fromYear;
        case EType.ApproximatedEstimate:
          return fromYear + "?";
        case EType.Interpreted:
          return "int " + fromYear;
        case EType.Phrase:
          return m_sPhrase;
        default:
          return "";
      }
    }

    // Converts consecutive years e.g. 1888,1889 to 1888-9 and 1781,1792 to 1781-92 but not 1781,1801 to 1781-801
    public static string Strokeise( int y1, int y2, string sDelimiter )
    {
      string sResult = "";

      string sYear1 = y1.ToString();
      string sYear2 = y2.ToString();
      int i = 0;
      if( sYear1.Length != sYear2.Length || (sYear1.Length>=4 && sYear2.Length>=4 && sYear1[1] != sYear2[1]) )
      {
        // Hack to prevent 1781,1801 being converted to 1781-801
        // Also prevents years of unequal length being converted, e.g. 12,1066 to 12-066
        return String.Concat( sYear1, sDelimiter, sYear2 );
      }
      while( i < sYear1.Length && i < sYear2.Length && (sYear1[i] == sYear2[i]) )
      {
        sResult += sYear1[i];
        i++;
      }
      sResult = String.Concat( sResult, sYear1.Substring( i ), sDelimiter, sYear2.Substring( i ) );

      return sResult;
    }

    // Accessor for the year part of the date.
    // Returns empty string if year not valid.
    public string Year
    {
      get
      {
          if (m_year == null)
          {
              return "";
          }

        return m_year.ToString();
      }
    }

    // Compare this CPGDate to another CPGDate to see which came first.
    public int CompareTo(object obj) 
    {
      if(obj is CPGDate) 
      {
        CPGDate date = (CPGDate) obj;

        bool bYearThis = true;
        bool bYearThat = true;
        if( m_year == null )
        {
          bYearThis = false;
        }
        if( date.m_year == null )
        {
          bYearThat = false;
        }

        if( bYearThis && bYearThat )
        {
            if (m_year.m_nYear < date.m_year.m_nYear)
            {
                return -1;
            }
            if (m_year.m_nYear > date.m_year.m_nYear)
            {
                return 1;
            }

            if (m_uMonth < date.m_uMonth)
            {
                return -1;
            }
            if (m_uMonth > date.m_uMonth)
            {
                return 1;
            }

            if (m_uDay < date.m_uDay)
            {
                return -1;
            }
            if (m_uDay < date.m_uDay)
            {
                return 1;
            }
        }
        else if( bYearThat ) 
        {
          // This goes after valid years
          return 1; 
        }
        else if( bYearThis ) 
        {
          // This goes before invalid years
          return -1; 
        }
            
        return 0;

        // TODO: comparison of julian with gregorian dates.
            
      }
        
      throw new ArgumentException("object is not a CPGDate(249)");    
    }

    // Return this date as a displayable string, including specifier (i.e. "approx", "from/to" etc.).
    public override string ToString()
    {
      string sFromDate = DateString( m_uDay, m_uMonth, m_year, m_ecCalendar );
      string sToDate = DateString( m_uDayTo, m_uMonthTo, m_yearTo, m_ecCalendar );

      string sDate = "";
      switch( m_etType )
      {
        case EType.ApproximatedAbout:
          sDate = "about " + sFromDate;
          break;
        case EType.ApproximatedCalculated:
          sDate = "approx. " + sFromDate;
          break;
        case EType.ApproximatedEstimate:
          sDate = sFromDate + " (est.)";
          break;
        case EType.Interpreted:
          sDate = "approx. " + sFromDate;
          break;
        case EType.PeriodFrom:
          sDate = "from " + sFromDate;
          break;
        case EType.PeriodFromTo:
          sDate = "from " + sFromDate + " to " + sToDate;
          break;
        case EType.PeriodTo:
          sDate = "to " + sFromDate;
          break;
        case EType.Phrase:
          sDate = m_sPhrase;
          break;
        case EType.RangeAfter:
          sDate = "after " + sFromDate;
          break;
        case EType.RangeBefore:
          sDate = "before " + sFromDate;
          break;
        case EType.RangeBetween:
          sDate = "between " + sFromDate + " and " + sToDate;
          break;
        case EType.Single:
        default:
          sDate = sFromDate;
          break;
      }

      return sDate;
    }

    // Returns duration of the date in days, for periods and ranges. Returns 1 for single dates etc.
    // TODO: only returns approximate duration, doesn't account for leap years, days in month, different calendars, alternate years etc.
    public uint GetDuration( CPGDate lowerLimit, CPGDate upperLimit )
    {
      AssertLower( ref lowerLimit );
      AssertHigher( ref upperLimit );

      switch( m_etType )
      {
        case EType.PeriodFrom:
        case EType.RangeAfter:
          return (uint)Math.Abs(CountDays( m_uDay, m_uMonth, m_year.m_nYear, upperLimit.m_uDay, upperLimit.m_uMonth, upperLimit.m_year.m_nYear ));
                    
        case EType.PeriodTo:
        case EType.RangeBefore:
          return (uint)Math.Abs(CountDays( lowerLimit.m_uDay, lowerLimit.m_uMonth, lowerLimit.m_year.m_nYear, m_uDay, m_uMonth, m_year.m_nYear ));

        case EType.PeriodFromTo:
        case EType.RangeBetween:
          return (uint)Math.Abs(CountDays( m_uDay, m_uMonth, m_year.m_nYear, m_uDayTo, m_uMonthTo, m_yearTo.m_nYear ));

        case EType.Single:
        case EType.ApproximatedAbout:
        case EType.ApproximatedCalculated:
        case EType.ApproximatedEstimate:
        case EType.Interpreted:
        case EType.Phrase:
        default:
          return 1;
      }
    }

    // Returns true if this instance contains valid date information.
    public static bool IsValid( CPGDate date )
    {
      return( date != null && date.m_year != null );
    }

    // Returns the number of days difference between the two given dates, or 0 if the date ranges overlap.
    public static int Difference( CPGDate date1, CPGDate date2 )
    {
      AssertLower( ref date1 );
      AssertHigher( ref date2 );
      // If date2 encompasses date1, difference = 0
      if (date2.Encompasses(date1))
      {
          return 0;
      }
      // If date1 encompasses date2, difference = 0
      if (date1.Encompasses(date2))
      {
          return 0;
      }

      return CountDays( date1.m_uDay, date1.m_uMonth, date1.m_year.m_nYear, date2.m_uDay, date2.m_uMonth, date2.m_year.m_nYear );
    }

        // Parse a basic date string (not including specifiers, but including calendar sType) and create an appropriate CPGDate object.
        // Returns null if parsing failed.
        private static CPGDate ParseDate( ref string sDate )
        {
            string sTemp = sDate;
            CGedcom.ParseWhitespace( ref sTemp );
            if (sTemp == null)
            {
                return null;
            }

            CPGDate date = null;

            ECalendar calendar = ParseDateCalendarEscape( ref sTemp );
            switch( calendar )
            {
                case ECalendar.Gregorian:
                    date = ParseDateGreg( ref sTemp );
                    break;
                case ECalendar.Julian:
                    date = ParseDateJuln( ref sTemp );
                    break;
                case ECalendar.Hebrew:
                    date = ParseDateHebr( ref sTemp );
                    break;
                case ECalendar.French:
                    date = ParseDateFren( ref sTemp );
                    break;
                case ECalendar.Roman:
                case ECalendar.Unknown:
                default:
                    break;
            }

            if( date != null )
            {
                sDate = sTemp;
                return date;
            }

            return ParseDatePhrase( ref sDate );

        }
        
        // Parse a date period string and create an appropriate CPGDate object.
        // Returns null if parsing failed.
        private static CPGDate ParseDatePeriod( ref string sDate )
        {
            string sTemp = sDate;
            CPGDate dateTo = null;
            CPGDate dateFrom = ParsePrefixedDate( ref sTemp, "FROM" );
            CGedcom.ParseWhitespace( ref sTemp );
            if (sTemp != null)
            {
                dateTo = ParsePrefixedDate(ref sTemp, "TO");
            }

            if( dateFrom != null && dateTo != null )
            {
                dateFrom.m_yearTo = dateTo.m_year;
                dateFrom.m_uMonthTo = dateTo.m_uMonth;
                dateFrom.m_uDayTo = dateTo.m_uDay;
                dateFrom.m_etType = EType.PeriodFromTo;
                sDate = sTemp;
            }
            else if( dateTo != null )
            {
                // Keep toDate.m_year toDate.m_month toDate.m_day intact.
                dateTo.m_yearTo = null;
                dateTo.m_uMonthTo = 0;
                dateTo.m_uDayTo = 0;
                dateTo.m_etType = EType.PeriodTo;
                dateFrom = dateTo;
                sDate = sTemp;
            }
            else if( dateFrom != null )
            {
                dateFrom.m_etType = EType.PeriodFrom;
                sDate = sTemp;
            }

            return dateFrom;
        }
        
        // Attempts to parse a date string with the given specifier, and create an appropriate CPGDate object.
        // Returns null if parsing failed.
        private static CPGDate ParsePrefixedDate( ref string sDate, string sPrefix )
        {
            if (sPrefix.Length >= sDate.Length || sDate.Substring(0, sPrefix.Length) != sPrefix)
            {
                return null;
            }

            string sTemp = sDate.Substring( sPrefix.Length );
            CGedcom.ParseWhitespace( ref sTemp );
            if (sTemp == null)
            {
                return null;
            }

            CPGDate date = ParseDate( ref sTemp );
            if( date != null )
            {
                sDate = sTemp;
            }
            return date;
        }

        // Parse a GEDCOM date range string and create an appropriate CPGDate object.
        // Returns null if parsing failed.
        private static CPGDate ParseDateRange( ref string sDate )
        {
            string sTemp = sDate;
            CPGDate date;

            date = ParsePrefixedDate( ref sTemp, "BEF" );
            if( date != null )
            {
                date.m_etType = EType.RangeBefore;
                sDate = sTemp;
                return date;
            }
            
            date = ParsePrefixedDate( ref sTemp, "AFT" );
            if( date != null )
            {
                date.m_etType = EType.RangeAfter;
                sDate = sTemp;
                return date;
            }

            CPGDate dateFrom = ParsePrefixedDate( ref sTemp, "BET" );
            CGedcom.ParseWhitespace( ref sTemp );
            if (sTemp == null)
            {
                return null;
            }

            CPGDate dateTo = ParsePrefixedDate( ref sTemp, "AND" );

            if( dateFrom != null && dateTo != null )
            {
                dateFrom.m_yearTo = dateTo.m_year;
                dateFrom.m_uMonthTo = dateTo.m_uMonth;
                dateFrom.m_uDayTo = dateTo.m_uDay;
                dateFrom.m_etType = EType.RangeBetween;
                sDate = sTemp;
                return dateFrom;
            }

            return null;
        }

        // Parse a GEDCOM approximate date string and create an appropriate CPGDate object.
        // Returns null if parsing failed.
        private static CPGDate ParseDateApproximated( ref string sDate )
        {
            string sTemp = sDate;
            CPGDate date;

            date = ParsePrefixedDate( ref sTemp, "ABT." );
            if( date != null )
            {
                date.m_etType = EType.ApproximatedAbout;
                sDate = sTemp;
                return date;
            }

            date = ParsePrefixedDate( ref sTemp, "ABT" );
            if( date != null )
            {
                date.m_etType = EType.ApproximatedAbout;
                sDate = sTemp;
                return date;
            }

            date = ParsePrefixedDate( ref sTemp, "CAL" );
            if( date != null )
            {
                date.m_etType = EType.ApproximatedCalculated;
                sDate = sTemp;
                return date;
            }

            date = ParsePrefixedDate( ref sTemp, "EST" );
            if( date != null )
            {
                date.m_etType = EType.ApproximatedEstimate;
                sDate = sTemp;
                return date;
            }
            return null;
        }

        // Parse a GEDCOM interpreted date string and create an appropriate CPGDate object.
        // Returns null if parsing failed.
        private static CPGDate ParseDateInterpreted( ref string sDate )
        {
            string sTemp = sDate;
            CPGDate date;

            date = ParsePrefixedDate( ref sTemp, "INT" );
            if( date != null )
            {
                date.m_etType = EType.Interpreted;
                CGedcom.ParseWhitespace( ref sTemp );
                date.m_sPhrase = sTemp;
                sDate = "";
            }
            return date;
        }

        // Parse a GEDCOM date phrase string and create an appropriate CPGDate object.
        // Returns null if parsing failed.
        private static CPGDate ParseDatePhrase( ref string sDate )
        {
            string sTemp = sDate;
            CPGDate date = null;

            CGedcom.ParseWhitespace( ref sTemp );
            if( sTemp != null && sTemp != "" )
            {
                date = new CPGDate();
                date.m_sPhrase = sTemp;
                date.m_etType = EType.Phrase;
                sDate = "";
            }
            return date;
        }

        // Determines which calendar to use for the given date string.
        private static ECalendar ParseDateCalendarEscape( ref string sDate )
        {
            if( sDate.Length>13 && sDate.Substring( 0, 13 ) == "@#DGREGORIAN@" )
            {
                sDate = sDate.Substring(13);
                return ECalendar.Gregorian;
            }
            if( sDate.Length>10 && sDate.Substring( 0, 10 ) == "@#DJULIAN@" )
            {
                sDate = sDate.Substring(10);
                return ECalendar.Julian;
            }
            if( sDate.Length>10 && sDate.Substring( 0, 10 ) == "@#DHEBREW@" )
            {
                sDate = sDate.Substring(10);
                return ECalendar.Hebrew;
            }
            if( sDate.Length>11 && sDate.Substring( 0, 11 ) == "@#DFRENCHR@" )
            {
                sDate = sDate.Substring(11);
                return ECalendar.French;
            }
            if( sDate.Length>9 && sDate.Substring( 0,9 ) == "@#DROMAN@" )
            {
                sDate = sDate.Substring(9);
                return ECalendar.Roman;
            }
            if( sDate.Length>11 && sDate.Substring( 0, 11 ) == "@#DUNKNOWN@" )
            {
                sDate = sDate.Substring(11);
                return ECalendar.Unknown;
            }
            return ECalendar.Gregorian;
        }

        // Parse a GEDCOM Gregorian date string and create an appropriate CPGDate object.
        // Returns null if parsing failed.
        // First tries day month year, then month year, then year.
        private static CPGDate ParseDateGreg( ref string sDate )
        {
            string sTemp = sDate;
            uint uDay=0, uMonth=0;
            CYear year=null;

            uDay = ParseDay( ref sTemp );
            if( uDay > 0 )
            {
                uMonth = ParseMonth( ref sTemp );
                if( uMonth > 0 )
                {
                    year = ParseYearGreg( ref sTemp );
                    if( year != null )
                    {
                        CPGDate date = new CPGDate();
                        date.m_uDay = uDay;
                        date.m_uMonth = uMonth;
                        date.m_year = year;
                        date.m_etType = EType.Single;
                        date.m_ecCalendar = ECalendar.Gregorian;
                        sDate = sTemp;
                        return date;
                    }
                }
            }

            sTemp = sDate;
            uDay = 0;
            uMonth = ParseMonth( ref sTemp );
            if( uMonth > 0 )
            {
                year = ParseYearGreg( ref sTemp );
                if( year != null )
                {
                    CPGDate date = new CPGDate();
                    date.m_uDay = uDay;
                    date.m_uMonth = uMonth;
                    date.m_year = year;
                    date.m_etType = EType.Single;
                    date.m_ecCalendar = ECalendar.Gregorian;
                    sDate = sTemp;
                    return date;
                }
            }


            sTemp = sDate;
            uDay = 0;
            uMonth = 0;
            year = ParseYearGreg( ref sTemp );
            if( year != null )
            {
                CPGDate date = new CPGDate();
                date.m_uDay = uDay;
                date.m_uMonth = uMonth;
                date.m_year = year;
                date.m_etType = EType.Single;
                date.m_ecCalendar = ECalendar.Gregorian;
                sDate = sTemp;
                return date;
            }

            return null;
        }
    
        // Parse a GEDCOM Julian date string and create an appropriate CPGDate object.
        // Returns null if parsing failed.
        // First tries day month year, then month year, then year.
        private static CPGDate ParseDateJuln( ref string sDate )
        {
            string sTemp = sDate;
            uint uDay=0, uMonth=0;
            CYear year = null;

            uDay = ParseDay( ref sTemp );
            if( uDay > 0 )
            {
                uMonth = ParseMonth( ref sTemp );
                if( uMonth > 0 )
                {
                    year = ParseYear( ref sTemp );
                    if( year != null )
                    {
                        CPGDate date = new CPGDate();
                        date.m_uDay = uDay;
                        date.m_uMonth = uMonth;
                        date.m_year = year;
                        date.m_etType = EType.Single;
                        date.m_ecCalendar = ECalendar.Julian;
                        sDate = sTemp;
                        return date;
                    }
                }
            }

            sTemp = sDate;
            uDay = 0;
            uMonth = ParseMonth( ref sTemp );
            if( uMonth > 0 )
            {
                year = ParseYear( ref sTemp );
                if( year != null )
                {
                    CPGDate date = new CPGDate();
                    date.m_uDay = uDay;
                    date.m_uMonth = uMonth;
                    date.m_year = year;
                    date.m_etType = EType.Single;
                    date.m_ecCalendar = ECalendar.Julian;
                    sDate = sTemp;
                    return date;
                }
            }


            sTemp = sDate;
            uDay = 0;
            uMonth = 0;
            year = ParseYear( ref sTemp );
            if( year != null )
            {
                CPGDate date = new CPGDate();
                date.m_uDay = uDay;
                date.m_uMonth = uMonth;
                date.m_year = year;
                date.m_etType = EType.Single;
                date.m_ecCalendar = ECalendar.Julian;
                sDate = sTemp;
                return date;
            }

            return null;
        }

        // Parse a GEDCOM Hebrew date string and create an appropriate CPGDate object.
        // Returns null if parsing failed.
        // First tries day month year, then month year, then year.
        private static CPGDate ParseDateHebr( ref string sDate )
        {
            string sTemp = sDate;
            uint uDay=0, uMonth=0;
            CYear year=null;

            uDay = ParseDay( ref sTemp );
            if( uDay > 0 )
            {
                uMonth = ParseMonthHebr( ref sTemp );
                if( uMonth > 0 )
                {
                    year = ParseYear( ref sTemp );
                    if( year != null )
                    {
                        CPGDate date = new CPGDate();
                        date.m_uDay = uDay;
                        date.m_uMonth = uMonth;
                        date.m_year = year;
                        date.m_etType = EType.Single;
                        date.m_ecCalendar = ECalendar.Hebrew;
                        sDate = sTemp;
                        return date;
                    }
                }
            }

            sTemp = sDate;
            uDay = 0;
            uMonth = ParseMonthHebr( ref sTemp );
            if( uMonth > 0 )
            {
                year = ParseYear( ref sTemp );
                if( year != null )
                {
                    CPGDate date = new CPGDate();
                    date.m_uDay = uDay;
                    date.m_uMonth = uMonth;
                    date.m_year = year;
                    date.m_etType = EType.Single;
                    date.m_ecCalendar = ECalendar.Hebrew;
                    sDate = sTemp;
                    return date;
                }
            }


            sTemp = sDate;
            uDay = 0;
            uMonth = 0;
            year = ParseYear( ref sTemp );
            if( year != null )
            {
                CPGDate date = new CPGDate();
                date.m_uDay = uDay;
                date.m_uMonth = uMonth;
                date.m_year = year;
                date.m_etType = EType.Single;
                date.m_ecCalendar = ECalendar.Hebrew;
                sDate = sTemp;
                return date;
            }

            return null;
        }

        // Parse a GEDCOM French date string and create an appropriate CPGDate object.
        // Returns null if parsing failed.
        // First tries day month year, then month year, then year.
        private static CPGDate ParseDateFren( ref string sDate )
        {
            string sTemp = sDate;
            uint uDay=0, uMonth=0;
            CYear year=null;

            uDay = ParseDay( ref sTemp );
            if( uDay > 0 )
            {
                uMonth = ParseMonthFren( ref sTemp );
                if( uMonth > 0 )
                {
                    year = ParseYear( ref sTemp );
                    if( year != null )
                    {
                        CPGDate date = new CPGDate();
                        date.m_uDay = uDay;
                        date.m_uMonth = uMonth;
                        date.m_year = year;
                        date.m_etType = EType.Single;
                        date.m_ecCalendar = ECalendar.French;
                        sDate = sTemp;
                        return date;
                    }
                }
            }

            sTemp = sDate;
            uDay = 0;
            uMonth = ParseMonthFren( ref sTemp );
            if( uMonth > 0 )
            {
                year = ParseYear( ref sTemp );
                if( year != null )
                {
                    CPGDate date = new CPGDate();
                    date.m_uDay = uDay;
                    date.m_uMonth = uMonth;
                    date.m_year = year;
                    date.m_etType = EType.Single;
                    date.m_ecCalendar = ECalendar.French;
                    sDate = sTemp;
                    return date;
                }
            }


            sTemp = sDate;
            uDay = 0;
            uMonth = 0;
            year = ParseYear( ref sTemp );
            if( year != null )
            {
                CPGDate date = new CPGDate();
                date.m_uDay = uDay;
                date.m_uMonth = uMonth;
                date.m_year = year;
                date.m_etType = EType.Single;
                date.m_ecCalendar = ECalendar.French;
                sDate = sTemp;
                return date;
            }

            return null;
        }

        // Parse the day part of a GEDCOM date string.
        // Returns null if parsing failed.
        private static uint ParseDay( ref string sDate )
        {
            if (sDate == null)
            {
                return 0;
            }

            uint uDay = 0;
            string sTemp = sDate;
            CGedcom.ParseWhitespace( ref sTemp );
            if (sTemp == null)
            {
                return 0;
            }

            int i = 0;
            char c;
            while( i<sTemp.Length && char.IsDigit(c = sTemp[i]) )
            {
                uDay *= 10;
                uDay += (uint)(c - '0');
                i++;
            }

            if (uDay != 0)
            {
                sDate = sTemp.Substring(i);
            }

            return uDay;
        }

        // Parse the month part of a GEDCOM date string.
        // Returns null if parsing failed.
        private static uint ParseMonth( ref string sDate )
        {
            if (sDate == null)
            {
                return 0;
            }

            uint uMonth = 0;
            string sTemp = sDate;
            CGedcom.ParseWhitespace( ref sTemp );
            if (sTemp == null)
            {
                return 0;
            }

            string sMonth = "";
            if (sTemp.Length >= 3)
            {
                sMonth = sTemp.Substring(0, 3);
            }
            switch( sMonth )
            {
                case "JAN":
                    uMonth = 1;
                    break;
                case "FEB":
                    uMonth = 2;
                    break;
                case "MAR":
                    uMonth = 3;
                    break;
                case "APR":
                    uMonth = 4;
                    break;
                case "MAY":
                    uMonth = 5;
                    break;
                case "JUN":
                    uMonth = 6;
                    break;
                case "JUL":
                    uMonth = 7;
                    break;
                case "AUG":
                    uMonth = 8;
                    break;
                case "SEP":
                    uMonth = 9;
                    break;
                case "OCT":
                    uMonth = 10;
                    break;
                case "NOV":
                    uMonth = 11;
                    break;
                case "DEC":
                    uMonth = 12;
                    break;
                default:
                    uMonth = 0;
                    break;
            }

            if( uMonth != 0 )
            {
                int nSpace = sTemp.IndexOf( ' ' );
                if( nSpace >= 0 )
                {
                    sDate = sTemp.Substring( nSpace );
                }
                else
                {
                    sDate = sTemp.Substring( 3 );
                }
            }

            return uMonth;
        }

        // Parse the year part of a GEDCOM date string.
        // Returns null if parsing failed.
        private static CYear ParseYear( ref string sDate )
        {
            if (sDate == null)
            {
                return null;
            }

            int nYear = 0;
            string sTemp = sDate;
            CGedcom.ParseWhitespace( ref sTemp );
            if (sTemp == null)
            {
                return null;
            }

            int i = 0;
            char c;
            while( i<sTemp.Length && char.IsDigit(c = sTemp[i]) )
            {
                nYear *= 10;
                nYear += (c - '0');
                i++;
            }

            if( nYear != 0 ) 
            {
                sTemp = sTemp.Substring( i );
                sDate = sTemp;
                CGedcom.ParseWhitespace( ref sTemp );
                if( sTemp != null && sTemp.Substring( 0, 4 ) == "B.C." )
                {
                    nYear = -nYear;
                    sDate = sTemp.Substring( 4 );
                }
                return new CYear( nYear );
            }

            return null;
        }

        // Parse the year part of a GEDCOM Gregorian date string.
        // Returns null if parsing failed.
        private static CYear ParseYearGreg( ref string sDate )
        {
            if (sDate == null)
            {
                return null;
            }

            int nYear = 0, nAltYear = 0;
            string tmpString = sDate;
            CGedcom.ParseWhitespace( ref tmpString );
            if (tmpString == null)
            {
                return null;
            }

            int i = 0;
            char c='\0';
            bool bAltValid = false;
            string sYear = "";
            string sAltyear = "";
            while( i<tmpString.Length && char.IsDigit(c = tmpString[i]) )
            {
                sYear += c;
                i++;
            }
            if( i<tmpString.Length && (c == '/' || c == '-') ) // In file I downloaded from somewhere called westrad.ged, they had used '-' in place of '/'
            {
                bAltValid = true;
                i++;
                while( i<tmpString.Length && char.IsDigit(c = tmpString[i]) )
                {
                    sAltyear += c;
                    i++;
                }

                // Cope with year format like "1600-1" :
                int nYearChars = sYear.Length;
                int nAltyearChars = sAltyear.Length;
                if( sAltyear != "" && nAltyearChars <= nYearChars )
                {
                    sAltyear = sYear.Substring( 0, nYearChars - nAltyearChars ) + sAltyear;
                }
            }
            // Convert both year strings to numeric
            int j;
            if( sYear == "" )
            {
                nYear = 0;
            }
            else
            {
                j = 0;
                while( j<sYear.Length && char.IsDigit(c = sYear[j]) )
                {
                    nYear *= 10;
                    nYear += (c - '0');
                    j++;
                }
            }
            if( sAltyear == "" )
            {
                nAltYear = 0;
            }
            else
            {
                j = 0;
                while( j<sAltyear.Length && char.IsDigit(c = sAltyear[j]) )
                {
                    nAltYear *= 10;
                    nAltYear += (c - '0');
                    j++;
                }
            }

            if( nYear != 0 ) 
            {
                tmpString = tmpString.Substring( i );
                sDate = tmpString;
                CGedcom.ParseWhitespace( ref tmpString );
                if( tmpString != null && tmpString.Length >= 4 && tmpString.Substring( 0, 4 ) == "B.C." )
                {
                    nYear = -nYear;
                    nAltYear = -nAltYear;
                    sDate = tmpString.Substring( 4 );
                }


                if (bAltValid)
                {
                    if (nYear > nAltYear)
                    {
                        int tmp = nYear;
                        nYear = nAltYear;
                        nAltYear = tmp;
                    }

                    return new CYear(nYear, nAltYear);
                }
                else
                {
                    return new CYear(nYear);
                }

            }

            return null;
        }

        // Parse the month part of a GEDCOM French date string.
        // Returns null if parsing failed.
        private static uint ParseMonthFren( ref string sDate )
        {
            if (sDate == null)
            {
                return 0;
            }

            uint uMonth = 0;
            string sTemp = sDate;
            CGedcom.ParseWhitespace( ref sTemp );
            if (sTemp == null)
            {
                return 0;
            }

            string sMonth = sTemp.Substring(0, 4);
            switch( sMonth )
            {
                case "VEND":
                    uMonth = 1;
                    break;
                case "BRUM":
                    uMonth = 2;
                    break;
                case "FRIM":
                    uMonth = 3;
                    break;
                case "NIVO":
                    uMonth = 4;
                    break;
                case "PLUV":
                    uMonth = 5;
                    break;
                case "VENT":
                    uMonth = 6;
                    break;
                case "GERM":
                    uMonth = 7;
                    break;
                case "FLOR":
                    uMonth = 8;
                    break;
                case "PRAI":
                    uMonth = 9;
                    break;
                case "MESS":
                    uMonth = 10;
                    break;
                case "THER":
                    uMonth = 11;
                    break;
                case "FRUC":
                    uMonth = 12;
                    break;
                case "COMP":
                    uMonth = 13;
                    break;
                default:
                    uMonth = 0;
                    break;
            }

            if( uMonth != 0 )
            {
                int nSpace = sTemp.IndexOf( ' ' );
                if( nSpace >= 0 )
                {
                    sDate = sTemp.Substring( nSpace );
                }
                else
                {
                    sDate = sTemp.Substring( 3 );
                }
            }

            return uMonth;
        }

        // Parse the month part of a GEDCOM Hebrew date string.
        // Returns null if parsing failed.
        private static uint ParseMonthHebr( ref string sDate )
        {
            if (sDate == null)
            {
                return 0;
            }

            uint uMonth = 0;
            string sTemp = sDate;
            CGedcom.ParseWhitespace( ref sTemp );
            if (sTemp == null)
            {
                return 0;
            }

            string sMonth = sTemp.Substring(0, 3);
            switch( sMonth )
            {
                case "TSH":
                    uMonth = 1;
                    break;
                case "CSH":
                    uMonth = 2;
                    break;
                case "KSL":
                    uMonth = 3;
                    break;
                case "TVT":
                    uMonth = 4;
                    break;
                case "SHV":
                    uMonth = 5;
                    break;
                case "ADR":
                    uMonth = 6;
                    break;
                case "ADS":
                    uMonth = 7;
                    break;
                case "NSN":
                    uMonth = 8;
                    break;
                case "IYR":
                    uMonth = 9;
                    break;
                case "SVN":
                    uMonth = 10;
                    break;
                case "TMZ":
                    uMonth = 11;
                    break;
                case "AAV":
                    uMonth = 12;
                    break;
                case "ELL":
                    uMonth = 13;
                    break;
                default:
                    uMonth = 0;
                    break;
            }

            if( uMonth != 0 )
            {
                int nSpace = sTemp.IndexOf( ' ' );
                if( nSpace >= 0 )
                {
                    sDate = sTemp.Substring( nSpace );
                }
                else
                {
                    sDate = sTemp.Substring( 3 );
                }
            }

            return uMonth;
        }

        // Return this date as a displayable string. Used by ToString().
        private string DateString( uint uDay, uint uMonth, CYear year, ECalendar calendar )
        {
            if (year == null)
            {
                return "";
            }

            if (calendar == ECalendar.French)
            {
                return DateStringFren(uDay, uMonth, year);
            }
            if (calendar == ECalendar.Hebrew)
            {
                return DateStringHebr(uDay, uMonth, year);
            }

            string sDate = "";

            string[] aMonths = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

            if( uDay != 0 )
            {
                sDate = uDay.ToString();
                uint ord = uDay % 10;
                uint ord10 = uDay % 100;
                if( ord == 1 && ord10 != 11 )
                    sDate += "st";
                else if( ord == 2 && ord10 != 12 )
                    sDate += "nd";
                else if( ord == 3 && ord10 != 13 )
                    sDate += "rd";
                else
                    sDate += "th";
                sDate += " ";
            }

            if( uMonth > 0 && uMonth <= aMonths.Length )
            {
                sDate += aMonths[uMonth-1];
                sDate += " ";
            }

            sDate += year.ToString();

            return sDate;
        }

        // Returns this date as a French string. Used by DateString().
        private string DateStringFren( uint uDay, uint uMonth, CYear year )
        {
            string sDate = "";

            string[] aMonths = { "Vend", "Brum", "Frim", "Nivo", "Pluv", "Vent", "Germ", "Flor", "Prai", "Mess", "Ther", "Fruc", "Comp" };

            if( uDay != 0 )
            {
                sDate = uDay.ToString();
                uint ord = uDay % 10;
                if (ord == 1)
                {
                    sDate += "er";
                }
                else
                {
                    sDate += "me";
                }
                sDate += " ";
            }

            if( uMonth > 0 && uMonth <= aMonths.Length )
            {
                sDate += aMonths[uMonth-1];
                sDate += " ";
            }

            sDate += year.ToString();

            return sDate;
        }

        
        // Returns this date as a Hebrew string. Used by DateString().
        private string DateStringHebr( uint uDay, uint uMonth, CYear year )
        {
            string sDate = "";

            string[] aMonths = { "Tsh", "Csh", "Ksl", "Tvt", "Shv", "Adr", "Ads", "Nsn", "Iyr", "Svn", "Tmz", "Aav", "Ell" };

            if( uDay != 0 )
            {
                sDate = uDay.ToString();
                sDate += " ";
            }

            if( uMonth > 0 && uMonth <= aMonths.Length )
            {
                sDate += aMonths[uMonth-1];
                sDate += " ";
            }

            sDate += year.ToString();

            return sDate;
        }

        // Used by comparison functions to ensure that a range has a valid lower bound, by setting the earliest date possible.
        private static void AssertLower( ref CPGDate date )
        {
            if( !IsValid( date ) )
            {
                date = new CPGDate();
                date.m_etType = CPGDate.EType.Single;
                date.m_uDay = 1;
                date.m_uMonth = 1;
                date.m_year = new CYear(1);
            }
        }

        // Used by comparison functions to ensure that a range has a valid upper bound, by setting the latest date possible (i.e. today).
        private static void AssertHigher( ref CPGDate date )
        {
            if( !IsValid( date ) )
            {
                DateTime dtNow = DateTime.Now;
                date = new CPGDate();
                date.m_etType = CPGDate.EType.Single;
                date.m_uDay = (uint)dtNow.Day;
                date.m_uMonth = (uint)dtNow.Month;
                date.m_year = new CYear(dtNow.Year);
            }
        }

        // Returns true if both this from and this to dates fall before specified date
        private bool IsBefore( uint uDay, uint uMonth, int nYear )
        {
            switch( m_etType )
            {
                case EType.PeriodFrom:
                case EType.RangeAfter:
                    return false;
                    
                case EType.PeriodTo:
                case EType.RangeBefore:
                    return ( m_year.m_nYear * 365 + m_uMonth * 31 + m_uDay 
                        <  nYear * 365 + uMonth * 31 + uDay );

                case EType.PeriodFromTo:
                case EType.RangeBetween:
                    return ( m_yearTo.m_nYear * 365 + m_uMonthTo * 31 + m_uDayTo 
                        <  nYear * 365 + uMonth * 31 + uDay ); // If end date is before, then start date must be

                case EType.Phrase:
                    return false;

                case EType.Single:
                case EType.ApproximatedAbout:
                case EType.ApproximatedCalculated:
                case EType.ApproximatedEstimate:
                case EType.Interpreted:
                default:
                    return ( m_year.m_nYear * 365 + m_uMonth * 31 + m_uDay 
                        <  nYear * 365 + uMonth * 31 + uDay );
            }
        }

        // Returns true if both from and to dates fall after specified date
        private bool IsAfter( uint uDay, uint uMonth, int nYear )
        {
            switch( m_etType )
            {
                case EType.PeriodFrom:
                case EType.RangeAfter:
                    return ( m_year.m_nYear * 365 + m_uMonth * 31 + m_uDay 
                        >  nYear * 365 + uMonth * 31 + uDay );

                case EType.PeriodTo:
                case EType.RangeBefore:
                    return false;
                    

                case EType.PeriodFromTo:
                case EType.RangeBetween:
                    return ( m_year.m_nYear * 365 + m_uMonth * 31 + m_uDay 
                        >  nYear * 365 + uMonth * 31 + uDay ); // If start date is after, then end date must be

                case EType.Phrase:
                    return false;

                case EType.Single:
                case EType.ApproximatedAbout:
                case EType.ApproximatedCalculated:
                case EType.ApproximatedEstimate:
                case EType.Interpreted:
                default:
                    return ( m_year.m_nYear * 365 + m_uMonth * 31 + m_uDay 
                        >  nYear * 365 + uMonth * 31 + uDay );
            }
        }

        // Returns true if the specified day, month and year fall inside the range of this date instance.
        private bool Encompasses( uint uDay, uint uMonth, int nYear )
        {
            switch( m_etType )
            {
                case EType.PeriodFrom:
                case EType.RangeAfter:
                    return IsBefore( uDay, uMonth, nYear );
                    
                case EType.PeriodTo:
                case EType.RangeBefore:
                    return IsAfter( uDay, uMonth, nYear );

                case EType.PeriodFromTo:
                case EType.RangeBetween:
                    return !IsBefore( uDay, uMonth, nYear )
                        && !IsAfter( uDay, uMonth, nYear );


                case EType.Single:
                case EType.ApproximatedAbout:
                case EType.ApproximatedCalculated:
                case EType.ApproximatedEstimate:
                case EType.Interpreted:
                case EType.Phrase:
                default:
                    return ( m_uDay == uDay && m_uMonth == uMonth && m_year.m_nYear == nYear );
            }
        }

        // Returns true if the specified date falls inside the range of this date instance.
        private bool Encompasses( CPGDate date )
        {
            if (date == null)
            {
                return false;
            }
            
            switch( m_etType )
            {
                case EType.PeriodFrom:
                case EType.RangeAfter:
                    return date.IsAfter( m_uDay, m_uMonth, m_year.m_nYear );
                    
                case EType.PeriodTo:
                case EType.RangeBefore:
                    return date.IsBefore( m_uDay, m_uMonth, m_year.m_nYear );

                case EType.PeriodFromTo:
                case EType.RangeBetween:
                    return date.IsBefore( m_uDayTo, m_uMonthTo, m_yearTo.m_nYear )
                        && date.IsAfter( m_uDay, m_uMonth, m_year.m_nYear );


                case EType.Single:
                case EType.ApproximatedAbout:
                case EType.ApproximatedCalculated:
                case EType.ApproximatedEstimate:
                case EType.Interpreted:
                case EType.Phrase:
                default:
                    return date.Encompasses( m_uDay, m_uMonth, m_year.m_nYear );
            }
        }

        // Returns the approximate number of days between the two dates given.
        // It doesn't account for leap years and so on.
        private static int CountDays( uint uStartDay, uint uStartMonth, int nStartYear, uint uEndDay, uint uEndMonth, int nEndYear )
        {
            int nStartDays = (int)nStartYear * 365 + (int)uStartMonth * 31 + (int)uStartDay;
            int nEndDays = (int)nEndYear * 365 + (int)uEndMonth * 31 + (int)uEndDay;

            return nEndDays - nStartDays;
        }

        
    }
}
