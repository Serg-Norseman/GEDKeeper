/* CIEvent.cs
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

namespace GEDmill.HTMLClasses
{
    // Encapsulates an event in an individual's life for sorting into chronological order.
    public class CIEvent : IComparable
    {
        // Indicates preference in cases where multiple records for a one-off event exist (e.g. birth)
        public enum EPreference 
        {
            First, 
            Subsequent, 
            Unknown
        }

        // Date or date range for the event
        private CPGDate m_date;

        // If the event is important
        private bool m_bImportant;

        // String describing the event
        private string m_sDescription;

        // Note text for the event
        private string m_sNote;

        // Event overview from GEDCOM _OVER tag
        private string m_sOverview; 

        // GEDCOM tag for event
        private string m_sType; 

        // Indicates that this event is the preferred one, in cases where multiple records for a one-off event exist (e.g. birth)
        private EPreference m_epPreference; 

        // Constructor
        public CIEvent( CPGDate date, string sType, string sDescription, string sOverview, string sNote, bool sImportant, bool bCapitalise )
        {
            m_epPreference = EPreference.Unknown;
            m_date = date;
            if( bCapitalise )
            {
                CCreator.Capitalise( ref sDescription );
            }
            if( sDescription.Length > 0 )
            {
                if( sDescription[ sDescription.Length - 1 ] == '.' )
                {
                    m_sDescription = sDescription.Substring( 0, sDescription.Length - 1 );
                }
                else
                {
                    m_sDescription = sDescription;
                }
            }
            m_sOverview = sOverview;
            m_bImportant = sImportant;
            m_sNote = sNote;
            if( sNote == "" )
            {
                m_sNote = null;
            }
            m_sType = sType;
        }

        // Returns the event description string.
        public override string ToString()
        {
          return m_sDescription;
        }

        // Accessor
        public string Overview
        {
            get
            {
                return m_sOverview;
            }
        }

        // Accessor
        public bool Important
        {
            get
            {
                return m_bImportant;
            }
        }

        // Accessor
        public string Date
        {
            get
            {
                if( m_date == null )
        {
                    return null;
        }
                return m_date.ToString();
            }
        }

        // Accessor
        public string Note
        {
            get
            {
                return m_sNote;
            }
        }

        // Accessor
        public string Type
        {
          get
          {
              return m_sType;
          }
        }

        // Accessor
        public EPreference Preference
        {
          get
          {
            return m_epPreference;
          }
          set
          {
            m_epPreference = value;
          }
        }

        // Used when sorting events into chronological order. If no dates are explicitly attached to the event, tries to work out an order.
        public int CompareTo(object obj)
        {
            if(obj is CIEvent) 
            {
                CIEvent ievThat = (CIEvent)obj;

                if( m_date != null && ievThat.m_date != null )
                {
                    int res = m_date.CompareTo( ievThat.m_date );
                    if (res != 0)
                        return res;

                    // Some events naturally precede others: BIRTH, MARRIAGE, DEATH, BURIAL
                    int precedence = Precedence - ievThat.Precedence;
                    if( precedence != 0 )
                    {
                        return precedence;
                    }

                    // Sort alphabetically if all else fails.
                    return m_sDescription.CompareTo( ievThat.m_sDescription );
        
                }
                else if( ievThat.m_date != null ) 
                {
                    // This goes after valid dates.
                    return 1; 
                }
                else if( m_date != null ) 
                {
                    // This goes before invalid dates.
                    return -1; 
                }

                // Some events naturally precede others: BIRTH, MARRIAGE, DEATH, BURIAL
                int precedence2 = Precedence - ievThat.Precedence;
                if( precedence2 != 0 )
                {
                    return precedence2;
                }
                // Sort alphabetically if all else fails
                return m_sDescription.CompareTo( ievThat.m_sDescription );
        
            }
        
            throw new ArgumentException("object is not a CIEvent (248)");
        }

        // Returns a value between 0 (earliest) and 100 (latest) used to
        // sort events if explicit date isn't present.
        private int Precedence
        {
            get 
            {
                switch( Type )
                {
                    case "BIRT":
                        return 3;
                    case "CHR":
                        return 6;
                    case "BAPM":
                    case "BASM":
                    case "BLES":
                        return 9;
                    case "CONF":
                    case "BARM":
                        return 12;
                    case "FCOM": // first communion
                        return 15;
                    case "ADOP":
                        return 18;
                    case "CHRA": // christened as adult
                        return 21;
                    case "NATU": //naturalized
                        return 24;
                    case "GRAD":
                        return 27;
                    case "ORDN": //ordained
                        return 30;
                    case "ENGA":
                        return 33;
                    case "MARB":
                    case "MARC":
                    case "MARL":
                        return 36;
                    case "MARR":
                        return 39;
                    case "GEDMILL_ADOPTION_OF_CHILD":
                        return 42;
                    case "NCHI":
                        return 45;
                    case "EVEN":
                    case "EMIG":
                    case "IMMI":
                    case "CAST":
                    case "DSCR":
                    case "EDUC":
                    case "IDNO":
                    case "NATI":
                    case "NMR":
                    case "OCCU":
                    case "PROP":
                    case "RELI":
                    case "RESI":
                    case "SSN":
                    case "TITL":
                    case "FACT":
                    case "_AKA": // _AKA Brother's Keeper
                    case "_AKAN": // _AKAN Brother's Keeper
                    case "CENS":
                        // These could have occured at any time in their life, so return middle value
                        return 50;
                    case "DIVF":
                        return 53;
                    case "DIV":
                    case "ANUL"://annulment of marriage";
                        return 56;
                    case "MARS":
                        return 59;
                    case "_NMR": // _NMR Brother's Keeper never married
                        return 62;
                    case "RETI":
                    case "WILL":
                        return 65;
                    case "PROB":
                        return 68;
                    case "DEAT":
                        return 71;
                    case "BURI":
                        return 73;
                    case "CREM":
                        return 76;

                    default:
                        return 50;
                }
            }
        }
    }
}
