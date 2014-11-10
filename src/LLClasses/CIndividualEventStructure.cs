/* CIndividualEventStructure.cs
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
using GEDmill.HTMLClasses;

namespace GEDmill.LLClasses
{
    // GEDCOM individual events and attributes. See GEDCOM standard for details on GEDCOM data.
    // Also encompassed INDIVIDUAL_ATTRIBUTE_STRUCTURE
    public class CIndividualEventStructure : GEDmill.LLClasses.CEventStructure, IComparable
    {
        // Constructor
        public CIndividualEventStructure( CGedcom gedcom ) : base( gedcom )
        {           
        }

        // Copy constructor
        public CIndividualEventStructure( CEventDetail ed ) : base( ed.Gedcom )
        {
            m_eventDetail = new CEventDetail( ed );
        }

        // Parser
        public static CIndividualEventStructure Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string sType;
            string sValue;
            string sSubtype = "";
            CEventDetail ed, eventDetail=null;

            // There must be one of these, it defines the object.
            if( 
                (gedcomLine = gedcom.GetLine(nLevel, "_NMR")) == null  // Never married (brother's keeper)
                && (gedcomLine = gedcom.GetLine(nLevel, "_AKA")) == null  // Also known as (brother's keeper)
                && (gedcomLine = gedcom.GetLine(nLevel, "_AKAN")) == null // Also known as (brother's keeper)
                && (gedcomLine = gedcom.GetLine(nLevel, "BIRT")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "CHR")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "DEAT"))  == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "BURI")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "CREM")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "ADOP")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "BAPM")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "BAP")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "BARM")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "BASM")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "BLES")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "CHRA")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "CONF")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "FCOM")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "ORDN")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "NATU")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "EMIG")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "IMMI")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "CENS")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "PROB")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "WILL")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "GRAD")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "RETI")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "EVEN")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "CAST")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "DSCR")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "EDUC")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "IDNO")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "NATI")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "NCHI")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "NMR")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "OCCU")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "PROP")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "RELI")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "RESI")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "SSN")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "TITL")) == null 
                && (gedcomLine = gedcom.GetLine(nLevel, "FACT")) == null 
                 )  
            {
                return null;
            }
            sType = gedcomLine.Tag;
            if( sType == "BAP" )
            {
                sType = "BAPM";
            }
            sValue = gedcomLine.LineItem;
            gedcom.IncrementLineIndex(1);

            
            bool bParsingFinished;
            do
            {
                bParsingFinished = true;
                if( (gedcomLine = gedcom.GetLine(nLevel+1, "TYPE")) != null )
                {
                    sSubtype = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                if( (gedcomLine = gedcom.GetLine(nLevel+1, "CONC")) != null )
                {
                    sValue += gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }                   
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "CONT")) != null )
                {
                    sValue += "\n" + gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }                   
                else if( (ed = CEventDetail.Parse( gedcom, nLevel+1 )) != null )
                {
                    eventDetail = ed;
                    bParsingFinished = false;
                }   
                else if( ( gedcomLine = gedcom.GetLine()).Level > nLevel )
                {
                    LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, "Unknown tag :" );
                    LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, gedcomLine.ToString() );
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }               
            }
            while( !bParsingFinished );

            // Parsing went ok. Construct a new object and return it.
            CIndividualEventStructure ies = new CIndividualEventStructure( gedcom );
            ies.Type = sType;
            ies.m_sSubtype = sSubtype;
            ies.Value = sValue;
            ies.m_eventDetail = eventDetail;
            return ies;
        }
        
        // Comparer
        public int CompareTo(object obj)
        {
            if(obj is CIndividualEventStructure) 
            {
                CIndividualEventStructure ies = (CIndividualEventStructure) obj;

                CPGDate date1 = null;
                CPGDate date2 = null;

                if( m_eventDetail != null )
                {
                    date1 = m_eventDetail.m_dateValue;
                }
                if( ies.m_eventDetail != null )
                {
                    date2 = ies.m_eventDetail.m_dateValue;
                }
            
                if( date1 != null )
                {
                    if (date2 != null)
                    {
                        return date1.CompareTo(date2);
                    }
                    // Puts this event before null dates
                    return -1; 
                }


                if (date2 != null)
                {
                    // Puts this event after non-null dates
                    return 1; 
                }


                return Type.CompareTo( ies.Type );
            }
            throw new ArgumentException("object is not a CIndividualEventStructure (252)");    
        }
    }
}
