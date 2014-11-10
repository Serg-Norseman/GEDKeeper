/* CEventRecorded.cs
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

namespace GEDmill.LLClasses
{
    // GEDCOM 'EVEN'. See GEDCOM standard for details on GEDCOM data.
    public class CEventRecorded : GEDmill.LLClasses.CRecord
    {
        // GEDCOM data
        public string m_sEventRecorded;
        public string m_sDatePeriod;
        public string m_sSourceJurisdictionPlace;

        // Constructor
        public CEventRecorded( CGedcom gedcom ) : base( gedcom )
        {           
        }

        // Parser
        public static CEventRecorded Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string eventRecorded;
            string datePeriod = "";
            string sourceJurisdictionPlace = "";

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(nLevel, "EVEN")) == null)
            {
                // Not one of us
                return null;
            }
            gedcom.IncrementLineIndex(1);
            eventRecorded = gedcomLine.LineItem;

            bool bParsingFinished;
            do
            {
                bParsingFinished = true;

                if( (gedcomLine = gedcom.GetLine(nLevel+1, "DATE")) != null  )
                {
                    datePeriod = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "PLAC")) != null  )
                {
                    sourceJurisdictionPlace = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
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
            CEventRecorded er = new CEventRecorded( gedcom );
            er.m_sEventRecorded = eventRecorded;
            er.m_sDatePeriod = datePeriod;
            er.m_sSourceJurisdictionPlace = sourceJurisdictionPlace;
            return er;
        }

    } // End of class
} // End of namespace