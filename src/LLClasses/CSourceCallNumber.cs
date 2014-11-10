/* CSourceCallNumber.cs
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
    // GEDCOM Source Call Number
    public class CSourceCallNumber : GEDmill.LLClasses.CParserObject
    {
        // GEDCOM data
        public string m_sSourceCallNumber;
        public string m_sSourceMediaType;

        // Constructor
        public CSourceCallNumber( CGedcom gedcom ) : base( gedcom )
        {
        }

        // Parser
        public static CSourceCallNumber Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string sSourceCallNumber;
            string sSourceMediaType = "";

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(nLevel, "CALN")) == null)
            {
                // Not one of us
                return null; 
            }
            gedcom.IncrementLineIndex(1);
            sSourceCallNumber = gedcomLine.LineItem;

            if( (gedcomLine = gedcom.GetLine(nLevel+1, "MEDI")) != null  )
            {
                sSourceMediaType = gedcomLine.LineItem;
                gedcom.IncrementLineIndex(1);
            }
            else if( ( gedcomLine = gedcom.GetLine()).Level > nLevel )
            {
                LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, "Unknown tag :" );
                LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, gedcomLine.ToString() );
                gedcom.IncrementLineIndex(1);
            }

            // Parsing went ok. Construct a new object and return it.
            CSourceCallNumber scn = new CSourceCallNumber( gedcom );
            scn.m_sSourceCallNumber = sSourceCallNumber;
            scn.m_sSourceMediaType = sSourceMediaType;
            return scn;
        }

    } // End of class
} // End of namespace