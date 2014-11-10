/* CUserReferenceNumber.cs
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
    // GEDCOM user reference number. See GEDCOM standard for details.
    public class CUserReferenceNumber : GEDmill.LLClasses.CParserObject
    {
        // GEDCOM data
        public string m_sUserReferenceNumber;
        public string m_sUserReferenceType;

        // Constructor
        public CUserReferenceNumber( CGedcom gedcom ) : base( gedcom )
        {
        }

        // Parser
        public static CUserReferenceNumber Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string sUserReferenceNumber = "";
            string sUserReferenceType = "";

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(nLevel, "REFN")) == null)
            {
                // Not one of us
                return null;
            }
            sUserReferenceNumber = gedcomLine.LineItem;

            // Parsing is going well enough to say that we definitely have one of these, 
            // so we can adjust the gedcom now.
            gedcom.IncrementLineIndex(1);

            // There may be one of these, standard specifies {0:1}
            if( (gedcomLine = gedcom.GetLine(nLevel+1, "TYPE")) != null )
            {
                sUserReferenceType = gedcomLine.LineItem;
                gedcom.IncrementLineIndex(1);
            }
            else if( ( gedcomLine = gedcom.GetLine()).Level > nLevel )
            {
                LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, "Unknown tag :" );
                LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, gedcomLine.ToString() );
                gedcom.IncrementLineIndex(1);
            }

            // Parsing went ok. Construct a new object and return it.
            CUserReferenceNumber urn = new CUserReferenceNumber( gedcom );
            urn.m_sUserReferenceNumber = sUserReferenceNumber;
            urn.m_sUserReferenceType = sUserReferenceType;
            return urn;
        }
    }
}
