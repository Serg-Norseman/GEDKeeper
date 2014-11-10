/* CSpouseToFamilyLink.cs
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
using System.Collections;

namespace GEDmill.LLClasses
{
    // GEDCOM FAMS. See GEDCOM standard for details.
    public class CSpouseToFamilyLink : GEDmill.LLClasses.CParserObject
    {
        // Identifies the family this person is a spouse to.
        public string m_xrefFam;

        // Notes associated with this connection
        public ArrayList m_alNoteStructures;

        // Constructor
        public CSpouseToFamilyLink( CGedcom gedcom ) : base( gedcom )
        {
        }

        // Parser
        public static CSpouseToFamilyLink Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string xrefFam;
            CNoteStructure noteStructure;
            ArrayList noteStructures = new ArrayList();

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(nLevel, "FAMS")) == null)
            {
                // Not one of us
                return null;
            }
            xrefFam = gedcomLine.LinePointer;

            // Parsing is going well enough to say that we definitely have one of these, 
            // so we can adjust the gedcom now.
            gedcom.IncrementLineIndex(1);

            bool bParsingFinished;
            do
            {
                bParsingFinished = true;

                if( (noteStructure = CNoteStructure.Parse( gedcom, nLevel+1 )) != null )
                {
                    noteStructures.Add( noteStructure );
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
            CSpouseToFamilyLink sfl = new CSpouseToFamilyLink( gedcom );
            sfl.m_xrefFam = xrefFam;
            sfl.m_alNoteStructures = noteStructures;
            return sfl;
        }
    }
}
