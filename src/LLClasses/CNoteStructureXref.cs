/* CNoteStructureXref.cs
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
    // GEDCOM 'NOTE'. See GEDCOM standard for details on GEDCOM data.
    public class CNoteStructureXref : GEDmill.LLClasses.CNoteStructure
    {
        // The xref id
        public string m_xref;

        // Constructor
        public CNoteStructureXref( CGedcom gedcom ) : base( gedcom )
        {
        }

        // Copy constructor
        public override CNoteStructure CopyConstructor()
        {
            CNoteStructureXref ns = new CNoteStructureXref( Gedcom );
            ns.m_xref = m_xref;
            return ns;
        }

        // Parser
        public static new CNoteStructureXref Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string xref;
            ArrayList sourceCitations = new ArrayList();
            CSourceCitation sourceCitation;

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(nLevel, "NOTE")) == null)
            {
                // Not one of us
                return null;
            }

            xref = gedcomLine.LinePointer;

            // Parsing is going well enough to say that we definitely have one of these, 
            // so we can adjust the gedcom now.
            gedcom.IncrementLineIndex(1);

            bool bParsingFinished;
            do
            {
                bParsingFinished = true;

                if( (sourceCitation = CSourceCitation.Parse( gedcom, nLevel+1 )) != null )
                {
                    sourceCitations.Add( sourceCitation );
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
            CNoteStructureXref ns = new CNoteStructureXref( gedcom );
            ns.m_xref = xref;
            ns.m_alSourceCitations = sourceCitations;
            return ns;
        }

        // Returns the text of the note.
        public override string GetText()
        {
            CNoteRecord nr = Gedcom.GetNoteRecord( m_xref );
            if( nr != null )
            {
                return nr.m_sSubmitterText;
            }
            return "Note missing.";
        }

    }
}
