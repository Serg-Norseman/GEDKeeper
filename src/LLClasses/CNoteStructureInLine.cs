/* CNoteStructureInLine.cs
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
    public class CNoteStructureInLine : GEDmill.LLClasses.CNoteStructure
    {
        // GEDCOM data
        public string m_sSubmitterText;

        // Family historian ASID attributes
        public string m_sArea;
        public string m_sAsid;

        // Constructor
        public CNoteStructureInLine( CGedcom gedcom ) : base( gedcom )
        {
        }

        // Copy constructor
        public override CNoteStructure CopyConstructor()
        {
            CNoteStructureInLine ns = new CNoteStructureInLine( Gedcom );
            ns.m_sSubmitterText = m_sSubmitterText;
            ns.m_sAsid = m_sAsid;
            ns.m_sArea = m_sArea;
            return ns;
        }

        // Parser
        public static new CNoteStructureInLine Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string sSubmitterText;
            string sAsid = "";
            string sArea = "";
            CSourceCitation sourceCitation = null;

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(nLevel, "NOTE")) == null)
            {
                // Not one of us
                return null;
            }
            sSubmitterText = gedcomLine.LineItem;

            // Parsing is going well enough to say that we definitely have one of these, 
            // so we can adjust the gedcom now.
            gedcom.IncrementLineIndex(1);

            ArrayList sourceCitations = new ArrayList();

            bool bParsingFinished;
            do
            {
                bParsingFinished = true;

                // Test for underscore items first so that parser doesn't skip them later
                if( (gedcomLine = gedcom.GetLine(nLevel+1, "_ASID")) != null )
                {
                    sAsid = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }               
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "_AREA")) != null )
                {
                    sArea = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }   
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "CONC")) != null )
                {
                    sSubmitterText += gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                    // There may be one of these, standard specifies {0:M}
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "CONT")) != null )
                {
                    sSubmitterText += "\n" + gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }           
                else if( (sourceCitation = CSourceCitation.Parse( gedcom, nLevel+1 )) != null )
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
            CNoteStructureInLine ns = new CNoteStructureInLine( gedcom );
            ns.m_sSubmitterText = sSubmitterText;
            ns.m_sAsid = sAsid;
            ns.m_sArea = sArea;
            ns.m_alSourceCitations = sourceCitations;
            return ns;
        }

        // Returns the text of the note.
        public override string GetText()
        {
            return m_sSubmitterText;
        }

    }
}
