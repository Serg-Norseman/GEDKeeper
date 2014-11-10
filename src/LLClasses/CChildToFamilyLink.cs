/* CChildToFamilyLink.cs
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
    // GEDCOM 'FAMC'. See GEDCOM standard for details on GEDCOM data.
    public class CChildToFamilyLink : GEDmill.LLClasses.CParserObject
    {
        // GEDCOM data.
        public string m_xrefFam;
        public string m_sPedigreeLinkageType;
        public string m_sChildLinkageStatus;
        public ArrayList m_alNoteStructures;

        // Constructor
        public CChildToFamilyLink( CGedcom gedcom ) : base( gedcom )
        {           
        }

        // Parser
        public static CChildToFamilyLink Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string xrefFam;
            string sPedigreeLinkageType = "";
            string sChildLinkageStatus = "";
            CNoteStructure noteStructure;
            ArrayList alNoteStructures = new ArrayList();

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(nLevel, "FAMC")) == null)
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

                // There may be one of these, standard specifies {0:1}
                if( (gedcomLine = gedcom.GetLine(nLevel+1, "PEDI")) != null )
                {
                    sPedigreeLinkageType = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "STAT")) != null )
                {
                    sChildLinkageStatus = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }               
                else if( (noteStructure = CNoteStructure.Parse( gedcom, nLevel+1 )) != null )
                {
                    alNoteStructures.Add( noteStructure );
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
            CChildToFamilyLink cfl = new CChildToFamilyLink( gedcom );
            cfl.m_xrefFam = xrefFam;
            cfl.m_sPedigreeLinkageType = sPedigreeLinkageType;
            cfl.m_sChildLinkageStatus = sChildLinkageStatus;
            cfl.m_alNoteStructures = alNoteStructures;
            return cfl;
        }
    }
}
