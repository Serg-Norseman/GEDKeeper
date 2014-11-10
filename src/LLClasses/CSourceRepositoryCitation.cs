/* CSourceRepositoryCitation.cs
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
    // GEDCOM 'REPO'. See GEDCOM standard for details on GEDCOM data.
    public class CSourceRepositoryCitation : GEDmill.LLClasses.CParserObject
    {
        // GEDCOM data
        public string m_xrefRepo;
        public ArrayList m_alNoteStructures;
        public ArrayList m_alSourceCallNumbers;

        // Constructor
        public CSourceRepositoryCitation( CGedcom gedcom ) : base( gedcom )
        {
        }

        // Parser
        public static CSourceRepositoryCitation Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string xrefRepo;
            ArrayList alSourceCallNumbers = new ArrayList();
            CSourceCallNumber sourceCallNumber = null;
            ArrayList alNoteStructures = new ArrayList();
            CNoteStructure noteStructure = null;

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(nLevel, "REPO")) == null)
            {
                // Not one of us
                return null;
            }
            xrefRepo = gedcomLine.LinePointer;
            gedcom.IncrementLineIndex(1);

            bool bParsingFinished;
            do
            {
                bParsingFinished = true;

                if( (noteStructure = CNoteStructure.Parse( gedcom, nLevel+1 )) != null )
                {
                    alNoteStructures.Add( noteStructure );
                    bParsingFinished = false;
                }
                else if( (sourceCallNumber = CSourceCallNumber.Parse( gedcom, nLevel+1 )) != null )
                {
                    alSourceCallNumbers.Add( sourceCallNumber );
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
            CSourceRepositoryCitation src = new CSourceRepositoryCitation( gedcom );
            src.m_xrefRepo = xrefRepo;
            src.m_alSourceCallNumbers = alSourceCallNumbers;
            src.m_alNoteStructures = alNoteStructures;
            return src;
        }

    } // End of class
} // End of namespace