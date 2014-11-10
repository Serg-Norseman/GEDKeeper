/* CNoteRecord.cs
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
    public class CNoteRecord : GEDmill.LLClasses.CRecord
    {
        // GEDCOM data
        public string m_sSubmitterText;
        public ArrayList m_alSourceCitations;

        // Constructor
        public CNoteRecord( CGedcom gedcom ) : base( gedcom )
        {
            m_alSourceCitations = new ArrayList();
        }

        // Parser
        public static CNoteRecord Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;
            bool bParsingFinished;

            // Temporary holders for class members.
            CSourceCitation sc;

            // Without an xref header, we can't continue
            if ((gedcomLine = gedcom.GetLine(nLevel, "NOTE")) == null)
            {
                // Not one of us
                return null;
            }
            
            CNoteRecord nr = new CNoteRecord( gedcom );

            nr.m_xref = gedcomLine.XrefID;
            nr.m_sSubmitterText += gedcomLine.LineItem;
            gedcom.IncrementLineIndex(1);

            int nLevelHack = nLevel; // This is because "Generations Grand Sierra" outputs NOTE->NOTE, ie. text as a sub-record
            bool bIncludeSpaceWithConc = false; // "Generations Grand Sierra" again misses out spaces on CONC lines.
            do
            {
                bParsingFinished = true;

                // Let Record have a go at parsing the rest
                if( nr.ParseRecord( gedcom, nLevelHack ) )
                {
                    bParsingFinished = false;
                    continue;
                }
                else if( (gedcomLine = gedcom.GetLine(nLevelHack+1, "NOTE")) != null )
                {
                    nr.m_sSubmitterText += gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                    nLevelHack++;
                    bIncludeSpaceWithConc = true;
                }
                else if( (gedcomLine = gedcom.GetLine(nLevelHack+1, "CONC")) != null )
                {
                    if( bIncludeSpaceWithConc )
                        nr.m_sSubmitterText += ' ';
                    nr.m_sSubmitterText += gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                else if( (gedcomLine = gedcom.GetLine(nLevelHack+1, "CONT")) != null )
                {
                    nr.m_sSubmitterText += "\n" + gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                else if( (sc = CSourceCitation.Parse( gedcom, nLevelHack+1 )) != null )
                {
                    nr.m_alSourceCitations.Add( sc );
                    bParsingFinished = false;
                }               
                else if( ( gedcomLine = gedcom.GetLine()).Level > nLevelHack )
                {
                    LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, "Unknown tag :" );
                    LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, gedcomLine.ToString() );
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
            }
            while( !bParsingFinished );

            return nr;
        }
    }
}
