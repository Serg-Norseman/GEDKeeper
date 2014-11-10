/* CPersonalNamePieces.cs
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
    // GEDCOM Personal Name Pieces. See GEDCOM standard for details on GEDCOM data.
    public class CPersonalNamePieces : GEDmill.LLClasses.CParserObject
    {
        // GEDCOM data
        public string m_sNamePiecePrefix;
        public string m_sNamePieceGiven;
        public string m_sNamePieceNickname;
        public string m_sNamePieceSurnamePrefix;
        public string m_sNamePieceSurname;
        public string m_sNamePieceSuffix;
        public ArrayList m_alNoteStructures;
        public ArrayList m_alSourceCitations;

        // Constructor
        public CPersonalNamePieces( CGedcom gedcom ) : base( gedcom )
        {
            m_sNamePiecePrefix = "";
            m_sNamePieceGiven = "";
            m_sNamePieceNickname = "";
            m_sNamePieceSurnamePrefix = "";
            m_sNamePieceSurname = "";
            m_sNamePieceSuffix = "";
            m_alNoteStructures = new ArrayList();
            m_alSourceCitations = new ArrayList();
        }

        // Parser
        public static CPersonalNamePieces Parse(CGedcom gedcom, int level)
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string sNamePiecePrefix="";
            string sNamePieceGiven="";
            string sNamePieceNickname="";
            string sNamePieceSurnamePrefix="";
            string sNamePieceSurname="";
            string sNamePieceSuffix="";
            ArrayList alNoteStructures = new ArrayList();
            ArrayList alSourceCitations = new ArrayList();

            CNoteStructure noteStructure;
            CSourceCitation sourceCitation;

            bool bGotSomething = false;
            bool bParsingFinished;
            do
            {
                bParsingFinished = true;

                if( (gedcomLine = gedcom.GetLine(level, "NPFX")) != null )
                {
                    sNamePiecePrefix = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                    bGotSomething = true;
                }               
                else if( (gedcomLine = gedcom.GetLine(level, "GIVN")) != null )
                {
                    sNamePieceGiven = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                    bGotSomething = true;
                }               
                else if( (gedcomLine = gedcom.GetLine(level, "NICK")) != null )
                {
                    sNamePieceNickname = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                    bGotSomething = true;
                }               
                else if( (gedcomLine = gedcom.GetLine(level, "SPFX")) != null )
                {
                    sNamePieceSurnamePrefix = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                    bGotSomething = true;
                }               
                else if( (gedcomLine = gedcom.GetLine(level, "SURN")) != null )
                {
                    sNamePieceSurname = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                    bGotSomething = true;
                }               
                else if( (gedcomLine = gedcom.GetLine(level, "NSFX")) != null )
                {
                    sNamePieceSuffix = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                    bGotSomething = true;
                }               
                else if( (noteStructure = CNoteStructure.Parse( gedcom, level )) != null )
                {
                    alNoteStructures.Add( noteStructure );
                    bParsingFinished = false;
                    bGotSomething = true;
                }
                else if( (sourceCitation = CSourceCitation.Parse( gedcom, level )) != null )
                {
                    alSourceCitations.Add( sourceCitation );
                    bParsingFinished = false;
                    bGotSomething = true;
                }
                else if( ( gedcomLine = gedcom.GetLine()).Level >= level )
                {
                    LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, "Unknown tag :" );
                    LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, gedcomLine.ToString() );
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
            }
            while( !bParsingFinished );
                        
            if( !bGotSomething )
            {
                return null;
            }

            // Parsing went ok. Construct a new object and return it.
            CPersonalNamePieces pnp = new CPersonalNamePieces( gedcom );
            pnp.m_sNamePiecePrefix = sNamePiecePrefix;
            pnp.m_sNamePieceGiven = sNamePieceGiven;
            pnp.m_sNamePieceNickname = sNamePieceNickname;
            pnp.m_sNamePieceSurnamePrefix = sNamePieceSurnamePrefix;
            pnp.m_sNamePieceSurname = sNamePieceSurname;
            pnp.m_sNamePieceSuffix = sNamePieceSuffix;
            pnp.m_alNoteStructures = alNoteStructures;
            pnp.m_alSourceCitations = alSourceCitations;
            return pnp;
        }
    } // End of class
} // End of namespace
