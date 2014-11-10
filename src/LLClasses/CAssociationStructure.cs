/* CAssociationStructure.cs
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
    // GEDCOM 'ASSO'. See GEDCOM standard for details on GEDCOM data.
    public class CAssociationStructure : GEDmill.LLClasses.CParserObject
    {
        // GEDCOM data.
        public string m_xrefIndi;
        public string m_sRelationIsDescriptor;
        public ArrayList m_alSourceCitations;
        public ArrayList m_alNoteStructures;
        public string m_sRecordType;

        // Constructor
        public CAssociationStructure( CGedcom gedcom ) : base( gedcom )
        {           
        }

        // Parser
        public static CAssociationStructure Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string xrefIndi;
            string sRelationIsDescriptor = "";
            CSourceCitation sourceCitation;
            ArrayList alSourceCitations = new ArrayList();
            CNoteStructure noteStructure;
            ArrayList alNoteStructures = new ArrayList();
            string sRecordType = "";

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(nLevel, "ASSO")) == null)
            {
                // Not one of us
                return null;
            }
            xrefIndi = gedcomLine.LinePointer;

            // Parsing is going well enough to say that we definitely have one of these, 
            // so we can adjust the gedcom now.
            gedcom.IncrementLineIndex(1);

            bool bParsingFinished;
            do
            {
                bParsingFinished = true;

                // There may be one of these, standard specifies {0:1}
                if( (gedcomLine = gedcom.GetLine(nLevel+1, "RELA")) != null )
                {
                    sRelationIsDescriptor = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }           
                else if( (sourceCitation = CSourceCitation.Parse( gedcom, nLevel+1 )) != null )
                {
                    alSourceCitations.Add( sourceCitation );
                    bParsingFinished = false;
                }
                else if( (noteStructure = CNoteStructure.Parse( gedcom, nLevel+1 )) != null )
                {
                    alNoteStructures.Add( noteStructure );
                    bParsingFinished = false;
                }               
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "TYPE")) != null )
                {
                    sRecordType = gedcomLine.LineItem;
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
            CAssociationStructure ass = new CAssociationStructure( gedcom );
            ass.m_xrefIndi = xrefIndi;
            ass.m_sRelationIsDescriptor = sRelationIsDescriptor;
            ass.m_alSourceCitations = alSourceCitations;
            ass.m_alNoteStructures = alNoteStructures;
            ass.m_sRecordType = sRecordType;
            return ass;
        }
    }
}
