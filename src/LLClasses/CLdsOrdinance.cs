/* CLdsOrdinance.cs
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
    // GEDCOM LDS Ordinance. See GEDCOM standard for details on GEDCOM data.
    public class CLdsOrdinance : GEDmill.LLClasses.CParserObject
    {
        // GEDCOM data
        public string m_sDateLdsOrd;
        public string m_sTempleCode;
        public string m_sPlaceLivingOrdinance;
        public string m_xrefFam;
        public string m_sLdsDateStatus;
        public string m_sChangeDate;
        public ArrayList m_alNoteStructures;
        public ArrayList m_alSourceCitations;

        // Constructor
        public CLdsOrdinance( CGedcom gedcom ) : base( gedcom )
        {
        }

        // Parser
        public static CLdsOrdinance Parse( CGedcom gedcom, int level )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string sType;
            string sDateLdsOrd = "";
            string sTempleCode = "";
            string sPlaceLivingOrdinance = "";
            string xrefFam = "";
            string sLdsDateStatus = "";
            string sChangeDate = "";
            ArrayList noteStructures = new ArrayList();
            ArrayList sourceCitations = new ArrayList();

            CSourceCitation sourceCitation;
            CNoteStructure noteStructure;

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(level, "BAPL")) == null
              || (gedcomLine = gedcom.GetLine(level, "ENDL")) == null
              || (gedcomLine = gedcom.GetLine(level, "SLGC")) == null
              || (gedcomLine = gedcom.GetLine(level, "CONL")) == null
              || (gedcomLine = gedcom.GetLine(level, "SLGS")) == null)
            {
                // Not one of us
                return null; 
            }

            sType = gedcomLine.Tag;
            gedcom.IncrementLineIndex(1);

            bool bParsingFinished;
            do
            {
                bParsingFinished = true;

                if( (gedcomLine = gedcom.GetLine(level+1, "DATE")) != null )
                {
                    sDateLdsOrd = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }               
                else if( (gedcomLine = gedcom.GetLine(level+1, "TEMP")) != null )
                {
                    sTempleCode = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }   
                else if( (gedcomLine = gedcom.GetLine(level+1, "PLAC")) != null )
                {
                    sPlaceLivingOrdinance = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }                   
                else if( (gedcomLine = gedcom.GetLine(level+1, "FAMC")) != null )
                {
                    xrefFam = gedcomLine.LinePointer;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }                           
                else if( (gedcomLine = gedcom.GetLine(level+1, "STAT")) != null )
                {
                    sLdsDateStatus = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    if( (gedcomLine = gedcom.GetLine(level+2, "DATE")) != null )
                    {
                        sChangeDate = gedcomLine.LineItem;
                        gedcom.IncrementLineIndex(1);
                    }   
                    bParsingFinished = false;
                }   
                else if( (sourceCitation = CSourceCitation.Parse( gedcom, level+1 )) != null )
                {
                    sourceCitations.Add( sourceCitation );
                    bParsingFinished = false;
                }
                else if( (noteStructure = CNoteStructure.Parse( gedcom, level+1 )) != null )
                {
                    noteStructures.Add( noteStructure );
                    bParsingFinished = false;
                }
                else if( ( gedcomLine = gedcom.GetLine()).Level > level )
                {
                    LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, "Unknown tag :" );
                    LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, gedcomLine.ToString() );
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
            }
            while( !bParsingFinished );
            
            // Parsing went ok. Construct a new object and return it.
            CLdsOrdinance lss = new CLdsOrdinance( gedcom );
            lss.Type = sType;
            lss.m_sDateLdsOrd = sDateLdsOrd;
            lss.m_sTempleCode = sTempleCode;
            lss.m_sPlaceLivingOrdinance = sPlaceLivingOrdinance;
            lss.m_xrefFam = xrefFam;
            lss.m_sLdsDateStatus = sLdsDateStatus;
            lss.m_sChangeDate = sChangeDate;
            lss.m_alNoteStructures = noteStructures;
            lss.m_alSourceCitations = sourceCitations;
            return lss;
        }
    }
}
