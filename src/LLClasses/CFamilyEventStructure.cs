/* CFamilyEventStructure.cs
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
    // Base class for GEDCOM events (family events and individual events)
    public abstract class CEventStructure : GEDmill.LLClasses.CParserObject
    {
        public CEventDetail m_eventDetail;
        public string m_sSubtype;

        public CEventStructure(CGedcom gedcom) : base(gedcom)
        {
            m_sSubtype = "";
        }
    }

    // GEDCOM family events. See GEDCOM standard for details on GEDCOM data.
    public class CFamilyEventStructure : GEDmill.LLClasses.CEventStructure
    {
        // Constructor
        public CFamilyEventStructure( CGedcom gedcom ) : base( gedcom )
        {
        }

        // Parser
        public static CFamilyEventStructure Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string sType;
            string sValue;
            CEventDetail ed, eventDetail=null;
            string sSubtype = "";

            // There must be one of these, it defines the object.
            if( (gedcomLine = gedcom.GetLine(nLevel, "ANUL")) == null 
             && (gedcomLine = gedcom.GetLine(nLevel, "CENS")) == null 
             && (gedcomLine = gedcom.GetLine(nLevel, "DIV"))  == null 
             && (gedcomLine = gedcom.GetLine(nLevel, "DIVF")) == null 
             && (gedcomLine = gedcom.GetLine(nLevel, "ENGA")) == null 
             && (gedcomLine = gedcom.GetLine(nLevel, "MARB")) == null 
             && (gedcomLine = gedcom.GetLine(nLevel, "MARC")) == null 
             && (gedcomLine = gedcom.GetLine(nLevel, "MARR")) == null 
             && (gedcomLine = gedcom.GetLine(nLevel, "MARL")) == null 
             && (gedcomLine = gedcom.GetLine(nLevel, "MARS")) == null 
             && (gedcomLine = gedcom.GetLine(nLevel, "RESI")) == null 
             && (gedcomLine = gedcom.GetLine(nLevel, "EVEN")) == null )
            {
                return null;
            }
            sType = gedcomLine.Tag;
            sValue = gedcomLine.LineItem;
            gedcom.IncrementLineIndex(1);


            
            bool bParsingFinished;
            do
            {
                bParsingFinished = true;
                if( (gedcomLine = gedcom.GetLine(nLevel+1, "TYPE")) != null )
                {
                    sSubtype = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                if( (gedcomLine = gedcom.GetLine(nLevel+1, "CONC")) != null )
                {
                    sValue += gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }                   
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "CONT")) != null )
                {
                    sValue += "\n" + gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }                   
                else if( (ed = CEventDetail.Parse( gedcom, nLevel+1 )) != null )
                {
                    eventDetail = ed;
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
            CFamilyEventStructure fes = new CFamilyEventStructure( gedcom );
            fes.Type = sType;
            fes.Value = sValue;
            fes.m_eventDetail = eventDetail;
            fes.m_sSubtype = sSubtype;
            return fes;
        }
    }
}
