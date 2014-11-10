/* CChangeDate.cs
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
    // GEDCOM 'CHAN'. See GEDCOM standard for details on GEDCOM data.
    public class CChangeDate : GEDmill.LLClasses.CParserObject
    {
        // GEDCOM data
        public string m_sChangeDate;
        public string m_sTimeValue;
        public ArrayList m_alNoteStructures;

        // Constructor
        public CChangeDate( CGedcom gedcom ) : base( gedcom )
        {           
        }

        // Parser
        public static CChangeDate Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string sChangeDate = "";
            string sTimeValue = "";
            ArrayList alNoteStructures = new ArrayList();
            CNoteStructure noteStructure = null;

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(nLevel, "CHAN")) == null)
            {
                // Not one of us
                return null;
            }
            gedcom.IncrementLineIndex(1);

            bool bGotMandatoryItem = false;
            bool bParsingFinished;
            do
            {
                bParsingFinished = true;

                // There must be one of these, standard specifies {1:1}
                if( (gedcomLine = gedcom.GetLine(nLevel+1, "DATE")) != null  )
                {
                    sChangeDate = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    // There may be one of these, standard specifies {0:1}
                    if( (gedcomLine = gedcom.GetLine(nLevel+2, "TIME")) != null )
                    {
                        sTimeValue = gedcomLine.LineItem;
                        gedcom.IncrementLineIndex(1);
                    }
                    bParsingFinished = false;
                    bGotMandatoryItem = true;
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

            if( !bGotMandatoryItem )
            {
                LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, "Change Date is missing mandatory field." );
                return null;
            }
            
            // Parsing went ok. Construct a new object and return it.
            CChangeDate cd = new CChangeDate( gedcom );
            cd.m_sChangeDate = sChangeDate;
            cd.m_sTimeValue = sTimeValue;
            cd.m_alNoteStructures = alNoteStructures;
            return cd;
        }

    } // End of class
} // End of namespace
