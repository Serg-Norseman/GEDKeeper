/* CNamePhoneticVariation.cs
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
    // GEDCOM 'FONE'. See GEDCOM standard for details on GEDCOM data.
    public class CNamePhoneticVariation : GEDmill.LLClasses.CParserObject
    {
        public string m_sNamePhoneticVariation;
        public string m_sPhoneticType;
        public CPersonalNamePieces m_personalNamePieces=null;

        public CNamePhoneticVariation( CGedcom gedcom ) : base( gedcom )
        {
        }

        public static CNamePhoneticVariation Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string sNamePhoneticVariation;
            string sPhoneticType = "";
            CPersonalNamePieces pnp, personalNamePieces=null;

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(nLevel, "FONE")) == null)
            {
                // Not one of us
                return null;
            }
            sNamePhoneticVariation = gedcomLine.LineItem;
            gedcom.IncrementLineIndex(1);

            bool bParsingFinished;
            do
            {
                bParsingFinished = true;

                if( (gedcomLine = gedcom.GetLine(nLevel+1, "TYPE")) != null )
                {
                    sPhoneticType = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }               
                else if( (pnp = CPersonalNamePieces.Parse( gedcom, nLevel+1 )) != null )
                {
                    pnp = personalNamePieces;
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
            CNamePhoneticVariation npv = new CNamePhoneticVariation( gedcom );
            npv.m_sNamePhoneticVariation = sNamePhoneticVariation;
            npv.m_sPhoneticType = sPhoneticType;
            npv.m_personalNamePieces = personalNamePieces;
            return npv;
        }
    }
}
