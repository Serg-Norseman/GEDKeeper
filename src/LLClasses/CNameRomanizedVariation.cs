/* CNameRomanizedVariation.cs
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
    // GEDCOM 'ROMN'. See GEDCOM standard for details on GEDCOM data.
    public class CNameRomanizedVariation : GEDmill.LLClasses.CParserObject
    {
        // GEDCOM data.
        public string m_sNameRomanizedVariation;
        public string m_sRomanizedType;
        public CPersonalNamePieces m_personalNamePieces;

        public CNameRomanizedVariation( CGedcom gedcom ) : base( gedcom )
        {
        }

        public static CNameRomanizedVariation Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLIne;

            // Temporary holders for class members. Saves constructing a class early.
            string sNameRomanizedVariation;
            string sRomanizedType = "";
            CPersonalNamePieces pnp, personalNamePieces=null;

            // There must be one of these, it defines the object.
            if ((gedcomLIne = gedcom.GetLine(nLevel, "ROMN")) == null)
            {
                // Not one of us
                return null;
            }
            sNameRomanizedVariation = gedcomLIne.LineItem;
            gedcom.IncrementLineIndex(1);

            bool bParsingFinished;
            do
            {
                bParsingFinished = true;

                if( (gedcomLIne = gedcom.GetLine(nLevel+1, "TYPE")) != null )
                {
                    sRomanizedType = gedcomLIne.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }               
                else if( (pnp = CPersonalNamePieces.Parse( gedcom, nLevel+1 )) != null )
                {
                    personalNamePieces = pnp;
                    bParsingFinished = false;
                }
                else if( ( gedcomLIne = gedcom.GetLine()).Level > nLevel )
                {
                    LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, "Unknown tag :" );
                    LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, gedcomLIne.ToString() );
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
            }
            while( !bParsingFinished );
            
            // Parsing went ok. Construct a new object and return it.
            CNameRomanizedVariation nrv = new CNameRomanizedVariation( gedcom );
            nrv.m_sNameRomanizedVariation = sNameRomanizedVariation;
            nrv.m_sRomanizedType = sRomanizedType;
            nrv.m_personalNamePieces = personalNamePieces;
            return nrv;
        }
    }
}
