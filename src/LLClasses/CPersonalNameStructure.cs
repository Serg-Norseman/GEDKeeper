/* CPersonalNameStructure.cs
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
    // GEDCOM 'NAME'. See GEDCOM standard for details on GEDCOM data.
    public class CPersonalNameStructure : GEDmill.LLClasses.CParserObject
    {
        // GEDCOM data
        public string m_sNamePersonal;
        public string m_sNameType;
        public string m_sUsedName;
        public CPersonalNamePieces m_personalNamePieces;
        public ArrayList m_alNamePhoneticVariations;
        public ArrayList m_alNameRomanizedVariations;

        // Constructor
        public CPersonalNameStructure( CGedcom gedcom ) : base( gedcom )
        {
        }

        // Parser
        public static CPersonalNameStructure Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string sNamePersonal;
            string sNameTempSuffix="";
            string sNameType="";
            string sUsedName = "";
            CPersonalNamePieces pnp, personalNamePiece=null;
            ArrayList alNamePhoneticVariations = new ArrayList();
            ArrayList alNameRomanizedVariations = new ArrayList();

            CNamePhoneticVariation namePhoneticVariation;
            CNameRomanizedVariation nameRomanizedVariation;
            
            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(nLevel, "NAME")) == null)
            {
                // Not one of us
                return null; 
            }
            sNamePersonal = gedcomLine.LineItem;

            // Some sw outputs name suffixes in the personal name, e.g. Fred /Bloggs/ Snr
            // Store the suffix, and let caller subsequently move the suffix to a proper class.
            int i = sNamePersonal.LastIndexOf('/') + 1;
            int l = sNamePersonal.Length;
            if (i != l)
            {
                int j = i;
                while ( j<l && (Char.IsWhiteSpace(sNamePersonal[j]) || sNamePersonal[j] == ','))
                {
                    ++j;
                }
                if (j < l)
                {
                    sNameTempSuffix = sNamePersonal.Substring(j);

                    personalNamePiece = new CPersonalNamePieces(gedcom);
                    personalNamePiece.m_sNamePieceSuffix = sNameTempSuffix;
                    sNamePersonal = sNamePersonal.Substring(0, i);
                }
            }
            gedcom.IncrementLineIndex(1);

            bool bParsingFinished;
            do
            {
                bParsingFinished = true;

                // Test for underscore items first so that parser doesn't skip them later
                if( (gedcomLine = gedcom.GetLine(nLevel+1, "_USED")) != null )
                {
                    sUsedName = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "TYPE")) != null )
                {
                    sNameType = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }   
                else if( (pnp = CPersonalNamePieces.Parse( gedcom, nLevel+1 ) ) != null )
                {
                    personalNamePiece = pnp;
                    bParsingFinished = false;
                }   
                else if( (namePhoneticVariation = CNamePhoneticVariation.Parse( gedcom, nLevel+1 ) ) != null )
                {
                    alNamePhoneticVariations.Add( namePhoneticVariation );
                    bParsingFinished = false;
                }   
                else if( (nameRomanizedVariation = CNameRomanizedVariation.Parse( gedcom, nLevel+1 ) ) != null )
                {
                    alNameRomanizedVariations.Add( nameRomanizedVariation );
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
            CPersonalNameStructure pns = new CPersonalNameStructure( gedcom );
            pns.m_sNamePersonal = sNamePersonal;
            pns.m_sNameType = sNameType;
            pns.m_sUsedName = sUsedName;
            pns.m_personalNamePieces = personalNamePiece;
            pns.m_alNamePhoneticVariations = alNamePhoneticVariations;
            pns.m_alNameRomanizedVariations = alNameRomanizedVariations;
            return pns;
        }
    }
}
