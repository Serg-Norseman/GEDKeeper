/* CPlaceStructure.cs
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
    // GEDCOM 'PLAC'. See GEDCOM standard for details on GEDCOM data.
    public class CPlaceStructure : GEDmill.LLClasses.CParserObject
    {
        // GEDCOM data
        public string m_sPlaceName;
        public string m_sPlaceHierarchy;
        public string m_sPlacePhoneticVariation;
        public string m_sPlacePhoneticType;
        public string m_sPlaceRomanizedVariation;
        public string m_sPlaceRomanizedType;
        public string m_sPlaceLatitude;
        public string m_sPlaceLongitude;
        public ArrayList m_alNoteStructures;
        public ArrayList m_alSourceCitations;

        // Constructor
        public CPlaceStructure( CGedcom gedcom ) : base( gedcom )
        {
        }

        // Constructor
        public CPlaceStructure( CPlaceStructure ps ) : base( ps.Gedcom )
        {
            m_sPlaceName = ps.m_sPlaceName;
            m_sPlaceHierarchy = ps.m_sPlaceHierarchy;
            m_sPlacePhoneticVariation = ps.m_sPlacePhoneticVariation;
            m_sPlacePhoneticType = ps.m_sPlacePhoneticType;
            m_sPlaceRomanizedVariation = ps.m_sPlaceRomanizedVariation;
            m_sPlaceRomanizedType = ps.m_sPlaceRomanizedType;
            m_sPlaceLatitude = ps.m_sPlaceLatitude;
            m_sPlaceLongitude = ps.m_sPlaceLongitude;
            m_alNoteStructures = new ArrayList();
            foreach( CNoteStructure ns in ps.m_alNoteStructures )
            {
                m_alNoteStructures.Add( ns.CopyConstructor() );
            }
            m_alSourceCitations = new ArrayList();
            foreach( CSourceCitation sc in ps.m_alSourceCitations )
            {
                m_alSourceCitations.Add( sc.CopyConstructor() );
            }
        }

        // Parser
        public static CPlaceStructure Parse( CGedcom gedcom, int level )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            string sPlaceName;
            string sPlaceHierarchy = "";
            string sPlacePhoneticVariation = "";
            string sPlacePhoneticType = "";
            string sPlaceRomanizedVariation = "";
            string sPlaceRomanizedType = "";
            string sPlaceLatitude = "";
            string sPlaceLongitude = "";
            ArrayList noteStructures = new ArrayList();
            ArrayList sourceCitations = new ArrayList();

            CSourceCitation sourceCitation;
            CNoteStructure noteStructure;

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(level, "PLAC")) == null)
            {
                // Not one of us
                return null; 
            }
            sPlaceName = gedcomLine.LineItem;
            gedcom.IncrementLineIndex(1);

            bool bParsingFinished;
            do
            {
                bParsingFinished = true;

                if( (gedcomLine = gedcom.GetLine(level+1, "FORM")) != null )
                {
                    sPlaceHierarchy = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }               
                else if( (gedcomLine = gedcom.GetLine(level+1, "FONE")) != null ) // WRONG!! These are 0:M
                {
                    sPlacePhoneticVariation = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    if( (gedcomLine = gedcom.GetLine(level+2, "TYPE")) != null )
                    {
                        sPlacePhoneticType = gedcomLine.LineItem;
                        gedcom.IncrementLineIndex(1);
                    }   
                    bParsingFinished = false;
                }   
                else if( (gedcomLine = gedcom.GetLine(level+1, "ROMN")) != null ) // WRONG!! These are 0:M
                {
                    sPlaceRomanizedVariation = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    if( (gedcomLine = gedcom.GetLine(level+2, "TYPE")) != null )
                    {
                        sPlaceRomanizedType = gedcomLine.LineItem;
                        gedcom.IncrementLineIndex(1);
                    }   
                    bParsingFinished = false;
                }                   
                else if( (gedcomLine = gedcom.GetLine(level+1, "MAP")) != null )
                {
                    gedcom.IncrementLineIndex(1);
                    bool bParsingFinished2;
                    do
                    {
                        bParsingFinished2 = true;
                        if( (gedcomLine = gedcom.GetLine(level+2, "LATI")) != null )
                        {
                            sPlaceLatitude = gedcomLine.LineItem;
                            gedcom.IncrementLineIndex(1);
                            bParsingFinished2 = false;
                        }   
                        else if( (gedcomLine = gedcom.GetLine(level+2, "LONG")) != null )
                        {
                            sPlaceLongitude = gedcomLine.LineItem;
                            gedcom.IncrementLineIndex(1);
                            bParsingFinished2 = false;
                        }                   
                    }
                    while( !bParsingFinished2 );

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
            CPlaceStructure ps = new CPlaceStructure( gedcom );
            ps.m_sPlaceName = sPlaceName;
            ps.m_sPlaceHierarchy = sPlaceHierarchy;
            ps.m_sPlacePhoneticVariation = sPlacePhoneticVariation;
            ps.m_sPlacePhoneticType = sPlacePhoneticType;
            ps.m_sPlaceRomanizedVariation = sPlaceRomanizedVariation;
            ps.m_sPlaceRomanizedType = sPlaceRomanizedType;
            ps.m_sPlaceLatitude = sPlaceLatitude;
            ps.m_sPlaceLongitude = sPlaceLongitude;
            ps.m_alNoteStructures = noteStructures;
            ps.m_alSourceCitations = sourceCitations;
            return ps;
        }
    }
}
