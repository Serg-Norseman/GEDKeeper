/* CSubmitterRecord.cs
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
    // GEDCOM 'SUBM'. See GEDCOM standard for details on GEDCOM data.
    public class CSubmitterRecord : GEDmill.LLClasses.CRecord
    {
        // GEDCOM data
        public string m_sSubmitterName;
        public CAddressStructure m_addressStructure;
        public ArrayList m_alMultimediaLinks;
        public ArrayList m_alLanguagePreferences;
        public ArrayList m_alNoteStructures;

        // Constructor
        public CSubmitterRecord( CGedcom gedcom ) : base( gedcom )
        {
            m_alMultimediaLinks = new ArrayList();
            m_alLanguagePreferences = new ArrayList();
            m_alNoteStructures = new ArrayList();
        }

        // Parser
        public static CSubmitterRecord Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;
            bool bParsingFinished;

            // Temporary holders for class members.
            CNoteStructure ns;
            CMultimediaLink ml;
            CAddressStructure ads;

            // Without an xref header, we can't continue
            if ((gedcomLine = gedcom.GetLine(nLevel, "SUBM")) == null)
            {
                // Not one of us
                return null;
            }
            
            CSubmitterRecord sr = new CSubmitterRecord( gedcom );

            sr.m_xref = gedcomLine.XrefID;
            gedcom.IncrementLineIndex(1);

            do
            {
                bParsingFinished = true;

                // Let Record have a go at parsing the rest
                if( sr.ParseRecord( gedcom, nLevel ) )
                {
                    bParsingFinished = false;
                    continue;
                }
                else if( (gedcomLine = gedcom.GetLine( nLevel+1, "NAME" )) != null )
                {
                    sr.m_sSubmitterName = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                else if( (gedcomLine = gedcom.GetLine( nLevel+1, "LANG" )) != null )
                {
                    sr.m_alLanguagePreferences.Add( gedcomLine.LineItem );
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }               
                else if( (ads = CAddressStructure.Parse( gedcom, nLevel+1 )) != null )
                {
                    sr.m_addressStructure = ads;
                    bParsingFinished = false;
                }               
                else if( (ml = CMultimediaLink.Parse( gedcom, nLevel+1 )) != null )
                {
                    sr.m_alMultimediaLinks.Add( ml );
                    bParsingFinished = false;
                }
                else if( (ns = CNoteStructure.Parse( gedcom, nLevel+1 )) != null )
                {
                    sr.m_alNoteStructures.Add( ns );
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

            return sr;
        }
    } // End of class
} // End of namespace
