/* CMultimediaRecord.cs
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
    // GEDCOM 'OBJE'. See GEDCOM standard for details on GEDCOM data.
    public class CMultimediaRecord : GEDmill.LLClasses.CRecord
    {
        // GEDCOM data
        public ArrayList m_alMultimediaFileReferences;
        private ArrayList m_alNoteStructures;
        private ArrayList m_alSourceCitations;
        public Hashtable m_htAsidPairs;

        // Constructor
        public CMultimediaRecord( CGedcom gedcom ) : base( gedcom )
        {
            m_alMultimediaFileReferences = new ArrayList();
            m_alNoteStructures = new ArrayList();
            m_alSourceCitations = new ArrayList();
            m_htAsidPairs = new Hashtable();
        }

        // Parser
        public static CMultimediaRecord Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;
            bool bParsingFinished;

            // Without an xref header, we can't continue
            if ((gedcomLine = gedcom.GetLine(nLevel, "OBJE")) == null)
            {
                // Not one of us
                return null; 
            }
            
            CMultimediaRecord mr = new CMultimediaRecord( gedcom );
            mr.m_xref = gedcomLine.XrefID;
            gedcom.IncrementLineIndex(1);
            CMultimediaFileReference mfr = null;

            CNoteStructure ns;
            CSourceCitation sc;
            System.Text.StringBuilder blob = new System.Text.StringBuilder(256);

            do
            {
                bParsingFinished = true;

                // Family Historian style. Underscore, so check for this first
                if( (gedcomLine = gedcom.GetLine(nLevel+1, "_FILE" )) != null )
                {
                    if( mfr == null )
                        mfr = new CMultimediaFileReference( gedcom );

                    mfr.m_sMultimediaFileReference = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }

                // Let Record have a go at parsing the rest
                else if( mr.ParseRecord( gedcom, nLevel ) )
                {
                    bParsingFinished = false;
                    continue;
                }

                // 5.5 style
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "FORM")) != null  )
                {
                    if( mfr == null )
                        mfr = new CMultimediaFileReference( gedcom );

                    mfr.m_sMultimediaFormat = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);

                    // There may be one of these, standard specifies {0:1}
                    if( (gedcomLine = gedcom.GetLine(nLevel+2, "TITL")) != null )
                    {
                        mfr.m_sDescriptiveTitle = gedcomLine.LineItem;
                        gedcom.IncrementLineIndex(1);
                    }
                    bParsingFinished = false;
                }
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "BLOB")) != null  )
                {
                    if( mfr == null )
                        mfr = new CMultimediaFileReference( gedcom );

                    mfr.m_bEmbedded = true;

                    gedcom.IncrementLineIndex(1);

                    while( (gedcomLine = gedcom.GetLine(nLevel+2, "CONT")) != null )
                    {
                        blob.Append( gedcomLine.LineItem );
                        gedcom.IncrementLineIndex(1);
                    }
                    bParsingFinished = false;
                }
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "OBJE")) != null  )
                {
                    if( mfr == null )
                        mfr = new CMultimediaFileReference( gedcom );

                    mfr.m_xrefObj = gedcomLine.LinePointer;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }               
                // GEDCOM 5.5 style
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "FILE" )) != null )
                {
                    if( mfr == null )
                        mfr = new CMultimediaFileReference( gedcom );

                    mfr.m_sMultimediaFileReference = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                // GEDCOM 5.5.1 style
                else if( (mfr == null) && ((mfr = CMultimediaFileReference.Parse( gedcom, nLevel+1 )) != null) )
                {
                    bParsingFinished = false;
                }
                else if( (ns = CNoteStructure.Parse( gedcom, nLevel+1 )) != null )
                {
                    mr.m_alNoteStructures.Add( ns );
                    bParsingFinished = false;
                }
                else if( (sc = CSourceCitation.Parse( gedcom, nLevel+1 )) != null )
                {
                    mr.m_alSourceCitations.Add( sc );
                    bParsingFinished = false;
                }
                else if( (gedcomLine = gedcom.GetLine(nLevel+1, "TITL")) != null )
                {
                    if( mfr == null )
                        mfr = new CMultimediaFileReference( gedcom );

                    mfr.m_sDescriptiveTitle = gedcomLine.LineItem;
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

            if (mfr != null)
            {
                mr.m_alMultimediaFileReferences.Add(mfr);
            }

            // Family Historian style
            // If any of the notes contain only _ASID and _AREA, move these values to the m_asidPairs
            ArrayList notesToRemove = new ArrayList();
            foreach( CNoteStructure nss in mr.m_alNoteStructures )
            {
                if( nss is CNoteStructureInLine )
                {
                    if( ((CNoteStructureInLine)nss).m_sSubmitterText == null || ((CNoteStructureInLine)nss).m_sSubmitterText == "" )
                    {
                        if( ((CNoteStructureInLine)nss).m_sAsid != "" 
                            || ((CNoteStructureInLine)nss).m_sArea != "" )
                        {
                            string asid = ((CNoteStructureInLine)nss).m_sAsid;
                            CAsidPair asidPair = new CAsidPair( asid, ((CNoteStructureInLine)nss).m_sArea );
                            mr.m_htAsidPairs.Add( asid, asidPair );
                            notesToRemove.Add( nss );
                        }
                    }
                }
            }
            foreach( CNoteStructure nsss in notesToRemove )
            {
                mr.m_alNoteStructures.Remove( nsss );
            }

            if( blob.Length > 0 )
            {
                // Decode blob and overwrite sFilename with decoded blob sFilename
                try
                {
                    mfr.m_sMultimediaFileReference = gedcom.DecodeBlob( blob.ToString() );
                }
                catch( CBlobException )
                {
                    // Error decoding blob
                    mfr.m_sMultimediaFileReference = null;
                }
            }

            return mr;
        }
    } // End of class
} // End of namespace
