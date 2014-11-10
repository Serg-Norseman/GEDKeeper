/* CMultimediaLinkInLine.cs
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
    public class CMultimediaLinkInLine : GEDmill.LLClasses.CMultimediaLink
    {
        // GEDCOM data
        public ArrayList m_alMultimediaFileRefns;
        public ArrayList m_alNoteStructures;

        // Constructor
        public CMultimediaLinkInLine( CGedcom gedcom ) : base( gedcom )
        {
        }

        // CopyConstructor
        public override CMultimediaLink CopyConstructor()
        {
            CMultimediaLinkInLine ml = new CMultimediaLinkInLine( Gedcom );
            foreach( CMultimediaFileReference mfr in ml.m_alMultimediaFileRefns )
            {
                m_alMultimediaFileRefns.Add( new CMultimediaFileReference( mfr, false ) );
            }
            foreach( CNoteStructure ns in ml.m_alNoteStructures )
            {
                m_alNoteStructures.Add( ns.CopyConstructor() );
            }
            return ml;
        }

        // Parser
        public static new CMultimediaLinkInLine Parse( CGedcom gedcom, int level )
        {
            CGedcomLine gedcomLine;

            // Temporary holders for class members. Saves constructing a class early.
            CMultimediaFileReference mfr = null;
            ArrayList alMultimediaFileRefns = new ArrayList();
            CNoteStructure ns = null;
            ArrayList alNoteStructures = new ArrayList();
            string sDescriptiveTitle = "";
            string sMultimediaFormat = "";

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(level, "OBJE")) == null)
            {
                // Not one of us
                return null; 
            }

            // Parsing is going well enough to say that we definitely have one of these, 
            // so we can adjust the gedcom now.
            gedcom.IncrementLineIndex(1);

            bool bParsingFinished;
            do
            {
                bParsingFinished = true;

                // There may be one of these, standard specifies {0:M}
                CMultimediaFileReference local_mfr = CMultimediaFileReference.Parse( gedcom, level+1 );
                if( local_mfr != null )
                {
                    if( local_mfr != null )
                    {
                        mfr = local_mfr;
                    }
                    bParsingFinished = false;
                }
                // There may be one of these, standard specifies {0:M}
                else if( (ns = CNoteStructure.Parse( gedcom, level+1 )) != null )
                {
                    bParsingFinished = false;
                }
                else if( (gedcomLine = gedcom.GetLine(level+1, "TITL")) != null )
                {
                    sDescriptiveTitle = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }               
                else if( (gedcomLine = gedcom.GetLine(level+1, "FORM")) != null )
                {
                    sMultimediaFormat = gedcomLine.LineItem;
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }               
                else if( ( gedcomLine = gedcom.GetLine()).Level > level )
                {
                    LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, "Unknown tag :" );
                    LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, gedcomLine.ToString() );
                    gedcom.IncrementLineIndex(1);
                    bParsingFinished = false;
                }
                
                if( ns != null )
                {
                    alNoteStructures.Add( ns );
                    ns = null;
                }

            }
            while( !bParsingFinished );


            if( mfr != null )
            {
                if( sDescriptiveTitle.Length > 0 )
                {
                    mfr.m_sDescriptiveTitle = sDescriptiveTitle;
                }
                if( sMultimediaFormat.Length > 0 )
                {
                    mfr.m_sMultimediaFormat = sMultimediaFormat;
                }
                alMultimediaFileRefns.Add( mfr );
                mfr = null;
            }

            // Parsing went ok. Construct a new object and return it.
            CMultimediaLinkInLine ml = new CMultimediaLinkInLine( gedcom );
            ml.m_alMultimediaFileRefns = alMultimediaFileRefns;
            ml.m_alNoteStructures = alNoteStructures;
            return ml;
        }

        // Returns the list of actual files for this multimedia object
        public override ArrayList GetFileReferences()
        {
            return m_alMultimediaFileRefns;
        }
    }
}
