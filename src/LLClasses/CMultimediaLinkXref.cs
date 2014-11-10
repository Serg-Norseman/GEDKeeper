/* CMultimediaLinkXref.cs
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
    public class CMultimediaLinkXref : GEDmill.LLClasses.CMultimediaLink
    {
        // Object id
        public string m_xref;

        // ASID (Family Historian)
        public string m_sAsid;

        // Constructor
        public CMultimediaLinkXref( CGedcom gedcom ) : base( gedcom )
        {
        }

        // Copy constructor
        public override CMultimediaLink CopyConstructor()
        {
            CMultimediaLinkXref ml = new CMultimediaLinkXref( Gedcom );
            ml.m_xref = m_xref;
            ml.m_sAsid = m_sAsid;
            return ml;
        }

        // Parser
        public static new CMultimediaLinkXref Parse( CGedcom gedcom, int level )
        {
            CGedcomLine line;

            // Temporary holders for class members. Saves constructing a class early.
            string xref;
            string asid = "";

            // There must be one of these, it defines the object.
            if ((line = gedcom.GetLine(level, "OBJE")) == null)
            {
                // Not one of us
                return null;
            }
            xref = line.LinePointer;

            // Parsing is going well enough to say that we definitely have one of these, 
            // so we can adjust the gedcom now.
            gedcom.IncrementLineIndex(1);
            
            if( (line = gedcom.GetLine(level+1, "_ASID")) != null )
            {
                asid = line.LineItem;
                gedcom.IncrementLineIndex(1);
            }   
            else if( ( line = gedcom.GetLine()).Level > level )
            {
                LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, "Unknown tag :" );
                LogFile.TheLogFile.WriteLine( LogFile.DT_GEDCOM, LogFile.EDebugLevel.Warning, line.ToString() );
                gedcom.IncrementLineIndex(1);
            }

            // Parsing went ok. Construct a new object and return it.
            CMultimediaLinkXref ml = new CMultimediaLinkXref( gedcom );
            ml.m_xref = xref;
            ml.m_sAsid = asid;
            return ml;
        }

        // Returns the list of actual files for this multimedia object
        public override ArrayList GetFileReferences()
        {
            CMultimediaRecord mr = Gedcom.GetMultimediaRecord( m_xref );
            
            if( mr == null )
                return null;

            return mr.m_alMultimediaFileReferences;
        }
    }
}
