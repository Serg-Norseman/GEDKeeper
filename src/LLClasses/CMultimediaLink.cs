/* CMultimediaLink.cs
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
    // Base class for the two times of multimedia links in GEDCOM (inline and xref)
    public abstract class CMultimediaLink : GEDmill.LLClasses.CParserObject
    {
        // Constructor
        public CMultimediaLink( CGedcom gedcom ) : base( gedcom )
        {
        }

        // Copy constructor
        public abstract CMultimediaLink CopyConstructor();

        // Parser
        public static CMultimediaLink Parse( CGedcom gedcom, int nLevel )
        {
            CGedcomLine gedcomLine;

            // There must be one of these, it defines the object.
            if ((gedcomLine = gedcom.GetLine(nLevel, "OBJE")) == null)
            {
                // Not one of us
                return null; 
            }

            // If bit after tag contains a pointer, it must be a reference sType of note
            if( gedcomLine.LinePointer != null ) //TODO
            {
                return CMultimediaLinkXref.Parse( gedcom, nLevel );
            }

            // Must be the inline version
            return CMultimediaLinkInLine.Parse( gedcom, nLevel );
        }

        // Returns the list of actual files for this multimedia object
        public abstract ArrayList GetFileReferences();
    }
}
