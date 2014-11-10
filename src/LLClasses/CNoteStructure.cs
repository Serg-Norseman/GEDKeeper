/* CNoteStructure.cs
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
    // GEDCOM note structure
    public abstract class CNoteStructure : GEDmill.LLClasses.CParserObject
    {
        // GEDCOM source citations
        public ArrayList m_alSourceCitations;

        // Constructor
        public CNoteStructure( CGedcom gedcom ) : base( gedcom )
        {
        }

        // Copy constructor
        public abstract CNoteStructure CopyConstructor();

        // Parser
        public static CNoteStructure Parse( CGedcom gedcom, int level )
        {
            CGedcomLine line;

            // There must be one of these, it defines the object.
            if ((line = gedcom.GetLine(level, "NOTE")) == null)
            {
                return null; // Not one of us
            }

            // If bit after tag contains a pointer, it must be a reference sType of note
            if( line.LinePointer != null ) //TODO
            {
                return CNoteStructureXref.Parse( gedcom, level );
            }

            // Must be the inline version
            return CNoteStructureInLine.Parse( gedcom, level );
        }

        // Returns the text of the note.
        public abstract string GetText();

        // Returns the text of the note.
        public string Text
        {
            get
            {
                return GetText();
            }
        }
    }
}
