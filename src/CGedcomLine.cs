/* CGedcomLine.cs
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

namespace GEDmill
{
    // Data structure containing the data extracted from a line of a GEDCOM file
    public class CGedcomLine
    {
        // Constructor
        public CGedcomLine()
        {
        }

        // Constructor
        public CGedcomLine( int level, string xrefID, string tag, string lineItem, string linePointer, uint lineInFile )
        {
            this.m_nLevel = level;
            this.m_xref = xrefID;
            this.m_sTag = tag;
            this.m_sLineItem = lineItem;
            this.m_sLinePointer = linePointer;
            this.m_uLineInFile = lineInFile;
        }

        // The gedcomLine number of this gedcomLine in the original .ged file (may be different due to blank lines)
        private uint m_uLineInFile;

        // The nLevel number attached to the gedcomLine
        private int m_nLevel;
        
        // The tag value for the gedcomLine
        private string m_sTag;

        // The optional cross reference ID attached to the gedcomLine. null if not present.
        private string m_xref;

        // The optional gedcomLine item attached to the gedcomLine. null if not present.
        protected string m_sLineItem;

        // The optional pointer attached to the gedcomLine. null if not present.
        private string m_sLinePointer;

        // Accessor
        public int Level
        {
            get
            {
                return m_nLevel;
            }
            set
            {
                m_nLevel = value;
            }
        }

        // Accessor
        public string XrefID
        {
            get
            {
                return m_xref;
            }
            set
            {
                m_xref = value;
            }
        }

        // Accessor
        public string Tag
        {
            get
            {
                return m_sTag;
            }
            set
            {
                m_sTag = value;
            }
        }

        // Accessor
        public string LineItem
        {
            get
            {
                return m_sLineItem;
            }
            set
            {
                m_sLineItem = value;
            }
        }

        // Accessor
        public string LinePointer
        {
            get
            {
                return m_sLinePointer;
            }
            set
            {
                m_sLinePointer = value;
            }
        }

        public override string ToString()
        {
            return String.Format("({0}) {1} {2} {3} {4} {5}",
                m_uLineInFile.ToString(),
                m_nLevel.ToString(),
                m_xref,
                m_sTag,
                m_sLinePointer,
                m_sLineItem );
        }
    }
}
