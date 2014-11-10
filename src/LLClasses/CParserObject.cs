/* CParserObject.cs
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
    // Base class for all GEDCOM entities that can be parsed.
    public abstract class CParserObject
    {
        // The GEDCOM tag text
        private string m_sType;

        // The value associated with the tag
        private string m_sValue;

        // Reference back to the GEDCOM parser
        private CGedcom m_gedcom;

        // Constructor
        public CParserObject( CGedcom gedcom )
        {
            m_gedcom = gedcom;
        }

        // Accessor
        public string Type
        {
            get
            {
                return m_sType;
            }
            set
            {
                m_sType = value;
            }
        }

        // Accessor
        public string Value
        {
            get 
            { 
                return m_sValue; 
            }
            set 
            { 
                m_sValue = value; 
            }
        }

        // Accessor
        public CGedcom Gedcom
        {
            get 
            { 
                return m_gedcom; 
            }
            set 
            { 
                m_gedcom = value; 
            }
        }


    }
}
