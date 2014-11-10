/* CBackReference.cs
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
    // Used to indicate the sType of record that the reference is for.
    public enum ERecordType
    {
        Individual,
        Family,
        Note
    }

    // Data structure used to refer from a record back to whatever references it
    public class CBackReference
    {
        // The record sType (individual, family, note)
        public ERecordType m_ertRecordType;

        // The xref of the record being referenced
        public string m_xref;

        // Stores the event sType that cites this source so that the event 
        // sType can be shown in the sources list box.
        public string m_sEventType;
        
        // Constructor
        public CBackReference( ERecordType recordType, string xrefId, string eventType )
        {
            m_ertRecordType = recordType;
            m_xref = xrefId;
            m_sEventType = eventType;
        }

        // Copy constructor
        public CBackReference( CBackReference other )
        {
            m_ertRecordType = other.m_ertRecordType;
            m_xref = other.m_xref;
            m_sEventType = other.m_sEventType;
        }
    }


}
