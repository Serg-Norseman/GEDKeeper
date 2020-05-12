/* 
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
 */

using System;
using GDModel;

namespace GEDmill.Model
{
    /// <summary>
    /// Data structure used to refer from a record back to whatever references it
    /// </summary>
    public class BackReference
    {
        // The record sType (individual, family, note)
        public GDMRecordType RecordType;

        // The xref of the record being referenced
        public string XRef;

        // Stores the event sType that cites this source so that the event 
        // sType can be shown in the sources list box.
        public string EventType;
        

        public BackReference(GDMRecordType recordType, string xref, string eventType)
        {
            RecordType = recordType;
            XRef = xref;
            EventType = eventType;
        }

        public BackReference(BackReference other)
        {
            RecordType = other.RecordType;
            XRef = other.XRef;
            EventType = other.EventType;
        }
    }
}
