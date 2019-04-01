/* CISRecordChanges.cs
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

using System.Collections.Generic;
using GKCommon.GEDCOM;

namespace GEDmill
{
    /// <summary>
    /// Data structure to hold a record for the load/save changes option on the prune
    /// individuals and sources page. See also CISRecord.
    /// </summary>
    public class CISRecordChanges
    {
        // True if this record is to be included (e.g. individual's checkbox is checked)
        public bool IncludeInWebsite;

        // Helper for parser
        public GEDCOMFileReferenceWithTitle CurrentMFR;

        // The multimedia file references
        public List<GEDCOMFileReferenceWithTitle> MFRList;

        public bool Visibility;


        public CISRecordChanges(bool includeInWebsite)
        {
            IncludeInWebsite = includeInWebsite;
            MFRList = new List<GEDCOMFileReferenceWithTitle>();
            CurrentMFR = null;
            Visibility = true;
        }
    }
}
