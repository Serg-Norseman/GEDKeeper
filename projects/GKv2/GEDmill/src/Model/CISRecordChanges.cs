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

using System.Collections.Generic;
using GDModel;

namespace GEDmill.Model
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
        public GDMFileReferenceWithTitle CurrentMFR;

        // The multimedia file references
        public List<GDMFileReferenceWithTitle> MFRList;

        public bool Visibility;


        public CISRecordChanges(bool includeInWebsite)
        {
            IncludeInWebsite = includeInWebsite;
            MFRList = new List<GDMFileReferenceWithTitle>();
            CurrentMFR = null;
            Visibility = true;
        }
    }
}
