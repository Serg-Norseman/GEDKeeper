/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public abstract class GDMIndividualEventDetail : GDMCustomEvent
    {
        private GDMAge fAge;

        public bool HasAge
        {
            get { return fAge != null && !fAge.IsEmpty(); }
        }

        public GDMAge Age
        {
            get {
                if (fAge == null) {
                    fAge = new GDMAge();
                }
                return fAge;
            }
        }


        protected GDMIndividualEventDetail()
        {
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fAge != null) fAge.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void Assign(GDMTag source)
        {
            var sourceObj = (source as GDMIndividualEventDetail);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);

            if (sourceObj.fAge != null) Age.Assign(sourceObj.fAge);
        }

        public override void Clear()
        {
            base.Clear();

            if (fAge != null) fAge.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (fAge == null || fAge.IsEmpty());
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fAge);
        }
    }


    public sealed class GDMIndividualEvent : GDMIndividualEventDetail
    {
        public GDMIndividualEvent()
        {
        }

        public GDMIndividualEvent(int tagId, string tagValue)
        {
            SetNameValue(tagId, tagValue);
        }

        public GDMIndividualEvent(int tagId, StringSpan tagValue)
        {
            SetNameValue(tagId, tagValue);
        }
    }
}
