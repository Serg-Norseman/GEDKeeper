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
    public sealed class GDMFamilyEvent : GDMCustomEvent
    {
        private GDMAge fHusbandAge;
        private GDMAge fWifeAge;

        public bool HasHusbandAge
        {
            get { return fHusbandAge != null && !fHusbandAge.IsEmpty(); }
        }

        public GDMAge HusbandAge
        {
            get {
                if (fHusbandAge == null) {
                    fHusbandAge = new GDMAge();
                }
                return fHusbandAge;
            }
        }

        public bool HasWifeAge
        {
            get { return fWifeAge != null && !fWifeAge.IsEmpty(); }
        }

        public GDMAge WifeAge
        {
            get {
                if (fWifeAge == null) {
                    fWifeAge = new GDMAge();
                }
                return fWifeAge;
            }
        }


        public GDMFamilyEvent()
        {
        }

        public GDMFamilyEvent(int tagId, string tagValue)
        {
            SetNameValue(tagId, tagValue);
        }

        public GDMFamilyEvent(int tagId, StringSpan tagValue)
        {
            SetNameValue(tagId, tagValue);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fHusbandAge != null) fHusbandAge.Dispose();
                if (fWifeAge != null) fWifeAge.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void Assign(GDMTag source)
        {
            GDMFamilyEvent sourceObj = (source as GDMFamilyEvent);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);
            if (sourceObj.fHusbandAge != null) HusbandAge.Assign(sourceObj.fHusbandAge);
            if (sourceObj.fWifeAge != null) WifeAge.Assign(sourceObj.fWifeAge);
        }

        public override void Clear()
        {
            base.Clear();

            if (fHusbandAge != null) fHusbandAge.Clear();
            if (fWifeAge != null) fWifeAge.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (fHusbandAge == null || fHusbandAge.IsEmpty()) && (fWifeAge == null || fWifeAge.IsEmpty());
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fHusbandAge);
            hashCode.Add(fWifeAge);
        }
    }
}
