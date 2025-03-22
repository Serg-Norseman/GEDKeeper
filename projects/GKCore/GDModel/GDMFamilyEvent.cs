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

namespace GDModel
{
    public sealed class GDMFamilyEvent : GDMCustomEvent
    {
        private string fHusbandAge;
        private string fWifeAge;

        public string HusbandAge
        {
            get { return fHusbandAge; }
            set { fHusbandAge = value; }
        }

        public string WifeAge
        {
            get { return fWifeAge; }
            set { fWifeAge = value; }
        }


        public GDMFamilyEvent()
        {
        }

        public GDMFamilyEvent(int tagId, string tagValue)
        {
            SetNameValue(tagId, tagValue);
        }

        public override void Assign(GDMTag source)
        {
            GDMFamilyEvent sourceObj = (source as GDMFamilyEvent);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);
            fHusbandAge = sourceObj.fHusbandAge;
            fWifeAge = sourceObj.fWifeAge;
        }

        public override void Clear()
        {
            base.Clear();
            fHusbandAge = string.Empty;
            fWifeAge = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(fHusbandAge) && string.IsNullOrEmpty(fWifeAge);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);
            hashCode.Add(fHusbandAge);
            hashCode.Add(fWifeAge);
        }
    }
}
