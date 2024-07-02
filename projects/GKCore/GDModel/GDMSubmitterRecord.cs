/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
    public sealed class GDMSubmitterRecord : GDMRecord
    {
        private GDMAddress fAddress;
        private GDMList<GDMLanguage> fLanguages;
        private string fName;
        private string fRegisteredReference;


        public GDMAddress Address
        {
            get { return fAddress; }
        }

        public GDMList<GDMLanguage> Languages
        {
            get { return fLanguages; }
        }

        public string Name
        {
            get { return fName; }
            set { fName = value; }
        }

        public string RegisteredReference
        {
            get { return fRegisteredReference; }
            set { fRegisteredReference = value; }
        }


        public GDMSubmitterRecord(GDMTree tree) : base(tree)
        {
            SetName(GEDCOMTagType.SUBM);

            fAddress = new GDMAddress();
            fLanguages = new GDMList<GDMLanguage>();
            fName = string.Empty;
            fRegisteredReference = string.Empty;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fLanguages.Dispose();
            }
            base.Dispose(disposing);
        }

        internal override void TrimExcess()
        {
            base.TrimExcess();

            fAddress.TrimExcess();
            fLanguages.TrimExcess();
        }

        public override void Clear()
        {
            base.Clear();

            fAddress.Clear();
            fLanguages.Clear();
            fName = string.Empty;
            fRegisteredReference = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fAddress.IsEmpty() && (fLanguages.Count == 0) && string.IsNullOrEmpty(fName) &&
                string.IsNullOrEmpty(fRegisteredReference);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);

            fAddress.ReplaceXRefs(map);
            fLanguages.ReplaceXRefs(map);
        }

        public GDMLanguage AddLanguage(GDMLanguage value)
        {
            fLanguages.Add(value);
            return value;
        }

        public void SetLanguage(int index, string value)
        {
            if (index < 0) return;

            while (index >= fLanguages.Count) {
                fLanguages.Add(new GDMLanguage());
            }
            fLanguages[index].StringValue = value;
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fAddress);
            ProcessHashes(ref hashCode, fLanguages);
            hashCode.Add(fName);
            hashCode.Add(fRegisteredReference);
        }
    }
}
