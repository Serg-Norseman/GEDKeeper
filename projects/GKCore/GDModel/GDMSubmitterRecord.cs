/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public sealed class GDMSubmitterRecord : GDMRecord
    {
        private readonly GDMAddress fAddress;
        private readonly GDMList<GDMLanguage> fLanguages;
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
