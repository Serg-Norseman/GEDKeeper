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
