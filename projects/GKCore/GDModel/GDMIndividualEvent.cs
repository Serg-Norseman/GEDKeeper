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
