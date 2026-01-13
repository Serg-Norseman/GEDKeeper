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
    public sealed class GDMIndividualAttribute : GDMIndividualEventDetail
    {
        public GDMLines PhysicalDescription
        {
            get { return GEDCOMUtils.GetTagStrings(this); }
            set { GEDCOMUtils.SetTagStrings(this, value); }
        }


        public GDMIndividualAttribute()
        {
        }

        public GDMIndividualAttribute(int tagId, string tagValue)
        {
            SetNameValue(tagId, tagValue);
        }

        public GDMIndividualAttribute(int tagId, StringSpan tagValue)
        {
            SetNameValue(tagId, tagValue);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            //hashCode.AddObj(PhysicalDescription); <- inner Tags
        }
    }
}
