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
    public sealed class GDMSpouseToFamilyLink : GDMPointerWithNotes
    {
        public GDMSpouseToFamilyLink()
        {
            SetName(GEDCOMTagType.FAMS);
        }

        public GDMSpouseToFamilyLink(string familyXRef)
        {
            SetName(GEDCOMTagType.FAMS);
            XRef = familyXRef;
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);
        }
    }
}
