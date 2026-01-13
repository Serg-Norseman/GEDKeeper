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
    public class GDMIndividualLink : GDMPointer
    {
        public GDMIndividualLink()
        {
        }

        public GDMIndividualLink(int tagId) : base(tagId)
        {
        }

        public GDMIndividualLink(int tagId, StringSpan tagValue)
        {
            SetNameValue(tagId, tagValue);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);
        }
    }


    public sealed class GDMChildLink : GDMIndividualLink
    {
        public GDMChildLink()
        {
            SetName(GEDCOMTagType.CHIL);
        }

        public GDMChildLink(string childXRef)
        {
            SetName(GEDCOMTagType.CHIL);
            XRef = childXRef;
        }

        public GDMChildLink(int tagId, StringSpan tagValue)
        {
            SetNameValue(tagId, tagValue);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);
        }
    }
}
