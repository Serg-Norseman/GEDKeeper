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
    public class GDMIndividualLink : GDMPointer
    {
        public GDMIndividualLink()
        {
        }

        public GDMIndividualLink(int tagId) : base(tagId)
        {
        }

        public GDMIndividualLink(int tagId, string tagValue)
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

        public GDMChildLink(int tagId, string tagValue)
        {
            SetNameValue(tagId, tagValue);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);
        }
    }
}
