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
    public class GDMPointer : GDMTag, IGDMPointerHost
    {
        private string fXRef;


        public bool IsPointer
        {
            get { return (!string.IsNullOrEmpty(fXRef)); }
        }

        public string XRef
        {
            get { return fXRef; }
            set { fXRef = value; }
        }


        public GDMPointer()
        {
            fXRef = string.Empty;
        }

        public GDMPointer(int tagId) : base(tagId)
        {
            fXRef = string.Empty;
        }

        public GDMPointer(int tagId, string tagValue)
        {
            SetNameValue(tagId, tagValue);
        }

        public override void Clear()
        {
            base.Clear();
            fXRef = string.Empty;
        }

        public override bool IsEmpty()
        {
            return (base.IsEmpty() && string.IsNullOrEmpty(fXRef));
        }

        protected override string GetStringValue()
        {
            return GEDCOMUtils.EncloseXRef(fXRef);
        }

        public override string ParseString(string strValue)
        {
            // here XRef is a pure value without delimiters
            return GEDCOMUtils.ParseXRefPointer(strValue, out fXRef);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            if (map != null) {
                XRef = map.FindNewXRef(XRef);
            }
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fXRef);
        }
    }
}
