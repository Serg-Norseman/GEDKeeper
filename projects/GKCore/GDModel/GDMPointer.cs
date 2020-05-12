﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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

using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public class GDMPointer : GDMTag
    {
        private string fXRef;


        public bool IsPointer
        {
            get { return (!string.IsNullOrEmpty(fXRef)); }
        }

        // TODO: need to do a protection test on the proper records (with XRef)
        public GDMRecord Value
        {
            get {
                GDMTree tree = GetTree();
                return (tree == null) ? null : tree.XRefIndex_Find(XRef);
            }
            set {
                fXRef = string.Empty;
                if (value == null) return;

                string xrf = value.XRef;
                if (string.IsNullOrEmpty(xrf))
                {
                    xrf = value.NewXRef();
                }
                XRef = xrf;
            }
        }

        public string XRef
        {
            get { return fXRef; }
            set { fXRef = value; }
        }


        public new static GDMTag Create(GDMObject owner, int tagId, string tagValue)
        {
            return new GDMPointer(owner, tagId, tagValue);
        }

        public GDMPointer(GDMObject owner) : base(owner)
        {
            fXRef = string.Empty;
        }

        public GDMPointer(GDMObject owner, int tagId, string tagValue) : this(owner)
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
            return GEDCOMUtils.ParseXRefPointer(strValue, out fXRef);
        }

        public override void ReplaceXRefs(GDMXRefReplacer map)
        {
            base.ReplaceXRefs(map);
            XRef = map.FindNewXRef(XRef);
        }
    }
}
