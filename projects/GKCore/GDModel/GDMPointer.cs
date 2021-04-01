/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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

        public T GetPtrValue<T>() where T : GDMRecord
        {
            GDMTree tree = GetTree();
            return (tree == null) ? null : tree.XRefIndex_Find(XRef) as T;
        }

        // TODO: how to be sure that the record will have the correct XRef in the required places?
        public string XRef
        {
            get { return fXRef; }
            set { fXRef = value; }
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
            if (map != null) {
                XRef = map.FindNewXRef(XRef);
            }
        }

        private GDMTree GetTree()
        {
            GDMTree owner = null;

            GDMTag current = this;
            while (current != null) {
                GDMObject parent = current.Owner;

                var parentTag = parent as GDMTag;
                if (parentTag != null) {
                    current = parentTag;
                } else {
                    var parentTree = parent as GDMTree;
                    if (parentTree != null) {
                        owner = parentTree;
                    }
                    break;
                }
            }

            return owner;
        }
    }
}
