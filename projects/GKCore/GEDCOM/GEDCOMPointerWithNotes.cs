/*
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

namespace GKCommon.GEDCOM
{
    public class GEDCOMPointerWithNotes : GEDCOMPointer
    {
        private GEDCOMList<GEDCOMNotes> fNotes;

        public GEDCOMList<GEDCOMNotes> Notes
        {
            get { return fNotes; }
        }


        public GEDCOMPointerWithNotes(GEDCOMTree owner, GEDCOMObject parent) : base(owner, parent)
        {
            fNotes = new GEDCOMList<GEDCOMNotes>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fNotes.Dispose();
            }
            base.Dispose(disposing);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == GEDCOMTagType.NOTE) {
                result = fNotes.Add(new GEDCOMNotes(Owner, this, tagName, tagValue));
            } else {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public override void Clear()
        {
            base.Clear();
            fNotes.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && Count == 0 && fNotes.Count == 0;
        }

        public override void ReplaceXRefs(XRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fNotes.ReplaceXRefs(map);
        }

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);
            fNotes.ResetOwner(newOwner);
        }
    }
}
