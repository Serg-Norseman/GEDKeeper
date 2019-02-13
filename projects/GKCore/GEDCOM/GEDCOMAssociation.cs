/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
    public sealed class GEDCOMAssociation : GEDCOMPointerWithNotes
    {
        private GEDCOMList<GEDCOMSourceCitation> fSourceCitations;

        public GEDCOMIndividualRecord Individual
        {
            get { return (Value as GEDCOMIndividualRecord); }
            set { Value = value; }
        }

        public string Relation
        {
            get { return GetTagStringValue("RELA"); }
            set { SetTagStringValue("RELA", value); }
        }

        public GEDCOMList<GEDCOMSourceCitation> SourceCitations
        {
            get { return fSourceCitations; }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            SetName("ASSO");
            fSourceCitations = new GEDCOMList<GEDCOMSourceCitation>(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fSourceCitations.Dispose();
            }
            base.Dispose(disposing);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;
            if (tagName == GEDCOMTagType.SOUR) {
                result = fSourceCitations.Add(new GEDCOMSourceCitation(Owner, this, tagName, tagValue));
            } else {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }
            return result;
        }

        public override void Clear()
        {
            base.Clear();
            fSourceCitations.Clear();
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && fSourceCitations.Count == 0;
        }

        public override void ReplaceXRefs(XRefReplacer map)
        {
            base.ReplaceXRefs(map);
            fSourceCitations.ReplaceXRefs(map);
        }

        public override void ResetOwner(GEDCOMTree newOwner)
        {
            base.ResetOwner(newOwner);
            fSourceCitations.ResetOwner(newOwner);
        }

        public GEDCOMAssociation(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMAssociation(owner, parent, tagName, tagValue);
        }
    }
}
