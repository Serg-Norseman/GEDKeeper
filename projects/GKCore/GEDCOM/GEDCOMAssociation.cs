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

using System;
using System.IO;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMAssociation : GEDCOMPointerWithNotes
    {
        private string fRelation;
        private GEDCOMList<GEDCOMSourceCitation> fSourceCitations;

        public GEDCOMIndividualRecord Individual
        {
            get { return (Value as GEDCOMIndividualRecord); }
            set { Value = value; }
        }

        public string Relation
        {
            get { return fRelation; }
            set { fRelation = value; }
        }

        public GEDCOMList<GEDCOMSourceCitation> SourceCitations
        {
            get { return fSourceCitations; }
        }


        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMAssociation(owner, parent, tagName, tagValue);
        }

        public GEDCOMAssociation(GEDCOMTree owner, GEDCOMObject parent) : base(owner, parent)
        {
            SetName(GEDCOMTagType.ASSO);
            fSourceCitations = new GEDCOMList<GEDCOMSourceCitation>(this);
        }

        public GEDCOMAssociation(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : this(owner, parent)
        {
            SetNameValue(tagName, tagValue);
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
            if (tagName == GEDCOMTagType.RELA) {
                fRelation = tagValue;
                result = null;
            } else if (tagName == GEDCOMTagType.SOUR) {
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
            fRelation = string.Empty;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && string.IsNullOrEmpty(fRelation) && (fSourceCitations.Count == 0);
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

        public override void SaveToStream(StreamWriter stream)
        {
            base.SaveToStream(stream);
            WriteTagLine(stream, Level + 1, GEDCOMTagType.RELA, fRelation, true);
            fSourceCitations.SaveToStream(stream);
        }

        public override void Assign(GEDCOMTag source)
        {
            GEDCOMAssociation srcAsso = (source as GEDCOMAssociation);
            if (srcAsso == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            fRelation = srcAsso.fRelation;
        }
    }
}
