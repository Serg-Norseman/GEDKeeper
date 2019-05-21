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


        public new static GEDCOMTag Create(GEDCOMObject owner, string tagName, string tagValue)
        {
            return new GEDCOMAssociation(owner, tagName, tagValue);
        }

        public GEDCOMAssociation(GEDCOMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.ASSO);
            fSourceCitations = new GEDCOMList<GEDCOMSourceCitation>(this);
        }

        public GEDCOMAssociation(GEDCOMObject owner, string tagName, string tagValue) : this(owner)
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

        public override void SaveToStream(StreamWriter stream, int level)
        {
            base.SaveToStream(stream, level);

            level += 1;
            WriteTagLine(stream, level, GEDCOMTagType.RELA, fRelation, true);
            fSourceCitations.SaveToStream(stream, level);
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
