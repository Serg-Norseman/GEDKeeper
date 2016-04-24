/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

using System.IO;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMEventDetail : GEDCOMTagWithLists
    {
        public string Classification
        {
            get { return base.GetTagStringValue("TYPE"); }
            set { base.SetTagStringValue("TYPE", value); }
        }

        public string Agency
        {
            get { return base.GetTagStringValue("AGNC"); }
            set { base.SetTagStringValue("AGNC", value); }
        }

        public string ReligiousAffilation
        {
            get { return base.GetTagStringValue("RELI"); }
            set { base.SetTagStringValue("RELI", value); }
        }

        public string Cause
        {
            get { return base.GetTagStringValue("CAUS"); }
            set { base.SetTagStringValue("CAUS", value); }
        }

        public GEDCOMPlace Place
        {
            get { return base.TagClass("PLAC", GEDCOMPlace.Create) as GEDCOMPlace; }
        }

        public GEDCOMAddress Address
        {
            get { return base.TagClass("ADDR", GEDCOMAddress.Create) as GEDCOMAddress; }
        }

        public GEDCOMDateValue Date
        {
            get { return base.TagClass("DATE", GEDCOMDateValue.Create) as GEDCOMDateValue; }
        }

        public GEDCOMRestriction Restriction
        {
            get { return GEDCOMUtils.GetRestrictionVal(base.GetTagStringValue("RESN")); }
            set { base.SetTagStringValue("RESN", GEDCOMUtils.GetRestrictionStr(value)); }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            base.SetLevel(((GEDCOMTag)parent).Level);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "PHON" || tagName == "EMAIL" || tagName == "FAX" || tagName == "WWW")
            {
                result = this.Address.AddTag(tagName, tagValue, tagConstructor);
            }
            else
            {
                // define "PLAC", "ADDR", "DATE" by default
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public override void SaveToStream(StreamWriter stream)
        {
            this.SaveTagsToStream(stream);

            this.fNotes.SaveToStream(stream);
            this.fSourceCitations.SaveToStream(stream);
            this.fMultimediaLinks.SaveToStream(stream);
        }

        public GEDCOMEventDetail(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }
    }
}
