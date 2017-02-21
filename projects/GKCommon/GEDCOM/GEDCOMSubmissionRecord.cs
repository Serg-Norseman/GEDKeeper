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
    public sealed class GEDCOMSubmissionRecord : GEDCOMRecord
    {
        public string FamilyFileName
        {
            get { return GetTagStringValue("FAMF"); }
            set { SetTagStringValue("FAMF", value); }
        }

        public string TempleCode
        {
            get { return GetTagStringValue("TEMP"); }
            set { SetTagStringValue("TEMP", value); }
        }

        public int GenerationsOfAncestors
        {
            get { return GetTagIntegerValue("ANCE", 0); }
            set { SetTagIntegerValue("ANCE", value); }
        }

        public int GenerationsOfDescendants
        {
            get { return GetTagIntegerValue("DESC", 0); }
            set { SetTagIntegerValue("DESC", value); }
        }

        public GEDCOMOrdinanceProcessFlag OrdinanceProcessFlag
        {
            get { return GEDCOMUtils.GetOrdinanceProcessFlagVal(GetTagStringValue("ORDI")); }
            set { SetTagStringValue("ORDI", GEDCOMUtils.GetOrdinanceProcessFlagStr(value)); }
        }

        public GEDCOMPointer Submitter
        {
            get { return TagClass("SUBM", GEDCOMPointer.Create) as GEDCOMPointer; }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            SetRecordType(GEDCOMRecordType.rtSubmission);
            SetName("SUBN");
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "SUBM")
            {
                result = base.AddTag(tagName, tagValue, GEDCOMPointer.Create);
            }
            else
            {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public GEDCOMSubmissionRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMSubmissionRecord(owner, parent, tagName, tagValue);
        }
    }
}
