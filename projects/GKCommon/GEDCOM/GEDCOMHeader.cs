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

using System;

namespace GKCommon.GEDCOM
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GEDCOMHeader : GEDCOMCustomRecord
    {

        public GEDCOMCharacterSet CharacterSet
        {
            get { return GEDCOMUtils.GetCharacterSetVal(base.GetTagStringValue("CHAR")); }
            set { base.SetTagStringValue("CHAR", GEDCOMUtils.GetCharacterSetStr(value)); }
        }

        public StringList Notes
        {
            get { return base.GetTagStrings(base.FindTag("NOTE", 0)); }
            set { base.SetTagStrings(base.TagClass("NOTE", GEDCOMNotes.Create), value); }
        }

        public string Source
        {
            get { return base.GetTagStringValue("SOUR"); }
            set { base.SetTagStringValue("SOUR", value); }
        }

        public string SourceVersion
        {
            get { return base.GetTagStringValue(@"SOUR\VERS"); }
            set { base.SetTagStringValue(@"SOUR\VERS", value); }
        }

        public string SourceProductName
        {
            get { return base.GetTagStringValue(@"SOUR\NAME"); }
            set { base.SetTagStringValue(@"SOUR\NAME", value); }
        }

        public string SourceBusinessName
        {
            get { return base.GetTagStringValue(@"SOUR\CORP"); }
            set { base.SetTagStringValue(@"SOUR\CORP", value); }
        }

        public GEDCOMAddress SourceBusinessAddress
        {
            get {
                GEDCOMTag corpTag = base.TagClass(@"SOUR\CORP", GEDCOMTag.Create);
                return corpTag.TagClass("ADDR", GEDCOMAddress.Create) as GEDCOMAddress;
            }
        }

        public string ReceivingSystemName
        {
            get { return base.GetTagStringValue("DEST"); }
            set { base.SetTagStringValue("DEST", value); }
        }

        public string FileName
        {
            get { return base.GetTagStringValue("FILE"); }
            set { base.SetTagStringValue("FILE", value); }
        }

        public string Copyright
        {
            get { return base.GetTagStringValue("COPR"); }
            set { base.SetTagStringValue("COPR", value); }
        }

        public string GEDCOMVersion
        {
            get { return base.GetTagStringValue(@"GEDC\VERS"); }
            set { base.SetTagStringValue(@"GEDC\VERS", value); }
        }

        public string GEDCOMForm
        {
            get { return base.GetTagStringValue(@"GEDC\FORM"); }
            set { base.SetTagStringValue(@"GEDC\FORM", value); }
        }

        public string CharacterSetVersion
        {
            get { return base.GetTagStringValue(@"CHAR\VERS"); }
            set { base.SetTagStringValue(@"CHAR\VERS", value); }
        }

        public string Language
        {
            get { return base.GetTagStringValue("LANG"); }
            set { base.SetTagStringValue("LANG", value); }
        }

        public string PlaceHierarchy
        {
            get { return base.GetTagStringValue(@"PLAC\FORM"); }
            set { base.SetTagStringValue(@"PLAC\FORM", value); }
        }

        public GEDCOMPointer Submission
        {
            get { return base.TagClass("SUBN", GEDCOMPointer.Create) as GEDCOMPointer; }
        }

        public GEDCOMPointer Submitter
        {
            get { return base.TagClass("SUBM", GEDCOMPointer.Create) as GEDCOMPointer; }
        }

        public GEDCOMDateExact TransmissionDate
        {
            get { return base.TagClass("DATE", GEDCOMDateExact.Create) as GEDCOMDateExact; }
        }

        public GEDCOMTime TransmissionTime
        {
            get { return this.TransmissionDate.TagClass("TIME", GEDCOMTime.Create) as GEDCOMTime; }
        }

        public DateTime TransmissionDateTime
        {
            get {
                return this.TransmissionDate.Date.Add(this.TransmissionTime.Value);
            }
            set {
                this.TransmissionDate.Date = value.Date;
                this.TransmissionTime.Value = value.TimeOfDay;
            }
        }

        // new property (not standard)
        public int FileRevision
        {
            get { return base.GetTagIntegerValue(@"FILE\_REV", 0); }
            set { base.SetTagIntegerValue(@"FILE\_REV", value); }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            this.SetName("HEAD");
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "DATE")
            {
                result = base.AddTag(tagName, tagValue, GEDCOMDateExact.Create);
            }
            else if (tagName == "SUBM")
            {
                result = base.AddTag(tagName, tagValue, GEDCOMPointer.Create);
            }
            else if (tagName == "SUBN")
            {
                result = base.AddTag(tagName, tagValue, GEDCOMPointer.Create);
            }
            else
            {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }

        public GEDCOMHeader(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }
    }
}
