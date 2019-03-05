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
using BSLib;

namespace GKCommon.GEDCOM
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GEDCOMHeader : GEDCOMCustomRecord
    {

        public GEDCOMCharacterSet CharacterSet
        {
            get { return GEDCOMUtils.GetCharacterSetVal(GetTagStringValue("CHAR")); }
            set { SetTagStringValue("CHAR", GEDCOMUtils.GetCharacterSetStr(value)); }
        }

        public StringList Notes
        {
            get { return GetTagStrings(FindTag(GEDCOMTagType.NOTE, 0)); }
            set { SetTagStrings(TagClass(GEDCOMTagType.NOTE, GEDCOMNotes.Create), value); }
        }

        public string Source
        {
            get { return GetTagStringValue(GEDCOMTagType.SOUR); }
            set { SetTagStringValue(GEDCOMTagType.SOUR, value); }
        }

        public string SourceVersion
        {
            get { return GetTagStringValue(@"SOUR\VERS"); }
            set { SetTagStringValue(@"SOUR\VERS", value); }
        }

        public string SourceProductName
        {
            get { return GetTagStringValue(@"SOUR\NAME"); }
            set { SetTagStringValue(@"SOUR\NAME", value); }
        }

        public string SourceBusinessName
        {
            get { return GetTagStringValue(@"SOUR\CORP"); }
            set { SetTagStringValue(@"SOUR\CORP", value); }
        }

        public GEDCOMAddress SourceBusinessAddress
        {
            get {
                GEDCOMTag corpTag = TagClass(@"SOUR\CORP", Create);
                return corpTag.TagClass(GEDCOMTagType.ADDR, GEDCOMAddress.Create) as GEDCOMAddress;
            }
        }

        public string ReceivingSystemName
        {
            get { return GetTagStringValue("DEST"); }
            set { SetTagStringValue("DEST", value); }
        }

        public string FileName
        {
            get { return GetTagStringValue(GEDCOMTagType.FILE); }
            set { SetTagStringValue(GEDCOMTagType.FILE, value); }
        }

        public string Copyright
        {
            get { return GetTagStringValue("COPR"); }
            set { SetTagStringValue("COPR", value); }
        }

        public string GEDCOMVersion
        {
            get { return GetTagStringValue(@"GEDC\VERS"); }
            set { SetTagStringValue(@"GEDC\VERS", value); }
        }

        public string GEDCOMForm
        {
            get { return GetTagStringValue(@"GEDC\FORM"); }
            set { SetTagStringValue(@"GEDC\FORM", value); }
        }

        public string CharacterSetVersion
        {
            get { return GetTagStringValue(@"CHAR\VERS"); }
            set { SetTagStringValue(@"CHAR\VERS", value); }
        }

        public GEDCOMLanguage Language
        {
            get { return TagClass(GEDCOMTagType.LANG, GEDCOMLanguage.Create) as GEDCOMLanguage; }
        }

        public string PlaceHierarchy
        {
            get { return GetTagStringValue(@"PLAC\FORM"); }
            set { SetTagStringValue(@"PLAC\FORM", value); }
        }

        public GEDCOMPointer Submission
        {
            get { return TagClass(GEDCOMTagType.SUBN, GEDCOMPointer.Create) as GEDCOMPointer; }
        }

        public GEDCOMPointer Submitter
        {
            get { return TagClass(GEDCOMTagType.SUBM, GEDCOMPointer.Create) as GEDCOMPointer; }
        }

        public GEDCOMDate TransmissionDate
        {
            get { return TagClass("DATE", GEDCOMDate.Create) as GEDCOMDate; }
        }

        public GEDCOMTime TransmissionTime
        {
            get { return TransmissionDate.TagClass("TIME", GEDCOMTime.Create) as GEDCOMTime; }
        }

        public DateTime TransmissionDateTime
        {
            get {
                return TransmissionDate.Date.Add(TransmissionTime.Value);
            }
            set {
                TransmissionDate.Date = value.Date;
                TransmissionTime.Value = value.TimeOfDay;
            }
        }

        // new property (not standard)
        public int FileRevision
        {
            get { return GetTagIntegerValue(@"FILE\_REV", 0); }
            set { SetTagIntegerValue(@"FILE\_REV", value); }
        }


        public GEDCOMHeader(GEDCOMTree owner, GEDCOMObject parent) : base(owner, parent)
        {
            SetName(GEDCOMTagType.HEAD);
        }

        public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag result;

            if (tagName == "DATE") {
                result = base.AddTag(tagName, tagValue, GEDCOMDate.Create);
            } else if (tagName == GEDCOMTagType.SUBM) {
                result = base.AddTag(tagName, tagValue, GEDCOMPointer.Create);
            } else if (tagName == GEDCOMTagType.SUBN) {
                result = base.AddTag(tagName, tagValue, GEDCOMPointer.Create);
            } else {
                result = base.AddTag(tagName, tagValue, tagConstructor);
            }

            return result;
        }
    }
}
