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
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    // TODO: remove and work with string idents
    public enum GEDCOMCharacterSet
    {
        csASCII,
        csANSEL,
        csUNICODE,
        csUTF8
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class GDMHeader : GDMTag
    {
        public GEDCOMCharacterSet CharacterSet
        {
            get { return GEDCOMUtils.GetCharacterSetVal(GetTagStringValue(GEDCOMTagType.CHAR)); }
            set { SetTagStringValue(GEDCOMTagType.CHAR, GEDCOMUtils.GetCharacterSetStr(value)); }
        }

        public StringList Notes
        {
            get { return GetTagStrings(FindTag(GEDCOMTagType.NOTE, 0)); }
            set { SetTagStrings(GetTag<GDMTag>(GEDCOMTagType.NOTE, GDMNotes.Create), value); }
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

        public GDMAddress SourceBusinessAddress
        {
            get {
                GDMTag corpTag = GetTag<GDMTag>(@"SOUR\CORP", Create);
                return corpTag.GetTag<GDMAddress>(GEDCOMTagType.ADDR, GDMAddress.Create);
            }
        }

        public string ReceivingSystemName
        {
            get { return GetTagStringValue(GEDCOMTagType.DEST); }
            set { SetTagStringValue(GEDCOMTagType.DEST, value); }
        }

        public string FileName
        {
            get { return GetTagStringValue(GEDCOMTagType.FILE); }
            set { SetTagStringValue(GEDCOMTagType.FILE, value); }
        }

        public string Copyright
        {
            get { return GetTagStringValue(GEDCOMTagType.COPR); }
            set { SetTagStringValue(GEDCOMTagType.COPR, value); }
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

        public GDMLanguage Language
        {
            get { return GetTag<GDMLanguage>(GEDCOMTagType.LANG, GDMLanguage.Create); }
        }

        public string PlaceHierarchy
        {
            get { return GetTagStringValue(@"PLAC\FORM"); }
            set { SetTagStringValue(@"PLAC\FORM", value); }
        }

        public GDMPointer Submission
        {
            get { return GetTag<GDMPointer>(GEDCOMTagType.SUBN, GDMPointer.Create); }
        }

        public GDMPointer Submitter
        {
            get { return GetTag<GDMPointer>(GEDCOMTagType.SUBM, GDMPointer.Create); }
        }

        public GDMDate TransmissionDate
        {
            get { return GetTag<GDMDate>(GEDCOMTagType.DATE, GDMDate.Create); }
        }

        public GDMTime TransmissionTime
        {
            get { return TransmissionDate.GetTag<GDMTime>(GEDCOMTagType.TIME, GDMTime.Create); }
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


        public GDMHeader(GDMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.HEAD);
        }
    }
}
