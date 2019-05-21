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
    public sealed class GEDCOMSubmissionRecord : GEDCOMRecord
    {
        public string FamilyFileName
        {
            get { return GetTagStringValue(GEDCOMTagType.FAMF); }
            set { SetTagStringValue(GEDCOMTagType.FAMF, value); }
        }

        public string TempleCode
        {
            get { return GetTagStringValue(GEDCOMTagType.TEMP); }
            set { SetTagStringValue(GEDCOMTagType.TEMP, value); }
        }

        public int GenerationsOfAncestors
        {
            get { return GetTagIntegerValue(GEDCOMTagType.ANCE, 0); }
            set { SetTagIntegerValue(GEDCOMTagType.ANCE, value); }
        }

        public int GenerationsOfDescendants
        {
            get { return GetTagIntegerValue(GEDCOMTagType.DESC, 0); }
            set { SetTagIntegerValue(GEDCOMTagType.DESC, value); }
        }

        public GEDCOMOrdinanceProcessFlag OrdinanceProcessFlag
        {
            get { return GEDCOMUtils.GetOrdinanceProcessFlagVal(GetTagStringValue(GEDCOMTagType.ORDI)); }
            set { SetTagStringValue(GEDCOMTagType.ORDI, GEDCOMUtils.GetOrdinanceProcessFlagStr(value)); }
        }

        public GEDCOMPointer Submitter
        {
            get { return GetTag(GEDCOMTagType.SUBM, GEDCOMPointer.Create) as GEDCOMPointer; }
        }


        public GEDCOMSubmissionRecord(GEDCOMObject owner) : base(owner)
        {
            SetRecordType(GEDCOMRecordType.rtSubmission);
            SetName(GEDCOMTagType.SUBN);
        }
    }
}
