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

using BSLib;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMSourceCitation : GEDCOMPointer
    {
        public StringList Description
        {
            get {
                StringList description;

                if (!IsPointer) {
                    description = GetTagStrings(this);
                } else {
                    GEDCOMSourceRecord sourceRecord = Value as GEDCOMSourceRecord;
                    if (sourceRecord != null) {
                        description = sourceRecord.Title;
                    } else {
                        description = new StringList();
                    }
                }

                return description;
            }
            set {
                Clear();
                SetTagStrings(this, value);
            }
        }

        public string Page
        {
            get { return GetTagStringValue(GEDCOMTagType.PAGE); }
            set { SetTagStringValue(GEDCOMTagType.PAGE, value); }
        }

        public int CertaintyAssessment
        {
            get { return GetTagIntegerValue(GEDCOMTagType.QUAY, 0); }
            set { SetTagIntegerValue(GEDCOMTagType.QUAY, value); }
        }


        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMSourceCitation(owner, parent, tagName, tagValue);
        }

        public GEDCOMSourceCitation(GEDCOMTree owner, GEDCOMObject parent) : base(owner, parent)
        {
            SetName(GEDCOMTagType.SOUR);
        }

        public GEDCOMSourceCitation(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : this(owner, parent)
        {
            SetNameValue(tagName, tagValue);
        }

        public override bool IsEmpty()
        {
            bool result;
            if (IsPointer) {
                result = base.IsEmpty();
            } else {
                result = (string.IsNullOrEmpty(fStringValue) && Count == 0);
            }
            return result;
        }

        protected override string GetStringValue()
        {
            string result = IsPointer ? base.GetStringValue() : fStringValue;
            return result;
        }

        public override string ParseString(string strValue)
        {
            string result = base.ParseString(strValue);
            if (!IsPointer) {
                fStringValue = result;
                result = string.Empty;
            } else {
                fStringValue = string.Empty;
            }
            return result;
        }
    }
}
