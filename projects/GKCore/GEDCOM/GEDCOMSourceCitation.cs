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

using BSLib;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMSourceCitation : GEDCOMPointer
    {
        public bool IsPointer
        {
            get {
                return (!string.IsNullOrEmpty(XRef));
            }
        }

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
            get { return GetTagStringValue("PAGE"); }
            set { SetTagStringValue("PAGE", value); }
        }

        public int CertaintyAssessment
        {
            get { return GetTagIntegerValue("QUAY", 0); }
            set { SetTagIntegerValue("QUAY", value); }
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            SetName("SOUR");
        }

        protected override string GetStringValue()
        {
            string result = IsPointer ? base.GetStringValue() : fStringValue;
            return result;
        }

        public override bool IsEmpty()
        {
            bool result;
            if (IsPointer) {
                result = base.IsEmpty();
            } else {
                result = (fStringValue == "" && Count == 0);
            }
            return result;
        }

        public override string ParseString(string strValue)
        {
            fStringValue = "";
            XRef = "";
            string result = strValue;
            if (!string.IsNullOrEmpty(result))
            {
                result = GEDCOMUtils.ExtractDelimiter(result, 0);
                result = base.ParseString(result);
                if (!IsPointer)
                {
                    fStringValue = result;
                    result = "";
                }
            }
            return result;
        }

        public GEDCOMSourceCitation(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMSourceCitation(owner, parent, tagName, tagValue);
        }
    }
}
