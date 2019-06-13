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
    public sealed class GDMSourceCitation : GDMPointer, IGDMTextObject
    {
        private int fCertaintyAssessment;
        private StringList fDescription;
        private string fPage;


        public int CertaintyAssessment
        {
            get { return fCertaintyAssessment; }
            set { fCertaintyAssessment = value; }
        }

        public StringList Description
        {
            get {
                StringList description;

                if (!IsPointer) {
                    description = fDescription;
                } else {
                    GDMSourceRecord sourceRecord = Value as GDMSourceRecord;
                    if (sourceRecord != null) {
                        description = sourceRecord.Title.Lines;
                    } else {
                        description = new StringList();
                    }
                }

                return description;
            }
        }

        public string Page
        {
            get { return fPage; }
            set { fPage = value; }
        }

        StringList IGDMTextObject.Lines
        {
            get { return fDescription; }
        }


        public GDMSourceCitation(GDMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.SOUR);

            fCertaintyAssessment = -1;
            fDescription = new StringList();
            fPage = string.Empty;
        }

        public GDMSourceCitation(GDMObject owner, string tagName, string tagValue) : this(owner)
        {
            SetNameValue(tagName, tagValue);
        }

        public override void Assign(GDMTag source)
        {
            GDMSourceCitation sourceObj = (source as GDMSourceCitation);
            if (sourceObj == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(sourceObj);

            fCertaintyAssessment = sourceObj.fCertaintyAssessment;
            fPage = sourceObj.fPage;
            fDescription.Assign(sourceObj.fDescription);
        }

        public override void Clear()
        {
            base.Clear();

            fCertaintyAssessment = -1;
            fDescription.Clear();
            fPage = string.Empty;
        }

        public override bool IsEmpty()
        {
            bool result;
            if (IsPointer) {
                result = base.IsEmpty();
            } else {
                result = fDescription.IsEmpty() && (SubTags.Count == 0) && string.IsNullOrEmpty(fPage);
            }
            return result;
        }

        protected override string GetStringValue()
        {
            string result = IsPointer ? base.GetStringValue() : ((fDescription.Count > 0) ? fDescription[0] : string.Empty);
            return result;
        }

        public override string ParseString(string strValue)
        {
            string result = base.ParseString(strValue);
            if (!IsPointer) {
                fDescription.Clear();
                if (!string.IsNullOrEmpty(result)) {
                    fDescription.Add(result);
                }
                result = string.Empty;
            } else {
                fStringValue = string.Empty;
            }
            return result;
        }

        /// <summary>
        /// Strange values were found, possibly from other genealogical programs.
        /// </summary>
        /// <returns>Checked value of CertaintyAssessment</returns>
        public int GetValidCertaintyAssessment()
        {
            int val = fCertaintyAssessment;
            return (val >= 0 && val <= 3) ? val : 0;
        }
    }
}
