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

using System;
using GKCommon.GEDCOM;

namespace GKCommon.GEDCOM.Extensions
{
    public enum BloodGroup
    {
        Unknown,
        APositive,
        ANegative,
        BPositive,
        BNegative,
        ABPositive,
        ABNegative,
        OPositive,
        ONegative
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class GEDCOMBloodGroup : GEDCOMTag
    {
        private BloodGroup fValue;

        public BloodGroup Value
        {
            get { return this.fValue; }
            set { this.fValue = value; }
        }

        protected override string GetStringValue()
        {
            string result = "";

            switch (this.fValue) {
                case BloodGroup.APositive:
                    result = "A+";
                    break;

                case BloodGroup.ANegative:
                    result = "A-";
                    break;

                case BloodGroup.BPositive:
                    result = "B+";
                    break;

                case BloodGroup.BNegative:
                    result = "B-";
                    break;

                case BloodGroup.ABPositive:
                    result = "AB+";
                    break;

                case BloodGroup.ABNegative:
                    result = "AB-";
                    break;

                case BloodGroup.OPositive:
                    result = "O+";
                    break;

                case BloodGroup.ONegative:
                    result = "O-";
                    break;
            }

            return result;
        }

        public override string ParseString(string strValue)
        {
            this.fValue = BloodGroup.Unknown;

            string result = strValue.Trim().ToUpperInvariant();
            if (!string.IsNullOrEmpty(result))
            {
                if (result == "A+") {
                    this.fValue = BloodGroup.APositive;
                } else if (result == "A-") {
                    this.fValue = BloodGroup.ANegative;
                } else if (result == "B+") {
                    this.fValue = BloodGroup.BPositive;
                } else if (result == "B-") {
                    this.fValue = BloodGroup.BNegative;
                } else if (result == "AB+") {
                    this.fValue = BloodGroup.ABPositive;
                } else if (result == "AB-") {
                    this.fValue = BloodGroup.ABNegative;
                } else if (result == "O+") {
                    this.fValue = BloodGroup.OPositive;
                } else if (result == "O-") {
                    this.fValue = BloodGroup.ONegative;
                }
            }

            return result;
        }

        public override void Clear()
        {
            base.Clear();
            this.fValue = BloodGroup.Unknown;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (this.fValue == BloodGroup.Unknown);
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            base.SetName("_BGRO");
        }

        public GEDCOMBloodGroup(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMBloodGroup(owner, parent, tagName, tagValue);
        }
    }
}
