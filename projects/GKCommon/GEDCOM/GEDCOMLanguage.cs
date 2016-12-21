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

namespace GKCommon.GEDCOM
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GEDCOMLanguage : GEDCOMTag
    {
        private static readonly GEDCOMEnumHelper<GEDCOMLanguageID> LangEnumHelper;

        private GEDCOMLanguageID fValue;

        public GEDCOMLanguageID Value
        {
            get { return this.fValue; }
            set { this.fValue = value; }
        }

        static GEDCOMLanguage()
        {
            LangEnumHelper = new GEDCOMEnumHelper<GEDCOMLanguageID>(GEDCOMConsts.LngEnumStr, GEDCOMLanguageID.Unknown, true);
        }

        public override void Clear()
        {
            base.Clear();
            this.fValue = GEDCOMLanguageID.Unknown;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (this.fValue == GEDCOMLanguageID.Unknown);
        }

        public override void Assign(GEDCOMTag source)
        {
            GEDCOMLanguage srcLang = (source as GEDCOMLanguage);

            if (srcLang != null) {
                this.fValue = srcLang.fValue;
            } else {
                base.Assign(source);
            }
        }

        public override string ParseString(string strValue)
        {
            if (!string.IsNullOrEmpty(strValue))
            {
                this.fValue = LangEnumHelper.GetEnumValue(strValue);
            }

            return strValue;
        }

        protected override string GetStringValue()
        {
            return LangEnumHelper.GetStrValue(this.fValue);
        }

        protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            base.CreateObj(owner, parent);
            this.SetName("LANG");
        }

        public GEDCOMLanguage(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
        {
        }

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMLanguage(owner, parent, tagName, tagValue);
        }

        #region Aux functions

        public static GEDCOMLanguageID GetLIDByName(string name)
        {
            return LangEnumHelper.GetEnumValue(name);
        }

        public static string GetNameByLID(GEDCOMLanguageID lid)
        {
            return LangEnumHelper.GetStrValue(lid);
        }

        #endregion
    }
}
