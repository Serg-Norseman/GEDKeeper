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
    /// <summary>
    /// Interpreted from knowledge about the associated date phrase included in parentheses (see GEDCOM 5.5.1 p.47).
    /// Objects of this class are created only from GEDCOMDateValue.
    /// </summary>
    public sealed class GEDCOMDateInterpreted : GEDCOMDate
    {
        private string fDatePhrase;

        public string DatePhrase
        {
            get {
                return fDatePhrase;
            }
            set {
                string phrase = value;

                if (!string.IsNullOrEmpty(phrase)) {
                    if (phrase[0] == '(') {
                        phrase = phrase.Remove(0, 1);
                    }

                    if (phrase.Length > 0 && phrase[phrase.Length - 1] == ')') {
                        phrase = phrase.Remove(phrase.Length - 1, 1);
                    }
                }

                fDatePhrase = phrase;
            }
        }


        public GEDCOMDateInterpreted(GEDCOMTree owner, GEDCOMObject parent) : base(owner, parent)
        {
            fDatePhrase = string.Empty;
        }

        protected override string GetStringValue()
        {
            return string.Format("{0} {1} ({2})", GEDCOMTagType.INT, base.GetStringValue(), fDatePhrase);
        }

        public override string ParseString(string strValue)
        {
            string result;
            if (string.IsNullOrEmpty(strValue)) {
                Clear();
                fDatePhrase = string.Empty;
                result = string.Empty;
            } else {
                result = GEDCOMUtils.ParseIntDate(Owner, this, strValue);
            }
            return result;
        }
    }
}
