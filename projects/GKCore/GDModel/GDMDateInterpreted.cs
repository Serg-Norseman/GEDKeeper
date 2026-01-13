/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    /// <summary>
    /// Interpreted from knowledge about the associated date phrase included in parentheses (see GEDCOM 5.5.1 p.47).
    /// Objects of this class are created only from GEDCOMDateValue.
    /// </summary>
    public sealed class GDMDateInterpreted : GDMDate
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


        public GDMDateInterpreted()
        {
            fDatePhrase = string.Empty;
        }

        protected override string GetStringValue()
        {
            return string.Format("{0} {1} ({2})", GEDCOMTagName.INT, base.GetStringValue(), fDatePhrase);
        }

        public override string ParseString(string strValue)
        {
            string result;
            if (string.IsNullOrEmpty(strValue)) {
                Clear();
                fDatePhrase = string.Empty;
                result = string.Empty;
            } else {
                result = GEDCOMUtils.ParseIntDate(this, strValue);
            }
            return result;
        }

        public override string ParseString(StringSpan strValue)
        {
            string result;
            if (strValue.IsEmptyOrEnd) {
                Clear();
                fDatePhrase = string.Empty;
                result = string.Empty;
            } else {
                result = GEDCOMUtils.ParseIntDate(this, strValue);
            }
            return result;
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fDatePhrase);
        }
    }
}
