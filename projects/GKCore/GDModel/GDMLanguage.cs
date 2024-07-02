/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    public enum GDMLanguageID
    {
        Unknown,

        Afrikaans,
        Akkadian,       // <extinct>
        Albanian,
        Amharic,        // Ethiopia
        AncientGreek,   // <ancient>
        AngloSaxon,
        Arabic,
        Armenian,
        Assamese,       // Bangladesh, Bhutan, India
        Belorusian,
        Bengali,        // Bangladesh, India
        Braj,           // India
        Bulgarian,
        Burmese,        // Myanmar/Burma
        Cantonese,      // Chinese
        Catalan,
        CatalanSpn,
        ChurchSlavic,
        Czech,
        Danish,
        Dogri,          // India
        Dutch,
        Eblaite,        // <extinct>
        English,
        Esperanto,
        Estonian,
        Faroese,
        Finnish,
        French,
        Georgian,
        German,
        Greek,
        Gujarati,       // India
        Hattic,         // <extinct>
        Hawaiian,
        Hebrew,         // Israel
        Hindi,          // India
        Hittite,        // <extinct>
        Hungarian,
        Hurrian,        // <extinct>
        Icelandic,
        Indonesian,
        Italian,
        Japanese,
        Kannada,        // India
        Kazakh,
        Khmer,          // Cambodia
        Konkani,        // India
        Korean,
        Lahnda,         // Pakistan
        Lao,            // Laos, Thailand
        Latin,          // <ancient>
        Latvian,
        Lithuanian,
        Luwian,         // <extinct>
        Macedonian,
        Maithili,       // Nepal, India
        Malayalam,      // India
        Mandrin,        // Chinese
        Manipuri,       // India
        Marathi,        // India
        Mewari,         // India
        MitanniAryan,   // <extinct>
        Navaho,
        Nepali,         // Nepal
        Norwegian,
        Oriya,          // India
        Pahari,         // Nepal, India, Pakistan
        Palaic,         // <extinct>
        Pali,           // India
        Panjabi,        // India, Pakistan
        Persian,
        Polish,
        Portuguese,
        Prakrit,        // India
        Pusto,
        Rajasthani,     // India
        Romanian,
        Russian,
        Sanskrit,
        Serb,
        SerboCroatian,
        Slovak,
        Slovene,
        Spanish,
        Sumerian,       // <extinct>
        Swedish,
        Tagalog,        // Philippines
        Tamil,          // India
        Telugu,         // India
        Thai,
        Tibetan,
        Turkish,
        Ukrainian,
        Urdu,           // India, Pakistan
        Vietnamese,
        Wendic,
        Yiddish,
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class GDMLanguage : GDMTag
    {
        private GDMLanguageID fValue;

        public GDMLanguageID Value
        {
            get { return fValue; }
            set { fValue = value; }
        }

        public override void Clear()
        {
            base.Clear();
            fValue = GDMLanguageID.Unknown;
        }

        public override bool IsEmpty()
        {
            return base.IsEmpty() && (fValue == GDMLanguageID.Unknown);
        }

        public override void Assign(GDMTag source)
        {
            GDMLanguage srcLang = (source as GDMLanguage);
            if (srcLang == null)
                throw new ArgumentException(@"Argument is null or wrong type", "source");

            base.Assign(source);

            fValue = srcLang.fValue;
        }

        public override string ParseString(string strValue)
        {
            fValue = GEDCOMUtils.GetLanguageVal(strValue);
            return string.Empty;
        }

        protected override string GetStringValue()
        {
            return GEDCOMUtils.GetLanguageStr(fValue);
        }

        public GDMLanguage()
        {
            SetName(GEDCOMTagType.LANG);
        }

        public GDMLanguage(int tagId, string tagValue)
        {
            SetNameValue(tagId, tagValue);
        }

        protected override void ProcessHashes(ref HashCode hashCode)
        {
            base.ProcessHashes(ref hashCode);

            hashCode.Add(fValue);
        }
    }
}
