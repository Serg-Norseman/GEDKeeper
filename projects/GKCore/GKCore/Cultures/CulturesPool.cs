/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;
using GDModel;
using GKCore.Interfaces;

namespace GKCore.Cultures
{
    /// <summary>
    /// 
    /// </summary>
    public static class CulturesPool
    {
        private static readonly Dictionary<GDMLanguageID, ICulture> fCultures = new Dictionary<GDMLanguageID, ICulture>();


        public static ICulture DefineCulture(GDMLanguageID langID)
        {
            ICulture result;
            if (!fCultures.TryGetValue(langID, out result)) {
                result = CreateCulture(langID);
                fCultures.Add(langID, result);
            }
            return result;
        }

        private static ICulture CreateCulture(GDMLanguageID langID)
        {
            ICulture culture;
            switch (langID) {
                case GDMLanguageID.Russian:
                case GDMLanguageID.Ukrainian:
                case GDMLanguageID.Kazakh:
                    culture = new RussianCulture();
                    break;

                case GDMLanguageID.Polish:
                    culture = new PolishCulture();
                    break;

                case GDMLanguageID.German:
                    culture = new GermanCulture();
                    break;

                case GDMLanguageID.Swedish:
                    culture = new SwedishCulture();
                    break;

                case GDMLanguageID.Icelandic:
                    culture = new IcelandCulture();
                    break;

                case GDMLanguageID.Armenian:
                    culture = new ArmenianCulture();
                    break;

                case GDMLanguageID.Turkish:
                    culture = new TurkishCulture();
                    break;

                case GDMLanguageID.French:
                    culture = new FrenchCulture();
                    break;

                case GDMLanguageID.Italian:
                    culture = new ItalianCulture();
                    break;

                case GDMLanguageID.Cantonese:
                case GDMLanguageID.Mandrin:
                    culture = new ChineseCulture();
                    break;

                case GDMLanguageID.English:
                default:
                    culture = new BritishCulture();
                    break;
            }

            culture.Language = langID;
            return culture;
        }
    }
}
