/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using System.IO;
using GDModel;
using GKCore.Interfaces;
using SGCulture = System.Globalization.CultureInfo;

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
            DefaultCulture culture;
            switch (langID) {
                case GDMLanguageID.Armenian:
                    culture = new ArmenianCulture();
                    break;

                case GDMLanguageID.Cantonese:
                case GDMLanguageID.Mandrin:
                    culture = new ChineseCulture();
                    break;

                case GDMLanguageID.Czech:
                    culture = new CzechCulture();
                    break;

                case GDMLanguageID.English:
                    culture = new BritishCulture();
                    break;

                case GDMLanguageID.French:
                    culture = new FrenchCulture();
                    break;

                case GDMLanguageID.German:
                    culture = new GermanCulture();
                    break;

                case GDMLanguageID.Icelandic:
                    culture = new IcelandCulture();
                    break;

                case GDMLanguageID.Italian:
                    culture = new ItalianCulture();
                    break;

                case GDMLanguageID.Polish:
                    culture = new PolishCulture();
                    break;

                case GDMLanguageID.Russian:
                case GDMLanguageID.Belorusian:
                case GDMLanguageID.Kazakh:
                case GDMLanguageID.Ukrainian:
                    culture = new RussianCulture();
                    break;

                case GDMLanguageID.Swedish:
                    culture = new SwedishCulture();
                    break;

                case GDMLanguageID.Turkish:
                    culture = new TurkishCulture();
                    break;

                default:
                    culture = new EuropeanCulture();
                    break;
            }

            culture.Language = langID;
            LoadSettings(culture);
            return culture;
        }

        public static SGCulture GetSystemCulture(ICulture culture)
        {
            try {
                if (culture == null) return SGCulture.InvariantCulture;

                string sysLC = culture.SysCulture;
                return (string.IsNullOrEmpty(sysLC)) ? SGCulture.InvariantCulture : SGCulture.GetCultureInfo(sysLC);
            } catch (Exception ex) {
                Logger.WriteError("CulturesPool.GetSystemCulture()", ex);
                return SGCulture.InvariantCulture;
            }
        }

        private static void LoadSettings(DefaultCulture culture)
        {
            string fileName = GKUtils.GetCulturesPath() + culture.Language.ToString() + ".yaml";
            if (!File.Exists(fileName)) return;

            try {
                CultureSettings cultureSettings;
                using (var reader = new StreamReader(fileName)) {
                    string content = reader.ReadToEnd();
                    cultureSettings = YamlHelper.Deserialize<CultureSettings>(content);
                }

                if (cultureSettings != null) {
                    culture.HasPatronymic = cultureSettings.HasPatronymic;
                    culture.HasSurname = cultureSettings.HasSurname;
                    culture.SysCulture = cultureSettings.SysCulture;
                }
            } catch (Exception ex) {
                Logger.WriteError(string.Format("CulturesPool.LoadSettings({0})", fileName), ex);
            }
        }

        private sealed class CultureSettings
        {
            public string Language { get; set; }
            public bool HasPatronymic { get; set; }
            public bool HasSurname { get; set; }
            public string SysCulture { get; set; }

            public CultureSettings()
            {
            }
        }
    }
}
