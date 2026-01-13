/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.IO;
using GDModel;
using GKCore.Utilities;
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
