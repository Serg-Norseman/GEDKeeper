/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKCore.Options
{
    /// <summary>
    /// Temporary class for options that depend on the current locale.
    /// </summary>
    public sealed class LocaleOptions
    {
        private static LocaleOptions fInstance = null;

        private ushort fLocale;

        public static LocaleOptions Instance
        {
            get {
                if (fInstance == null) fInstance = new LocaleOptions();
                return fInstance;
            }
        }

        public void SetLocale(ushort locale)
        {
            fLocale = locale;
        }

        public bool AlwaysCapitalizeNouns()
        {
            // 1031,Deutsch(de),German(en)
            return (fLocale == 1031);
        }
    }
}
