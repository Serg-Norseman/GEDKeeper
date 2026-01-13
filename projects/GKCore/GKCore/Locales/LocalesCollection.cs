/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.IO;
using System.Linq;
using GKCore.Utilities;

namespace GKCore.Locales
{
    public sealed class LocaleInfo
    {
        public string Locale;
        public string MapBasePoint;
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class LocalesCollection
    {
        private LocaleInfo[] fList;

        public LocalesCollection()
        {
            fList = new LocaleInfo[0];
        }

        public void Load(string fileName)
        {
            if (!File.Exists(fileName))
                return;

            try {
                using (var reader = new StreamReader(fileName)) {
                    string content = reader.ReadToEnd();
                    fList = YamlHelper.Deserialize<LocaleInfo[]>(content);
                }
            } catch (Exception ex) {
                Logger.WriteError("LocalesCollection.Load()", ex);
            }
        }

        public string GetMapBasePoint(string locale)
        {
            var locInfo = fList.FirstOrDefault((x) => x.Locale == locale);
            return (locInfo == null) ? null : locInfo.MapBasePoint;
        }
    }
}
