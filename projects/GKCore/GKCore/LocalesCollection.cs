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
using System.IO;
using System.Linq;

namespace GKCore
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
