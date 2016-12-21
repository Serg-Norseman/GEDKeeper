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

using System.IO;
using GKCommon;
using GKCommon.GEDCOM;

namespace GKCore.Options
{
    public sealed class LangRecord
    {
        public readonly ushort Code;
        public readonly string Sign;
        public readonly string Name;
        public readonly string FileName;
        public readonly GEDCOMLanguageID LangID;

        public LangRecord(ushort code, string sign, string name, string fileName)
        {
            this.Code = code;
            this.Sign = sign;
            this.Name = name;
            this.FileName = fileName;

            string engLangName = SysUtils.NormalizeName(Path.GetFileNameWithoutExtension(fileName));
            this.LangID = GEDCOMLanguage.GetLIDByName(engLangName);
        }
    }
}
