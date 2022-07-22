/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;

namespace GKCore.Options
{
    public sealed class LangRecord
    {
        public readonly int Code;
        public readonly string Sign;
        public readonly string Name;
        public readonly string FileName;
        public readonly GDMLanguageID LangID;

        public LangRecord(int code, string sign, string name, string fileName)
        {
            Code = code;
            Sign = sign;
            Name = name;
            FileName = fileName;

            string engLangName = StringHelper.UniformName(Path.GetFileNameWithoutExtension(fileName));
            LangID = GEDCOMUtils.GetLanguageVal(engLangName);
        }
    }
}
