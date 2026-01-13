/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.IO;
using System.Reflection;
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
        public readonly Assembly ResAssembly;

        public LangRecord(int code, string sign, string name, string fileName, Assembly resAssembly)
        {
            Code = code;
            Sign = sign;
            Name = name;
            FileName = fileName;
            ResAssembly = resAssembly;

            string engLangName = StringHelper.UniformName(Path.GetFileNameWithoutExtension(fileName));
            LangID = GEDCOMUtils.GetLanguageVal(engLangName);
        }
    }
}
