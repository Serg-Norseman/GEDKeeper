/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Reflection;
using System.Text;
using BSLib;
using GKCore.Utilities;

namespace GKCore.Locales
{
    /// <summary>
    /// Global (static) manager of localizable strings.
    /// </summary>
    public static partial class LangMan
    {
        public const int LS_DEF_CODE = 1033;
        public const string LS_DEF_SIGN = "enu";
        public const string LS_DEF_NAME = "English";

        private static readonly LangManager fLangMan = new LangManager();

        public static string LS(LSID lsid)
        {
            return fLangMan.LS(lsid);
        }

        public static string LS(LSID lsid, params object[] args)
        {
            return string.Format(fLangMan.LS(lsid), args);
        }

        public static string LSS(LSID lsid, int selector)
        {
            return SysUtils.GetWordForm(fLangMan.LS(lsid), selector);
        }

        public static void DefInit()
        {
            fLangMan.DefInit(LSDefList);
        }

        public static bool LoadFromFile(string fileName, Assembly resAssembly)
        {
            return fLangMan.LoadFromFile(fileName, resAssembly);
        }

        public static void SaveDefaultLanguage()
        {
            StreamWriter lf = new StreamWriter(GKUtils.GetLangsPath() + "english.sample2", false, Encoding.UTF8);
            try {
                lf.WriteLine(";" + LS_DEF_CODE.ToString() + "," + LS_DEF_SIGN + "," + LS_DEF_NAME);
                for (LSID i = LSID.First; i <= LSID.Last; i++) {
                    string ls = LSDefList[(int)i - 1];
                    if (!string.IsNullOrEmpty(ls)) {
                        lf.WriteLine(ConvertHelper.AdjustNumber((int)i, 3) + "=" + ls);
                    }
                }
            } finally {
                lf.Close();
            }
        }
    }


    /// <summary>
    /// Instantiable manager of localizable strings.
    /// </summary>
    public class LangManager : ILangMan
    {
        private readonly Dictionary<int, string> fList;

        public LangManager()
        {
            fList = new Dictionary<int, string>();
        }

        public string LS(Enum lsid)
        {
            int idx = ((IConvertible)lsid).ToInt32(null);
            string res;
            return fList.TryGetValue(idx, out res) ? res : "?";
        }

        public bool LoadFromFile(string fileName, Assembly resAssembly)
        {
            bool result = false;

            if (resAssembly == null && !File.Exists(fileName)) return result;

            using (var inputStream = (resAssembly == null) ? new FileStream(fileName, FileMode.Open, FileAccess.Read) : resAssembly.GetManifestResourceStream(fileName)) {
                fList.Clear();

                using (StreamReader lngFile = new StreamReader(inputStream, Encoding.UTF8)) {
                    string st = lngFile.ReadLine();
                    if (!string.IsNullOrEmpty(st) && st[0] == ';') {
                        st = st.Remove(0, 1);
                        string[] lngParams = st.Split(',');
                        if (lngParams.Length < 3)
                            throw new GKException("Header is incorrect");
                    }

                    while (lngFile.Peek() != -1) {
                        st = lngFile.ReadLine();
                        if (string.IsNullOrEmpty(st)) continue;

                        // allowed empty and comment strings
                        int commentPos = st.IndexOf(";");
                        if (commentPos >= 0) st = st.Substring(0, commentPos);
                        st = st.Trim();
                        if (string.IsNullOrEmpty(st)) continue;

                        string[] parts = st.Split('=');
                        if (parts.Length == 2) {
                            int i = int.Parse(parts[0]);
                            fList.Add(i, parts[1]);
                        }
                    }
                    result = true;
                }
            }

            return result;
        }

        internal void DefInit(string[] source)
        {
            fList.Clear();

            for (LSID id = LSID.First; id <= LSID.Last; id++) {
                int idx = (int)id;
                fList.Add(idx, source[idx - 1]);
            }
        }
    }
}
