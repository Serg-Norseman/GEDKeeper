/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.IO;
using System.Text;
using BSLib;
using GDModel;

namespace GKNamesBookPlugin
{
    internal class NameRecord
    {
        public string Name;
        public string Desc;
        public GDMSex Sex;

        public StringList ChList;
        public int ChIndex;
    }

    internal class NamesBook
    {
        private readonly List<NameRecord> fNames;
        private readonly StringList fChurchFNames;
        private readonly StringList fChurchMNames;
        private readonly Plugin fPlugin;

        public List<NameRecord> Names
        {
            get { return fNames; }
        }

        public StringList ChurchFNames
        {
            get { return fChurchFNames; }
        }

        public StringList ChurchMNames
        {
            get { return fChurchMNames; }
        }

        public NamesBook(Plugin plugin)
        {
            fPlugin = plugin;

            fNames = new List<NameRecord>();
            fChurchFNames = new StringList();
            fChurchMNames = new StringList();

            PrepareList();
        }

        private static string ExtractFlags(string st)
        {
            bool res = (st != null);
            if (res) {
                res = (st.Length >= 2 && st[0] == '[' && st[st.Length - 1] == ']');
                if (res) {
                    st = st.Substring(1, st.Length - 2);
                }
                return st;
            } else {
                return string.Empty;
            }
        }

        private void PrepareList()
        {
            using (Stream bookNames = fPlugin.LoadResourceStream("bk_names.txt")) {
                using (StreamReader strd = new StreamReader(bookNames, Encoding.UTF8)) {
                    while (strd.Peek() != -1) {
                        string ns = strd.ReadLine().Trim();
                        if (ns != "") {
                            string[] toks = ns.Split('/');
                            if (toks.Length >= 3) {
                                var rec = new NameRecord();
                                rec.Name = toks[0].Trim();
                                rec.Desc = toks[2].Trim();
                                string st = toks[1].Trim();

                                st = ExtractFlags(st);
                                if (!string.IsNullOrEmpty(st)) {
                                    char c = st[0];
                                    switch (c) {
                                        case 'f':
                                            rec.Sex = GDMSex.svFemale;
                                            break;
                                        case 'm':
                                            rec.Sex = GDMSex.svMale;
                                            break;
                                    }
                                }

                                fNames.Add(rec);
                            }
                        }
                    }
                }
            }

            using (Stream bookNames = fPlugin.LoadResourceStream("bk_names_cf.txt")) {
                using (StreamReader strd = new StreamReader(bookNames, Encoding.UTF8)) {
                    while (strd.Peek() != -1) {
                        string ns = strd.ReadLine().Trim();
                        fChurchFNames.Add(ns);
                    }
                }
            }

            using (Stream bookNames = fPlugin.LoadResourceStream("bk_names_cm.txt")) {
                using (StreamReader strd = new StreamReader(bookNames, Encoding.UTF8)) {
                    while (strd.Peek() != -1) {
                        string ns = strd.ReadLine().Trim();
                        fChurchMNames.Add(ns);
                    }
                }
            }

            foreach (var rec in fNames) {
                string ns = rec.Name.ToUpper();
                rec.ChIndex = -1;

                StringList lst = null;
                switch (rec.Sex) {
                    case GDMSex.svMale:
                        lst = fChurchMNames;
                        break;
                    case GDMSex.svFemale:
                        lst = fChurchFNames;
                        break;
                }

                rec.ChList = lst;
                if (lst != null) {
                    for (int j = 0; j < lst.Count; j++) {
                        string st = lst[j];
                        if (st[0] == '-' && st.IndexOf(ns) >= 0) {
                            rec.ChIndex = j;
                            break;
                        }
                    }
                }
            }
        }

        public string GetNameDesc(NameRecord rec)
        {
            if (rec == null) return string.Empty;

            var result = new StringBuilder();

            result.AppendLine(rec.Name);
            result.AppendLine(rec.Desc);

            if (rec.ChIndex >= 0 && rec.ChList != null) {
                result.AppendLine("");
                result.AppendLine(fPlugin.LangMan.LS(PLS.Calendar) + ":");

                int num = rec.ChList.Count;
                for (int i = rec.ChIndex + 1; i < num; i++) {
                    string st = rec.ChList[i].Trim();
                    if (st[0] == '-') {
                        break;
                    }
                    st = st.Remove(0, 1);
                    result.AppendLine(st);
                }
            }

            return result.ToString();
        }
    }
}
