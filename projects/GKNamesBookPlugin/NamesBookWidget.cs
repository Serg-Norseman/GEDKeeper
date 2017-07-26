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

using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;

namespace GKNamesBookPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public partial class NamesBookWidget : Form, ILocalization
    {
        private class NameRecord
        {
            public string Name;
            public string Desc;
            public GEDCOMSex Sex;
            public int ChIndex;
        }

        private readonly Plugin fPlugin;
        private readonly List<NameRecord> fNames;
        private readonly StringList fChurchFNames;
        private readonly StringList fChurchMNames;

        public NamesBookWidget(Plugin plugin)
        {
            InitializeComponent();

            fPlugin = plugin;
            
            Screen scr = Screen.PrimaryScreen;
            Location = new Point(scr.WorkingArea.Width - Width - 10, (scr.WorkingArea.Height - Height) / 2);

            fNames = new List<NameRecord>();
            fChurchFNames = new StringList();
            fChurchMNames = new StringList();

            PrepareList();
            UpdateList();

            SetLang();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                fChurchFNames.Dispose();
                fChurchMNames.Dispose();
                //this.fNames.Dispose();
            }
            base.Dispose(disposing);
        }

        #region ILocalization support

        public void SetLang()
        {
            Text = fPlugin.LangMan.LS(NLS.LSID_MINamesBook);
        }

        #endregion

        private void NamesBookWidget_Load(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetShow(fPlugin);
        }

        private void NamesBookWidget_Closed(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetClose(fPlugin);
        }

        private void cbNames_SelectedIndexChanged(object sender, EventArgs e)
        {
            int idx = cbNames.SelectedIndex;
            if (idx < 0 || idx >= cbNames.Items.Count) return;

            GKComboItem item = (GKComboItem)cbNames.Items[idx];
            NameRecord rec = (NameRecord)item.Data;

            mmDesc.Text = "";
            mmDesc.AppendText(rec.Name + "\r\n");
            mmDesc.AppendText(rec.Desc + "\r\n");

            if (rec.ChIndex < 0) return;

            mmDesc.AppendText("\r\n");
            mmDesc.AppendText(fPlugin.LangMan.LS(NLS.LSID_Calendar) + ":\r\n");

            StringList lst;
            switch (rec.Sex)
            {
                case GEDCOMSex.svMale:
                    lst = fChurchMNames;
                    break;
                case GEDCOMSex.svFemale:
                    lst = fChurchFNames;
                    break;
                default:
                    return;
            }

            int num = lst.Count;
            for (int i = rec.ChIndex + 1; i < num; i++)
            {
                string st = lst[i].Trim();
                if (st[0] == '-')
                {
                    break;
                }
                st = st.Remove(0, 1);
                mmDesc.AppendText(st + "\r\n");
            }
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
            GKResourceManager resMgr = new GKResourceManager("NBResources", typeof(NamesBookWidget).Assembly);

            byte[] bookNames = (byte[])resMgr.GetObjectEx("book_names");

            using (MemoryStream memStream = new MemoryStream(/*NBResources.*/bookNames))
            {
                using (StreamReader strd = new StreamReader(memStream, Encoding.GetEncoding(1251)))
                {
                    while (strd.Peek() != -1)
                    {
                        string ns = strd.ReadLine().Trim();
                        if (ns != "")
                        {
                            string[] toks = ns.Split('/');
                            if (toks.Length >= 3)
                            {
                                NameRecord rec = new NameRecord();

                                rec.Name = toks[0].Trim();
                                rec.Desc = toks[2].Trim();
                                string st = toks[1].Trim();

                                st = ExtractFlags(st);
                                if (!string.IsNullOrEmpty(st))
                                {
                                    char c = st[0];
                                    switch (c)
                                    {
                                        case 'f':
                                            rec.Sex = GEDCOMSex.svFemale;
                                            break;
                                        case 'm':
                                            rec.Sex = GEDCOMSex.svMale;
                                            break;
                                    }
                                }

                                fNames.Add(rec);
                            }
                        }
                    }
                }
            }

            byte[] bookNamesCf = (byte[])resMgr.GetObjectEx("book_names_cf");

            using (MemoryStream memStream = new MemoryStream(/*NBResources.*/bookNamesCf))
            {
                using (StreamReader strd = new StreamReader(memStream, Encoding.GetEncoding(1251)))
                {
                    while (strd.Peek() != -1) {
                        string ns = strd.ReadLine().Trim();
                        fChurchFNames.Add(ns);
                    }
                }
            }

            byte[] bookNamesCm = (byte[])resMgr.GetObjectEx("book_names_cm");

            using (MemoryStream memStream = new MemoryStream(/*NBResources.*/bookNamesCm))
            {
                using (StreamReader strd = new StreamReader(memStream, Encoding.GetEncoding(1251)))
                {
                    while (strd.Peek() != -1)
                    {
                        string ns = strd.ReadLine().Trim();
                        fChurchMNames.Add(ns);
                    }
                }
            }
        }

        private void UpdateList()
        {
            cbNames.BeginUpdate();
            try
            {
                cbNames.Items.Clear();

                foreach (NameRecord rec in fNames)
                {
                    string ns = rec.Name;
                    cbNames.Items.Add(new GKComboItem(ns, rec));

                    rec.ChIndex = -1;
                    ns = ns.ToUpper();

                    StringList lst;
                    switch (rec.Sex)
                    {
                        case GEDCOMSex.svMale:
                            lst = fChurchMNames;
                            break;
                        case GEDCOMSex.svFemale:
                            lst = fChurchFNames;
                            break;
                        default:
                            return;
                    }

                    int num2 = lst.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        string st = lst[j];
                        if (st[0] == '-' && st.IndexOf(ns) >= 0)
                        {
                            rec.ChIndex = j;
                            break;
                        }
                    }
                }
            }
            finally
            {
                cbNames.EndUpdate();
            }
        }
    }

    public class GKComboItem
    {
        public readonly string Caption;
        public readonly object Data;

        public GKComboItem(string caption, object data)
        {
            Caption = caption;
            Data = data;
        }

        public override string ToString()
        {
            return Caption;
        }
    }
}
