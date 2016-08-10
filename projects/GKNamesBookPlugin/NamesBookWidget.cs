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

using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;

namespace GKNamesBookPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public partial class NamesBookWidget : Form
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

        public NamesBookWidget(Plugin plugin) : base()
        {
            this.InitializeComponent();

            this.fPlugin = plugin;
            
            Screen scr = Screen.PrimaryScreen;
            this.Location = new Point(scr.WorkingArea.Width - this.Width - 10, (scr.WorkingArea.Height - this.Height) / 2);

            this.fNames = new List<NameRecord>();
            this.fChurchFNames = new StringList();
            this.fChurchMNames = new StringList();

            this.PrepareList();
            this.UpdateList();

            this.Text = this.fPlugin.LangMan.LS(NLS.LSID_MINamesBook);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fChurchFNames.Dispose();
                this.fChurchMNames.Dispose();
                //this.fNames.Dispose();
            }
            base.Dispose(disposing);
        }

        private void NamesBookWidget_Load(object sender, EventArgs e)
        {
            this.fPlugin.Host.WidgetShow(this.fPlugin);
        }

        private void NamesBookWidget_Closed(object sender, EventArgs e)
        {
            this.fPlugin.Host.WidgetClose(this.fPlugin);
        }

        private void cbNames_SelectedIndexChanged(object sender, EventArgs e)
        {
            int idx = this.cbNames.SelectedIndex;
            if (idx < 0 || idx >= this.cbNames.Items.Count) return;

            GKComboItem item = (GKComboItem)this.cbNames.Items[idx];
            NameRecord rec = (NameRecord)item.Data;

            this.mmDesc.Text = "";
            this.mmDesc.AppendText(rec.Name + "\r\n");
            this.mmDesc.AppendText(rec.Desc + "\r\n");

            if (rec.ChIndex < 0) return;

            this.mmDesc.AppendText("\r\n");
            this.mmDesc.AppendText("Святцы:\r\n");

            StringList lst;
            switch (rec.Sex)
            {
                case GEDCOMSex.svMale:
                    lst = this.fChurchMNames;
                    break;
                case GEDCOMSex.svFemale:
                    lst = this.fChurchFNames;
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
                this.mmDesc.AppendText(st + "\r\n");
            }
        }

        private static bool ExtractFlags(ref string st)
        {
            bool res = (st != null);
            if (res) {
                res = (st.Length >= 2 && st[0] == '[' && st[st.Length - 1] == ']');
                if (res) {
                    st = st.Substring(1, st.Length - 2);
                }
            }
            return res;
        }

        private void PrepareList()
        {
            GKResourceManager resMgr = new GKResourceManager("NBResources", typeof(NamesBookWidget).Assembly);

            byte[] book_names = (byte[])resMgr.GetObjectEx("book_names");

            using (MemoryStream memStream = new MemoryStream(/*NBResources.*/book_names))
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

                                if (ExtractFlags(ref st))
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

                                this.fNames.Add(rec);
                            }
                        }
                    }
                }
            }

            byte[] book_names_cf = (byte[])resMgr.GetObjectEx("book_names_cf");

            using (MemoryStream memStream = new MemoryStream(/*NBResources.*/book_names_cf))
            {
                using (StreamReader strd = new StreamReader(memStream, Encoding.GetEncoding(1251)))
                {
                    while (strd.Peek() != -1) {
                        string ns = strd.ReadLine().Trim();
                        this.fChurchFNames.Add(ns);
                    }
                }
            }

            byte[] book_names_cm = (byte[])resMgr.GetObjectEx("book_names_cm");

            using (MemoryStream memStream = new MemoryStream(/*NBResources.*/book_names_cm))
            {
                using (StreamReader strd = new StreamReader(memStream, Encoding.GetEncoding(1251)))
                {
                    while (strd.Peek() != -1)
                    {
                        string ns = strd.ReadLine().Trim();
                        this.fChurchMNames.Add(ns);
                    }
                }
            }
        }

        private void UpdateList()
        {
            this.cbNames.BeginUpdate();
            try
            {
                this.cbNames.Items.Clear();

                foreach (NameRecord rec in this.fNames)
                {
                    string ns = rec.Name;
                    this.cbNames.Items.Add(new GKComboItem(ns, rec));

                    rec.ChIndex = -1;
                    ns = ns.ToUpper();

                    StringList lst;
                    switch (rec.Sex)
                    {
                        case GEDCOMSex.svMale:
                            lst = this.fChurchMNames;
                            break;
                        case GEDCOMSex.svFemale:
                            lst = this.fChurchFNames;
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
                this.cbNames.EndUpdate();
            }
        }
    }

    public class GKComboItem
    {
        public readonly string Caption;
        public readonly object Data;

        public GKComboItem(string caption, object data)
        {
            this.Caption = caption;
            this.Data = data;
        }

        public override string ToString()
        {
            return this.Caption;
        }
    }
}
