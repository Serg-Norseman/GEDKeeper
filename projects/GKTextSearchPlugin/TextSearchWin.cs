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
using System.Windows.Forms;

using GKCommon;
using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCore.Interfaces;

namespace GKTextSearchPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TextSearchWin : Form, ILocalization
    {
        private readonly Plugin fPlugin;
        private readonly IBaseWindow fBase;
        private readonly HyperView fResultsText;

        public TextSearchWin(Plugin plugin, IBaseWindow aBase)
        {
            InitializeComponent();

            this.fPlugin = plugin;
            this.fBase = aBase;
            this.Text = string.Format(fPlugin.LangMan.LS(TLS.LSID_PluginTitle) + " [{0}]", Path.GetFileName(fBase.Tree.FileName));

            this.SuspendLayout();
            this.fResultsText = new HyperView();
            this.fResultsText.Dock = DockStyle.Fill;
            this.fResultsText.Location = new Point(0, 0);
            this.fResultsText.Size = new Size(300, 200);
            this.fResultsText.OnLink += mTextLink;
            this.Controls.Add(this.fResultsText);
            this.ResumeLayout(false);
            this.Controls.SetChildIndex(this.fResultsText, 0);

            this.SetLang();
        }

        private void Write(string text)
        {
            fResultsText.Lines.Add(text);
        }

        private void mTextLink(object sender, string linkName)
        {
            if (string.IsNullOrEmpty(linkName)) return;

            fBase.SelectRecordByXRef(linkName);
        }

        private void btnSearch_Click(object sender, EventArgs e)
        {
            this.btnSearch.Enabled = false;
            fResultsText.Lines.BeginUpdate();
            try
            {
                fResultsText.Lines.Clear();
                List<SearchManager.SearchEntry> searchResults = fPlugin.SearchMan.Search(fBase, txtQuery.Text);

                Write(string.Format(fPlugin.LangMan.LS(TLS.LSID_SearchResults) + "\r\n", searchResults.Count));

                int num = searchResults.Count;
                for (int i = 0; i < num; i++)
                {
                    Write("__________________________________________________________________________________________");
                    Write("");

                    SearchManager.SearchEntry entry = searchResults[i];
                    Write(String.Format("~bu+1~{0}: {1}%~u~ ~^{2}:[{2}]~", entry.Rank, entry.Percent, entry.XRef) + "~b-1~");

                    GEDCOMRecord rec = fBase.Tree.XRefIndex_Find(entry.XRef);
                    StringList ctx = fBase.GetRecordContent(rec);
                    fResultsText.Lines.AddStrings(ctx);
                    Write("");
                }
            }
            finally
            {
                fResultsText.Lines.EndUpdate();
                this.btnSearch.Enabled = true;
            }
        }

        private void TextSearchWin_Load(object sender, EventArgs e)
        {
            fPlugin.SearchMan.ReindexBase(fBase);
        }

        private void TextSearchWin_FormClosed(object sender, FormClosedEventArgs e)
        {
            this.fPlugin.tsWin = null;
        }

        #region ILocalization support

        public void SetLang()
        {
            this.btnSearch.Text = fPlugin.LangMan.LS(TLS.LSID_Search);
        }

        #endregion
    }
}
