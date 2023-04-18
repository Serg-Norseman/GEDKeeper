/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKTextSearchPlugin
{
    public sealed partial class TextSearchWin : Form, ILocalizable
    {
        private readonly Plugin fPlugin;
        private readonly IBaseWindow fBase;
        private readonly HyperView fResultsText;

        public TextSearchWin(Plugin plugin, IBaseWindow baseWin)
        {
            InitializeComponent();

            fPlugin = plugin;
            fBase = baseWin;

            SuspendLayout();
            fResultsText = new HyperView();
            fResultsText.Dock = DockStyle.Fill;
            fResultsText.Location = new Point(0, 0);
            fResultsText.Size = new Size(300, 200);
            fResultsText.OnLink += mTextLink;
            Controls.Add(fResultsText);
            ResumeLayout(false);
            Controls.SetChildIndex(fResultsText, 0);

            SetLocale();
        }

        public void SetLocale()
        {
            Text = fPlugin.LangMan.LS(TLS.LSID_PluginTitle) + string.Format(" [{0}]", Path.GetFileName(fBase.Context.FileName));
            btnSearch.Text = fPlugin.LangMan.LS(TLS.LSID_Search);
        }

        private void Form_Load(object sender, EventArgs e)
        {
            fPlugin.SearchMan.ReindexBase(fBase);
        }

        private void Form_Closed(object sender, FormClosedEventArgs e)
        {
            fPlugin.fForm = null;
        }

        private void mTextLink(object sender, string linkName)
        {
            if (!string.IsNullOrEmpty(linkName))
                fBase.SelectRecordByXRef(linkName);
        }

        private void btnSearch_Click(object sender, EventArgs e)
        {
            btnSearch.Enabled = false;
            try {
                fPlugin.SearchMan.ShowResults(fBase, txtQuery.Text, fResultsText.Lines);
            } finally {
                btnSearch.Enabled = true;
            }
        }
    }
}
