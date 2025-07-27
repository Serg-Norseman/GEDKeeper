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
using System.IO;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore.Design;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKTextSearchPlugin
{
    public sealed partial class TextSearchWin : Form, ILocalizable
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnSearch;
        private TextBox txtQuery;
        private HyperView fResultsText;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly Plugin fPlugin;
        private readonly IBaseWindow fBase;

        public TextSearchWin(Plugin plugin, IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fPlugin = plugin;
            fBase = baseWin;

            SetLocale();
        }

        public void SetLocale()
        {
            Title = fPlugin.LangMan.LS(PLS.TextSearch) + string.Format(" [{0}]", Path.GetFileName(fBase.Context.FileName));
            btnSearch.Text = fPlugin.LangMan.LS(PLS.Search);
        }

        private void Form_Shown(object sender, EventArgs e)
        {
            fPlugin.SearchMan.ReindexBase(fBase);
        }

        private void Form_Closed(object sender, EventArgs e)
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
