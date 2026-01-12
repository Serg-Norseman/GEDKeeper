/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.IO;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Design;
using GKCore.Locales;
using GKUI.Components;
using GKUI.Themes;

namespace GKTextSearchPlugin
{
    public sealed partial class TextSearchWin : Form, ILocalizable, IThemedView
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
            if (AppHost.Instance != null) AppHost.Instance.ApplyTheme(this);
            fPlugin.SearchMan.ReindexBase(fBase);
        }

        private void Form_Closed(object sender, EventArgs e)
        {
            fPlugin.fForm = null;
        }

        public void ApplyTheme()
        {
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
