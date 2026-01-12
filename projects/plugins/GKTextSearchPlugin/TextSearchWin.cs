/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using GKCore;
using GKCore.Design;
using GKCore.Locales;
using GKUI.Components;
using GKUI.Themes;

namespace GKTextSearchPlugin
{
    public sealed partial class TextSearchWin : Form, ILocalizable, IThemedView
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
            Text = fPlugin.LangMan.LS(PLS.TextSearch) + string.Format(" [{0}]", Path.GetFileName(fBase.Context.FileName));
            btnSearch.Text = fPlugin.LangMan.LS(PLS.Search);
        }

        private void Form_Load(object sender, EventArgs e)
        {
            if (!DesignMode && AppHost.Instance != null) AppHost.Instance.ApplyTheme(this);
            fPlugin.SearchMan.ReindexBase(fBase);
        }

        private void Form_Closed(object sender, FormClosedEventArgs e)
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
