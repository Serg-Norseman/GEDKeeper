/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Windows.Forms;
using GKCore;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Locales;
using GKCore.Stats;
using GKUI.Themes;
using GKWordsCloudPlugin.WordsCloud;

namespace GKWordsCloudPlugin
{
    public partial class WordsCloudWidget : Form, ILocalizable, IThemedView
    {
        private readonly Plugin fPlugin;
        private IBaseWindow fBase;
        private StatsMode fMode;

        public WordsCloudWidget(Plugin plugin)
        {
            InitializeComponent();

            fPlugin = plugin;
            fMode = StatsMode.smNames;

            SetLocale();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fPlugin.CloseForm();
            }
            base.Dispose(disposing);
        }

        private void Form_Load(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetShow(fPlugin);
            if (!DesignMode && AppHost.Instance != null) AppHost.Instance.ApplyTheme(this);

            BaseChanged(fPlugin.Host.GetCurrentFile());
        }

        private void Form_Closed(object sender, EventArgs e)
        {
            BaseChanged(null);
            fPlugin.Host.WidgetClose(fPlugin);
        }

        public void ApplyTheme()
        {
        }

        public void BaseChanged(IBaseWindow baseWin)
        {
            if (fBase != baseWin) {
                fBase = baseWin;
                UpdateCloud();
            }
        }

        private void UpdateCloud()
        {
            try {
                var words = new List<Word>();
                fPlugin.CollectData(fBase, fMode, words);
                cloudViewer.SetWeightedWords(words);
            } catch (Exception ex) {
                MessageBox.Show(ex.Message);
            }
        }

        private void cbType_SelectedIndexChanged(object sender, EventArgs e)
        {
            var item = (ComboItem<StatsMode>)cbType.SelectedItem;
            if (item != null) {
                fMode = item.Tag;
                UpdateCloud();
            }
        }

        public void SetLocale()
        {
            Text = fPlugin.LangMan.LS(PLS.WordsCloud);

            cbType.BeginUpdate();
            int selItem = cbType.SelectedIndex;
            cbType.Items.Clear();
            foreach (var cloudType in Plugin.CloudTypes) {
                cbType.Items.Add(new ComboItem<StatsMode>(LangMan.LS(cloudType.Name), cloudType.Mode));
            }
            cbType.SelectedIndex = selItem;
            cbType.EndUpdate();
        }
    }
}
