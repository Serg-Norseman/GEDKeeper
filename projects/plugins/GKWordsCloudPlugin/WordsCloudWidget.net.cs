/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017-2023 by Sergey V. Zhdanovskih.
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
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Design;
using GKCore.Locales;
using GKCore.Stats;
using GKUI.Platform;
using GKUI.Themes;
using GKWordsCloudPlugin.WordsCloud;

namespace GKWordsCloudPlugin
{
    public partial class WordsCloudWidget : Form, ILocalizable, IThemedView
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private GKDropDownToolItem tbType;
        private CloudViewer cloudViewer;
        private ContextMenu cbType;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly Plugin fPlugin;
        private IBaseWindow fBase;
        private StatsMode fMode;

        public WordsCloudWidget(Plugin plugin)
        {
            XamlReader.Load(this);

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

        private void Form_Shown(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetShow(fPlugin);
            if (AppHost.Instance != null) AppHost.Instance.ApplyTheme(this);

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

        /*private void cbType_SelectedIndexChanged(object sender, EventArgs e)
        {
            var item = (ComboItem<StatsMode>)cbType.SelectedItem;
            if (item != null) {
                fMode = item.Tag;
                UpdateCloud();
            }
        }*/

        public void SetLocale()
        {
            Title = fPlugin.LangMan.LS(PLS.WordsCloud);

            //cbType.BeginUpdate();
            //int selItem = cbType.SelectedIndex;
            cbType.Items.Clear();
            foreach (var cloudType in Plugin.CloudTypes) {
                var mi = new ButtonMenuItem();
                mi.Text = LangMan.LS(cloudType.Name);
                mi.Tag = cloudType.Mode;
                mi.Click += (sender, e) => {
                    var item = ((ButtonMenuItem)sender);
                    fMode = (StatsMode)item.Tag;
                    UpdateCloud();
                    tbType.Text = item.Text;
                };

                cbType.Items.Add(mi);
            }
            //cbType.SelectedIndex = selItem;
            //cbType.EndUpdate();
        }
    }
}
