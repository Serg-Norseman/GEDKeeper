/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Plugins;
using GKUI.Components;
using GKUI.Themes;

namespace GKNamesBookPlugin
{
    public partial class NamesBookWidget : Form, IWidgetForm, IThemedView
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private ComboBox cbNames;
        private TextArea mmDesc;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly Plugin fPlugin;
        private readonly NamesBook fNamesBook;

        public NamesBookWidget(Plugin plugin)
        {
            XamlReader.Load(this);

            fPlugin = plugin;
            fNamesBook = new NamesBook(plugin);

            UpdateList();

            SetLocale();
        }

        public void SetLocale()
        {
            Title = fPlugin.LangMan.LS(PLS.NamesBook);
        }

        private void Form_Shown(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetShow(fPlugin);
            if (AppHost.Instance != null) AppHost.Instance.ApplyTheme(this);
            AppHost.Instance.WidgetLocate(this, WidgetLocation.HRight | WidgetLocation.VCenter);
        }

        private void Form_Closed(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetClose(fPlugin);
        }

        public void ApplyTheme()
        {
        }

        private void cbNames_SelectedIndexChanged(object sender, EventArgs e)
        {
            int idx = cbNames.SelectedIndex;
            if (idx >= 0 && idx < cbNames.Items.Count) {
                var item = (GKComboItem<NameRecord>)cbNames.Items[idx];
                mmDesc.Text = fNamesBook.GetNameDesc(item.Tag);
            }
        }

        private void UpdateList()
        {
            //cbNames.BeginUpdate();
            try {
                cbNames.Items.Clear();
                foreach (var rec in fNamesBook.Names) {
                    cbNames.Items.Add(new GKComboItem<NameRecord>(rec.Name, rec));
                }
            } finally {
                //cbNames.EndUpdate();
            }
        }
    }
}
