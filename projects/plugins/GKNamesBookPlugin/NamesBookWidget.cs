/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Windows.Forms;
using GKCore;
using GKCore.Plugins;
using GKUI.Components;
using GKUI.Themes;

namespace GKNamesBookPlugin
{
    public partial class NamesBookWidget : Form, IWidgetForm, IThemedView
    {
        private readonly Plugin fPlugin;
        private readonly NamesBook fNamesBook;

        public NamesBookWidget(Plugin plugin)
        {
            InitializeComponent();

            fPlugin = plugin;
            fNamesBook = new NamesBook(plugin);

            UpdateList();

            SetLocale();
        }

        public void SetLocale()
        {
            Text = fPlugin.LangMan.LS(PLS.NamesBook);
        }

        private void NamesBookWidget_Load(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetShow(fPlugin);
            if (!DesignMode && AppHost.Instance != null) AppHost.Instance.ApplyTheme(this);
            AppHost.Instance.WidgetLocate(this, WidgetLocation.HRight | WidgetLocation.VCenter);
        }

        private void NamesBookWidget_Closed(object sender, EventArgs e)
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
            cbNames.BeginUpdate();
            try {
                cbNames.Items.Clear();
                foreach (var rec in fNamesBook.Names) {
                    cbNames.Items.Add(new GKComboItem<NameRecord>(rec.Name, rec));
                }
            } finally {
                cbNames.EndUpdate();
            }
        }
    }
}
