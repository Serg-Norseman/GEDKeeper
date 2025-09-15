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
