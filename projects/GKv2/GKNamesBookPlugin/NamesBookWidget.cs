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
using System.Windows.Forms;
using GKCore;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKNamesBookPlugin
{
    public partial class NamesBookWidget : Form, IWidgetForm
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
            Text = fPlugin.LangMan.LS(NLS.LSID_MINamesBook);
        }

        private void NamesBookWidget_Load(object sender, EventArgs e)
        {
            AppHost.Instance.WidgetLocate(this, WidgetLocation.HRight | WidgetLocation.VCenter);
            fPlugin.Host.WidgetShow(fPlugin);
        }

        private void NamesBookWidget_Closed(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetClose(fPlugin);
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
