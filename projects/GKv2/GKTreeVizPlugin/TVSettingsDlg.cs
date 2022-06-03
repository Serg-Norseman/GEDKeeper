﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using GKCore.Interfaces;

namespace GKTreeVizPlugin
{
    public sealed partial class TVSettingsDlg : Form
    {
        private readonly IPlugin fPlugin;

        public int MinGens
        {
            get { return decimal.ToInt32(edMinGens.Value); }
        }

        public TVSettingsDlg(IPlugin plugin)
        {
            InitializeComponent();

            fPlugin = plugin;
            Text = fPlugin.DisplayName;

            // SetLocale()
            chkWithoutDates.Text = fPlugin.LangMan.LS(PLS.LSID_WithoutDates);
            lblMinGens.Text = fPlugin.LangMan.LS(PLS.LSID_MinGens);
            btnAccept.Text = fPlugin.LangMan.LS(PLS.LSID_Accept);
            btnCancel.Text = fPlugin.LangMan.LS(PLS.LSID_Cancel);
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
        }
    }
}
