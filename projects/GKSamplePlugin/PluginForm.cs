/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

namespace GKSamplePlugin
{
    public partial class PluginForm : Form
    {
        private readonly IPlugin plugin;

        public PluginForm(IPlugin plugin)
        {
            InitializeComponent();
            this.plugin = plugin;
        }

        private void frmP1Main_Load(object sender, EventArgs e)
        {
            tbInfo.AppendText(plugin.DisplayName + "\r\n");
        }
    }
}
