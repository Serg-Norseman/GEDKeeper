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

namespace GKTreeVizPlugin
{
    public partial class TVSettingsDlg : Form
    {
        private readonly IPlugin fPlugin;

        public int MinGens
        {
        	get { return decimal.ToInt32(this.edMinGens.Value); }
        }
        
        public TVSettingsDlg(IPlugin plugin)
        {
            InitializeComponent();
            
            this.fPlugin = plugin;
            this.Text = fPlugin.DisplayName;
            
            // SetLang()
        	this.chkWithoutDates.Text = fPlugin.LangMan.LS(TVLS.LSID_WithoutDates);
        	this.lblMinGens.Text = fPlugin.LangMan.LS(TVLS.LSID_MinGens);
        	this.btnAccept.Text = fPlugin.LangMan.LS(TVLS.LSID_Accept);
        	this.btnCancel.Text = fPlugin.LangMan.LS(TVLS.LSID_Cancel);
        }
        
        private void BtnAcceptClick(object sender, EventArgs e)
        {
        	
        }
    }
}
