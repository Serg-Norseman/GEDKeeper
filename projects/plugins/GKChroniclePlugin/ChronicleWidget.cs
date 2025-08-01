/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using GKCore.Design;
using GKCore.Locales;

namespace GKChroniclePlugin
{
    public partial class ChronicleWidget : Form, ILocalizable
    {
        private readonly Plugin fPlugin;

        public ChronicleWidget(Plugin plugin)
        {
            InitializeComponent();
            fPlugin = plugin;
            SetLocale();
        }

        private void WidgetForm_Load(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetShow(fPlugin);
            BaseChanged(fPlugin.Host.GetCurrentFile());
        }

        private void WidgetForm_Closed(object sender, EventArgs e)
        {
            BaseChanged(null);
            fPlugin.Host.WidgetClose(fPlugin);
        }

        public void BaseChanged(IBaseWindow baseWin)
        {
            fPlugin.SetLVBase(lvEvents, baseWin);
        }

        public void SetLocale()
        {
            Text = fPlugin.LangMan.LS(PLS.Chronicle);
        }
    }
}
