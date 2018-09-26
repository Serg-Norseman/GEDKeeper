/*
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
using System.Reflection;
using System.Windows.Forms;
using GKCore;
using GKCore.Interfaces;
using GKCore.Plugins;

[assembly: AssemblyTitle("GKTreeVizPlugin")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2014 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyCulture("")]

namespace GKTreeVizPlugin
{
    public enum PLS
    {
        LSID_DisplayName,
        LSID_WithoutDates,
        LSID_MinGens,
        LSID_Accept,
        LSID_Cancel
    }

    public sealed class Plugin : OrdinaryPlugin
    {
        private string fDisplayName = "GKTreeVizPlugin";
        private ILangMan fLangMan;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Common; } }

        public override void Execute()
        {
            if (SysUtils.IsUnix()) {
                AppHost.StdDialogs.ShowWarning(@"This function is not supported in Linux");
                return;
            }

            using (TVSettingsDlg dlg = new TVSettingsDlg(this)) {
                if (dlg.ShowDialog() != DialogResult.OK)
                    return;

                IBaseWindow curBase = Host.GetCurrentFile(true);
                using (TreeVizViewer viewer = new TreeVizViewer(curBase, dlg.MinGens)) {
                    viewer.ShowDialog();
                }
            }
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.LSID_DisplayName);
            } catch (Exception ex) {
                Logger.LogWrite("GKTreeVizPlugin.OnLanguageChange(): " + ex.Message);
            }
        }
    }
}
