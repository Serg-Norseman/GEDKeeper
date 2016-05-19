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
using System.Reflection;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using GKCore.Interfaces;

[assembly: AssemblyTitle("GKTreeVizPlugin")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper2")]
[assembly: AssemblyCopyright("Copyright © 2014, Serg V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: CLSCompliant(false)]
[assembly: ComVisible(false)]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyFileVersion("1.0.0.0")]

namespace GKTreeVizPlugin
{
    public enum TVLS
    {
        LSID_DisplayName,
        LSID_WithoutDates,
        LSID_MinGens,
        LSID_Accept,
        LSID_Cancel
    }
    
    public sealed class Plugin : IPlugin
    {
        private string fDisplayName = "GKTreeVizPlugin";
        private IHost fHost;
        private ILangMan fLangMan;

        public string DisplayName { get { return this.fDisplayName; } }
        public IHost Host { get { return this.fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }

        public void Execute()
        {
            if (this.fHost.IsUnix()) {
                this.fHost.ShowWarning(@"This function is not supported in Linux");
                return;
            }

            using (TVSettingsDlg dlg = new TVSettingsDlg(this))
            {
                if (dlg.ShowDialog() == DialogResult.OK)
                {
                    IBaseWindow curBase = this.fHost.GetCurrentFile(true);

                    using (TreeVizViewer viewer = new TreeVizViewer(curBase, dlg.MinGens))
                    {
                        viewer.ShowDialog();
                    }
                }
            }
        }

        public void OnHostClosing(ref bool cancelClosing) {}
        public void OnHostActivate() {}
        public void OnHostDeactivate() {}

        public void OnLanguageChange()
        {
            try
            {
                this.fLangMan = this.fHost.CreateLangMan(this);
                this.fDisplayName = this.fLangMan.LS(TVLS.LSID_DisplayName);
            }
            catch (Exception ex)
            {
                fHost.LogWrite("GKTreeVizPlugin.OnLanguageChange(): " + ex.Message);
            }
        }

        public bool Startup(IHost host)
        {
            bool result = true;
            try
            {
                this.fHost = host;
                // Implement any startup code here
            }
            catch (Exception ex)
            {
                fHost.LogWrite("GKTreeVizPlugin.Startup(): " + ex.Message);
                result = false;
            }
            return result;
        }

        public bool Shutdown()
        {
            bool result = true;
            try
            {
                // Implement any shutdown code here
            }
            catch (Exception ex)
            {
                fHost.LogWrite("GKTreeVizPlugin.Shutdown(): " + ex.Message);
                result = false;
            }
            return result;
        }

    }
}
