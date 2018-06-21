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
using System.Runtime.InteropServices;

using GKCore;
using GKCore.Interfaces;

[assembly: AssemblyTitle("GKSamplePlugin1")]
[assembly: AssemblyDescription("GEDKeeper sample plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2014 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyCulture("")]
[assembly: CLSCompliant(true)]
[assembly: ComVisible(false)]

namespace GKSamplePlugin
{
    public class Plugin : IPlugin
    {
        private const string DISPLAY_NAME = "GKSamplePlugin";

        private IHost fHost;
        private ILangMan fLangMan;

        public string DisplayName { get { return DISPLAY_NAME; } }
        public IHost Host { get { return fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }
        public IImage Icon { get { return null; } }
        public PluginCategory Category { get { return PluginCategory.Common; } }

        public void Execute()
        {
            using (PluginForm frm = new PluginForm(this)) {
                frm.ShowDialog();
            }
        }

        public void OnHostClosing(HostClosingEventArgs eventArgs) {}
        public void OnHostActivate() {}
        public void OnHostDeactivate() {}

        public void OnLanguageChange()
        {
            try
            {
                fLangMan = fHost.CreateLangMan(this);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKSamplePlugin1.OnLanguageChange(): " + ex.Message);
            }
        }

        public bool Startup(IHost host)
        {
            bool result = true;
            try
            {
                fHost = host;
                // Implement any startup code here
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKSamplePlugin1.Startup(): " + ex.Message);
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
                Logger.LogWrite("GKSamplePlugin1.Shutdown(): " + ex.Message);
                result = false;
            }
            return result;
        }
    }
}
