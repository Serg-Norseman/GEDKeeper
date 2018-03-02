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
using System.Drawing;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;

using BSLib;
using GKCore;
using GKCore.Interfaces;
using GKUI.Components;

[assembly: AssemblyTitle("GKCalculatorPlugin")]
[assembly: AssemblyDescription("GEDKeeper Calculator plugin")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2014 by Sergey V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: CLSCompliant(true)]
[assembly: ComVisible(false)]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyFileVersion("1.0.0.0")]

namespace GKCalculatorPlugin
{
    public enum PLS
    {
        /* 031 */ LSID_MICalc,
        /* 167 */ LSID_CopyResultToClipboard,
    }

    public sealed class Plugin : BaseObject, IPlugin, IWidget
    {
        private string fDisplayName = "GKCalculatorPlugin";
        private IHost fHost;
        private ILangMan fLangMan;
        private IImage fIcon;

        public string DisplayName { get { return fDisplayName; } }
        public IHost Host { get { return fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }
        public IImage Icon { get { return fIcon; } }
        public PluginCategory Category { get { return PluginCategory.Tool; } }

        private CalcWidget fForm;

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (fForm != null) fForm.Dispose();
            }
            base.Dispose(disposing);
        }

        public void Execute()
        {
            if (!fHost.IsWidgetActive(this)) {
                fForm = new CalcWidget(this);
                fForm.Show();
            } else {
                fForm.Close();
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
                fDisplayName = fLangMan.LS(PLS.LSID_MICalc);

                if (fForm != null) fForm.SetLang();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKCalculatorPlugin.OnLanguageChange(): " + ex.Message);
            }
        }

        public bool Startup(IHost host)
        {
            bool result = true;
            try
            {
                fHost = host;

                Assembly assembly = typeof(Plugin).Assembly;
                using (Stream stmIcon = assembly.GetManifestResourceStream("Resources.icon_calc.gif"))
                {
                    Image bmp = Image.FromStream(stmIcon);
                    fIcon = new ImageHandler(bmp);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKCalculatorPlugin.Startup(): " + ex.Message);
                result = false;
            }
            return result;
        }

        public bool Shutdown()
        {
            bool result = true;
            try
            {
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKCalculatorPlugin.Shutdown(): " + ex.Message);
                result = false;
            }
            return result;
        }

        #region IWidget common

        void IWidget.WidgetInit(IHost host) {}
        void IWidget.BaseChanged(IBaseWindow baseWin) {}
        void IWidget.BaseClosed(IBaseWindow baseWin) {}
        void IWidget.BaseRenamed(IBaseWindow baseWin, string oldName, string newName) {}

        void IWidget.WidgetEnable()
        {
            if (fForm != null) {
                fHost.EnableWindow(fForm, true);
            }
        }

        #endregion
    }
}
