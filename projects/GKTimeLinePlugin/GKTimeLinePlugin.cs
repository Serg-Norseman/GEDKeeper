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

using GKCommon;
using GKCore.Interfaces;

[assembly: AssemblyTitle("GKTimeLinePlugin")]
[assembly: AssemblyDescription("GEDKeeper TimeLine plugin")]
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

namespace GKTimeLinePlugin
{
    public enum PLS
    {
        /* 032 */ LSID_MITimeLine,
        /* 130 */ LSID_TimeScale,
        /* 131 */ LSID_CurrentYear,
    }

    public sealed class Plugin : BaseObject, IPlugin, IWidget
    {
        private string fDisplayName = "GKTimeLinePlugin";
        private IHost fHost;
        private ILangMan fLangMan;

        public string DisplayName { get { return fDisplayName; } }
        public IHost Host { get { return fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }

        private TimeLineWidget fForm;

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (fForm != null) fForm.Dispose();
            }
            base.Dispose(disposing);
        }

        #region IPlugin support

        public void Execute()
        {
            if (!fHost.IsWidgetActive(this)) {
                fForm = new TimeLineWidget(this);
                fForm.Show();
            } else {
                fForm.Close();
                fForm = null;
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
                fDisplayName = fLangMan.LS(PLS.LSID_MITimeLine);

                if (fForm != null) fForm.SetLang();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKTimeLinePlugin.OnLanguageChange(): " + ex.Message);
            }
        }
        
        public bool Startup(IHost host)
        {
            bool result = true;
            try
            {
                fHost = host;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKTimeLinePlugin.Startup(): " + ex.Message);
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
                Logger.LogWrite("GKTimeLinePlugin.Shutdown(): " + ex.Message);
                result = false;
            }
            return result;
        }

        #endregion

        #region IWidget support

        void IWidget.WidgetInit(IHost host) {}

        void IWidget.BaseChanged(IBaseWindow baseWin)
        {
            if (fForm != null) {
                fForm.BaseChanged(baseWin);
            }
        }

        void IWidget.BaseClosed(IBaseWindow baseWin)
        {
            if (fForm != null) {
                fForm.BaseChanged(null);
            }
        }

        void IWidget.BaseRenamed(IBaseWindow baseWin, string oldName, string newName) {}
        void IWidget.WidgetEnable() {}

        #endregion
    }
}
