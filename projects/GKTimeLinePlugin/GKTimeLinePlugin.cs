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
[assembly: AssemblyDescription("GEDKeeper2 TimeLine plugin")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper2")]
[assembly: AssemblyCopyright("Copyright © 2014, Sergey V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: CLSCompliant(false)]
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

        public string DisplayName { get { return this.fDisplayName; } }
        public IHost Host { get { return fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }

        private TimeLineWidget frm;

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (frm != null) frm.Dispose();
            }
            base.Dispose(disposing);
        }

        #region IPlugin support

        public void Execute()
        {
            if (!this.fHost.IsWidgetActive(this)) {
                frm = new TimeLineWidget(this);
                frm.Show();
            } else {
                frm.Close();
                frm = null;
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
                this.fDisplayName = this.fLangMan.LS(PLS.LSID_MITimeLine);

                if (frm != null) frm.SetLang();
            }
            catch (Exception ex)
            {
                fHost.LogWrite("GKTimeLinePlugin.OnLanguageChange(): " + ex.Message);
            }
        }
        
        public bool Startup(IHost host)
        {
            bool result = true;
            try
            {
                this.fHost = host;
            }
            catch (Exception ex)
            {
                fHost.LogWrite("GKTimeLinePlugin.Startup(): " + ex.Message);
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
                fHost.LogWrite("GKTimeLinePlugin.Shutdown(): " + ex.Message);
                result = false;
            }
            return result;
        }

        #endregion

        #region IWidget support

        void IWidget.WidgetInit(IHost host) {}

        void IWidget.BaseChanged(IBaseWindow baseWin)
        {
            if (frm != null) {
                frm.BaseChanged(baseWin);
            }
        }

        void IWidget.BaseClosed(IBaseWindow baseWin)
        {
            if (frm != null) {
                frm.BaseChanged(null);
            }
        }

        void IWidget.BaseRenamed(IBaseWindow baseWin, string oldName, string newName) {}
        void IWidget.WidgetEnable() {}

        #endregion
    }
}
