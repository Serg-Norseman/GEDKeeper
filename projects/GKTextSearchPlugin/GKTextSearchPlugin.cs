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
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;

[assembly: AssemblyTitle("GKTextSearchPlugin")]
[assembly: AssemblyDescription("GEDKeeper TextSearch plugin")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2014 by Sergey V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: CLSCompliant(false)]
[assembly: ComVisible(false)]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyFileVersion("1.0.0.0")]

namespace GKTextSearchPlugin
{
    public enum TLS
    {
        LSID_PluginTitle,
        LSID_SearchIndexRefreshing,
        LSID_SearchResults,
        LSID_Search
    }

    public sealed class Plugin : BaseObject, IPlugin, ISubscriber
    {
        private string fDisplayName = "GKTextSearchPlugin";
        private IHost fHost;
        private ILangMan fLangMan;
        private SearchManager fSearchMan;

        public string DisplayName { get { return this.fDisplayName; } }
        public IHost Host { get { return this.fHost; } }
        public ILangMan LangMan { get { return this.fLangMan; } }
        public SearchManager SearchMan { get { return this.fSearchMan; } }

        internal TextSearchWin tsWin;

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (tsWin != null) tsWin.Dispose();
            }
            base.Dispose(disposing);
        }

        #region IPlugin support

        public void Execute()
        {
            if (this.fHost.IsUnix()) {
                this.fHost.ShowWarning(@"This function is not supported in Linux");
                return;
            }

            IBaseWindow curBase = fHost.GetCurrentFile();
            if (curBase == null) return;

            tsWin = new TextSearchWin(this, curBase);
            tsWin.Show();
        }
        
        public void OnHostClosing(ref bool cancelClosing) {}
        public void OnHostActivate() {}
        public void OnHostDeactivate() {}

        public void OnLanguageChange()
        {
            try
            {
                this.fLangMan = this.fHost.CreateLangMan(this);
                this.fDisplayName = this.fLangMan.LS(TLS.LSID_PluginTitle);

                if (tsWin != null) tsWin.SetLang();
            }
            catch (Exception ex)
            {
                fHost.LogWrite("GKTextSearchPlugin.OnLanguageChange(): " + ex.Message);
            }
        }
        
        public bool Startup(IHost host)
        {
            bool result = true;
            try
            {
                this.fHost = host;
                this.fSearchMan = new SearchManager(this);
            }
            catch (Exception ex)
            {
                fHost.LogWrite("GKTextSearchPlugin.Startup(): " + ex.Message);
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
                //if (this.fSearchMan != null) this.fSearchMan.Dispose();
            }
            catch (Exception ex)
            {
                fHost.LogWrite("GKTextSearchPlugin.Shutdown(): " + ex.Message);
                result = false;
            }
            return result;
        }

        #endregion

        #region ISubscriber support

        public void NotifyRecord(IBaseWindow baseWin, object record, RecordAction action)
        {
            #if !__MonoCS__
            if (baseWin == null || record == null || this.fSearchMan == null) return;

            switch (action) {
                case RecordAction.raEdit:
                    this.fSearchMan.UpdateRecord(baseWin, (GEDCOMRecord)record);
                    break;

                case RecordAction.raDelete:
                    this.fSearchMan.DeleteRecord(baseWin, ((GEDCOMRecord)record).XRef);
                    break;
            }
            #endif
        }

        #endregion
    }
}
