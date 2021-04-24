/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using GKCore;
using GKCore.Interfaces;
using GKCore.Plugins;
using GKCore.Types;

[assembly: AssemblyTitle("GKTextSearchPlugin")]
[assembly: AssemblyDescription("GEDKeeper TextSearch plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2014 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyCulture("")]

namespace GKTextSearchPlugin
{
    using BSLib.Design.Graphics;
    using GDModel;

    public enum TLS
    {
        LSID_PluginTitle,
        LSID_SearchIndexRefreshing,
        LSID_SearchResults,
        LSID_Search
    }

    public sealed class Plugin : OrdinaryPlugin, ISubscriber
    {
        private string fDisplayName = "GKTextSearchPlugin";
        private ILangMan fLangMan;
        private SearchManager fSearchMan;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Common; } }

        public SearchManager SearchMan { get { return fSearchMan; } }

        internal TextSearchWin fForm;

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fForm != null) fForm.Dispose();
            }
            base.Dispose(disposing);
        }

        #region IPlugin support

        public override void Execute()
        {
            if (SysUtils.IsUnix()) {
                AppHost.StdDialogs.ShowWarning(@"This function is not supported in Linux");
                return;
            }

            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            fForm = new TextSearchWin(this, curBase);
            fForm.Show();
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(TLS.LSID_PluginTitle);

                if (fForm != null) fForm.SetLang();
            } catch (Exception ex) {
                Logger.WriteError("GKTextSearchPlugin.OnLanguageChange()", ex);
            }
        }

        public override bool Startup(IHost host)
        {
            bool result = base.Startup(host);
            try {
                if (result) {
                    fSearchMan = new SearchManager(this);
                }
            } catch (Exception ex) {
                Logger.WriteError("GKTextSearchPlugin.Startup()", ex);
                result = false;
            }
            return result;
        }

        public override bool Shutdown()
        {
            bool result = base.Shutdown();
            try {
                //if (this.fSearchMan != null) this.fSearchMan.Dispose();
            } catch (Exception ex) {
                Logger.WriteError("GKTextSearchPlugin.Shutdown()", ex);
                result = false;
            }
            return result;
        }

        #endregion

        #region ISubscriber support

        public void NotifyRecord(IBaseWindow baseWin, object record, RecordAction action)
        {
            #if !__MonoCS__
            if (baseWin == null || record == null || fSearchMan == null) return;

            switch (action) {
                case RecordAction.raEdit:
                    fSearchMan.UpdateRecord(baseWin, (GDMRecord)record);
                    break;

                case RecordAction.raDelete:
                    fSearchMan.DeleteRecord(baseWin, ((GDMRecord)record).XRef);
                    break;
            }
            #endif
        }

        #endregion
    }
}
