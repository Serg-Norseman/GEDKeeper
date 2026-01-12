/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Reflection;
using GDModel;
using GKCore;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Filters;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Plugins;
using GKCore.Utilities;

[assembly: AssemblyTitle("GKTextSearchPlugin")]
[assembly: AssemblyDescription("GEDKeeper TextSearch plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2014-2023 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

namespace GKTextSearchPlugin
{
    public enum PLS
    {
        TextSearch = 1,
        SearchIndexRefreshing,
        SearchResults,
        Search
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
                fDisplayName = fLangMan.LS(PLS.TextSearch);

                if (fForm != null) fForm.SetLocale();
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
            if (baseWin == null || record == null || fSearchMan == null) return;

            switch (action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit:
                    fSearchMan.UpdateRecord(baseWin, (GDMRecord)record);
                    break;

                case RecordAction.raDelete:
                    fSearchMan.DeleteRecord(baseWin, (GDMRecord)record);
                    break;
            }
        }

        public void NotifyFilter(IBaseWindow baseWin, GDMRecordType recType, IListSource listSource, ListFilter filter)
        {
            // dummy
        }

        #endregion
    }
}
