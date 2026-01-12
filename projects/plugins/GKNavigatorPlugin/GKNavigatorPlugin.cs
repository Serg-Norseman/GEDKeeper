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

[assembly: AssemblyTitle("GKNavigatorPlugin")]
[assembly: AssemblyDescription("GEDKeeper Navigator plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2016,2022-2025 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.2.0.0")]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

namespace GKNavigatorPlugin
{
    public enum PLS
    {
        Navigator = 1,
        RecentActivity,
        JumpHistory,
        PotencialProblems,
        Filters,
        Bookmarks,
        Languages,
        Records,
        Time,
        Name,
        Action,
        WebLinks,
    }

    public sealed class Plugin : WidgetPlugin, ISubscriber
    {
        private string fDisplayName = "GKNavigatorPlugin";
        private ILangMan fLangMan;
        private readonly NavigatorData fData;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Tool; } }

        private NavigatorWidget fForm;

        internal NavigatorData Data
        {
            get { return fData; }
        }

        public Plugin()
        {
            fData = new NavigatorData(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fForm != null) {
                    fForm.Dispose();
                    fForm = null;
                }
            }
            base.Dispose(disposing);
        }

        internal void CloseForm()
        {
            if (fForm != null) {
                fForm = null;
            }
        }

        public override void Execute()
        {
            if (!Host.IsWidgetActive(this)) {
                fForm = new NavigatorWidget(this);
                fForm.Show();
            } else {
                fForm.Close();
                fForm = null;
            }
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.Navigator);

                if (fForm != null) fForm.SetLocale();
            } catch (Exception ex) {
                Logger.WriteError("GKNavigatorPlugin.OnLanguageChange()", ex);
            }
        }

        public override bool Shutdown()
        {
            bool result = true;
            try {
                CloseForm();
            } catch (Exception ex) {
                Logger.WriteError("GKNavigatorPlugin.Shutdown()", ex);
                result = false;
            }
            return result;
        }

        public override void BaseChanged(IBaseWindow baseWin)
        {
            if (fForm != null) {
                fForm.BaseChanged(baseWin);
            }
        }

        public override void BaseClosed(IBaseWindow baseWin)
        {
            if (fForm != null) {
                fForm.BaseClosed(baseWin);
            }
        }

        public override void BaseRenamed(IBaseWindow baseWin, string oldName, string newName)
        {
            fData.RenameBase(oldName, newName);
        }

        public void NotifyRecord(IBaseWindow baseWin, object record, RecordAction action)
        {
            if (baseWin == null || record == null) return;

            string baseName = baseWin.Context.FileName;
            fData[baseName].NotifyRecord(baseWin, record, action);
        }

        public void NotifyFilter(IBaseWindow baseWin, GDMRecordType recType, IListSource listSource, ListFilter filter)
        {
        }
    }
}
