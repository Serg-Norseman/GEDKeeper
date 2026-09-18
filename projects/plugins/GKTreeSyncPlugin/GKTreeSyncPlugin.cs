/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Reflection;
using GKCore;
using GKCore.Charts;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Locales;
using GKCore.Plugins;
using GKCore.Utilities;

[assembly: AssemblyTitle("GKTreeSyncPlugin")]
[assembly: AssemblyDescription("GEDKeeper Tree Synchronization plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2024 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("0.1.0.0")]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

namespace GKTreeSyncPlugin
{
    public enum PLS
    {
        Title = 1,
    }

    public sealed class Plugin : WidgetPlugin
    {
        private string fDisplayName = "GKTreeSyncPlugin";
        private ILangMan fLangMan;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Tool; } }

        private TSForm fForm;

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fForm != null) fForm.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            if (fForm != null) fForm.Dispose();
            fForm = new TSForm(this, curBase);
            fForm.Show();
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                //fDisplayName = fLangMan.LS(PLS.Title);

                if (fForm != null) fForm.SetLocale();
            } catch (Exception ex) {
                Logger.WriteError("GKTreeSyncPlugin.OnLanguageChange()", ex);
            }
        }

        public override void BaseChanged(IBaseWindow baseWin)
        {
            /*if (fForm != null) {
                fForm.BaseChanged(baseWin);
            }*/
        }

        public override void BaseClosed(IBaseWindow baseWin)
        {
            /*if (fForm != null) {
                fForm.BaseChanged(null);
            }*/
        }

        internal static IColor GetDiffColor(DiffStatus diffStatus)
        {
            int backColor;
            switch (diffStatus) {
                case DiffStatus.Equal:
                default:
                    backColor = GKColors.White;
                    break;
                case DiffStatus.Deleted:
                    backColor = GKColors.Coral;
                    break;
                case DiffStatus.Inserted:
                    backColor = GKColors.LightBlue;
                    break;
                case DiffStatus.Modified:
                    backColor = GKColors.Yellow;
                    break;
                case DiffStatus.DeepModified:
                    backColor = GKColors.Orange;
                    break;
            }
            return ChartRenderer.GetColor(backColor);
        }
    }
}
