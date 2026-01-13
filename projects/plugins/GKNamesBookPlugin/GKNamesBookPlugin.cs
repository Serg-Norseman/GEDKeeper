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
using GKCore.Design.Graphics;
using GKCore.Locales;
using GKCore.Plugins;

[assembly: AssemblyTitle("GKNamesBookPlugin")]
[assembly: AssemblyDescription("GEDKeeper NamesBook plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2014-2026 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

namespace GKNamesBookPlugin
{
    public enum PLS
    {
        NamesBook = 1,
        Calendar
    }

    public sealed class Plugin : WidgetPlugin
    {
        private string fDisplayName = "GKNamesBookPlugin";
        private ILangMan fLangMan;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Common; } }

        private NamesBookWidget fForm;
        
        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fForm != null) fForm.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void Execute()
        {
            if (!Host.IsWidgetActive(this)) {
                fForm = new NamesBookWidget(this);
                fForm.Show();
            } else {
                fForm.Close();
            }
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.NamesBook);

                if (fForm != null) fForm.SetLocale();
            } catch (Exception ex) {
                Logger.WriteError("GKNamesBookPlugin.OnLanguageChange()", ex);
            }
        }
    }
}
