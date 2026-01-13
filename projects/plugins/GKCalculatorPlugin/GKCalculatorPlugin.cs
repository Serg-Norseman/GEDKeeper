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

[assembly: AssemblyTitle("GKCalculatorPlugin")]
[assembly: AssemblyDescription("GEDKeeper Calculator plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2014-2026 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("2.0.0.0")]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

namespace GKCalculatorPlugin
{
    public enum PLS
    {
        Calculator = 1,
        CopyResultToClipboard,
        EventsYearCalculation,
    }

    public sealed class Plugin : WidgetPlugin
    {
        private string fDisplayName = "GKCalculatorPlugin";
        private ILangMan fLangMan;
        private IImage fIcon;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return fIcon; } }
        public override PluginCategory Category { get { return PluginCategory.Tool; } }

        private CalcWidget fForm;

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
                fForm = new CalcWidget(this);
                fForm.Show();
            } else {
                fForm.Close();
            }
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.Calculator);

                if (fForm != null) fForm.SetLocale();
            } catch (Exception ex) {
                Logger.WriteError("GKCalculatorPlugin.OnLanguageChange()", ex);
            }
        }

        public override bool Startup(IHost host)
        {
            bool result = base.Startup(host);
            try {
                fIcon = AppHost.GfxProvider.LoadResourceImage(this.GetType(), "GKCalculatorPlugin.Resources.icon_calc.gif", ImageTarget.UI);
            } catch (Exception ex) {
                Logger.WriteError("GKCalculatorPlugin.Startup()", ex);
                result = false;
            }
            return result;
        }

        public override void WidgetEnable()
        {
            if (fForm != null) {
                Host.EnableWindow(fForm, true);
            }
        }
    }
}
