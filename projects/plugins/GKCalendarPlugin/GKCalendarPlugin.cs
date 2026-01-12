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

[assembly: AssemblyTitle("GKCalendarPlugin")]
[assembly: AssemblyDescription("GEDKeeper Calendar plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2014-2023 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.1.0.0")]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

namespace GKCalendarPlugin
{
    public enum PLS
    {
        Calendar = 1,

        unused_1,

        Cal_Gregorian = 3,
        Cal_Julian,
        Cal_Hebrew,
        Cal_Islamic,
        Cal_Persian,
        Cal_Indian,
        Cal_Bahai,

        BahaiMonths,
        BahaiWeekdays,
        ClassicMonths,
        ClassicWeekdays,
        HebrewMonths,
        HebrewWeekdays,
        IndianCivilMonths,
        IndianCivilWeekdays,
        IslamicMonths,
        IslamicWeekdays,
        PersianMonths,
        PersianWeekdays,
        BahaiCycles,

        Cal_Byzantine,
        ByzantineMonths,

        Day,
        Month,
        Year,
        SourceDate,
        ConvertedDate,
    }

    public sealed class Plugin : WidgetPlugin
    {
        private string fDisplayName = "GKCalendarPlugin";
        private ILangMan fLangMan;
        private IImage fIcon;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return fIcon; } }
        public override PluginCategory Category { get { return PluginCategory.Tool; } }

        private CalendarWidget fForm;

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
                fForm = new CalendarWidget(this);
                fForm.Show();
            } else {
                fForm.Close();
            }
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.Calendar);

                if (fForm != null) fForm.SetLocale();
            } catch (Exception ex) {
                Logger.WriteError("GKCalendarPlugin.OnLanguageChange()", ex);
            }
        }

        public override bool Startup(IHost host)
        {
            bool result = base.Startup(host);
            try {
                fIcon = AppHost.GfxProvider.LoadResourceImage(this.GetType(), "GKCalendarPlugin.Resources.icon_time.gif", ImageTarget.UI);
            } catch (Exception ex) {
                Logger.WriteError("GKCalendarPlugin.Startup()", ex);
                result = false;
            }
            return result;
        }
    }
}
