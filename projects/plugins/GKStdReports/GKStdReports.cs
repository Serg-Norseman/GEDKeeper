/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Locales;
using GKCore.Options;
using GKCore.Plugins;

[assembly: AssemblyTitle("GKStdReports")]
[assembly: AssemblyDescription("GEDKeeper standard reports plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2018-2023 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("0.7.0.0")]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

namespace GKStdReports
{
    public enum PLS
    {
        NamesFreqReport = 1,
        PersonalEventsReport,
        Names,
        Surnames,
        PhoneticsReport,
        ContemporariesReport,
        SourcesReport,
        Name,
        Title,
        RepositoriesReport,
        Address,
        PlacesReport,
        Repository,
        AncestorStatisticsReport,
        RootIndividual,
        ImplexFactor,
        ConsanguinityFactor,
        Generation,
        Possible,
        Known,
        KnownPercent,
        Cumul,
        CumulPercent,
        Diff,
        Implex,
        CommonAncestors,
        ConsanguinityCommonAncestors,
        RecordCardReport,
        NotSelectedRecord,
    }


    public static class SRLangMan
    {
        private static ILangMan fInstance;

        public static ILangMan Instance
        {
            get { return fInstance; }
            set {
                // TODO: for lang changes
                //if (fInstance == null /* finstance.Lang != value.Lang */) {
                    fInstance = value;
                //}
            }
        }

        public static string LS(Enum lsid)
        {
            return (fInstance == null) ? "" : fInstance.LS(lsid);
        }
    }


    public abstract class StdReportPlugin : OrdinaryPlugin
    {
        public override ILangMan LangMan { get { return SRLangMan.Instance; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Report; } }

        public override void OnLanguageChange()
        {
            try {
                SRLangMan.Instance = Host.CreateLangMan(this);
            } catch (Exception ex) {
                Logger.WriteError("StdReportPlugin.OnLanguageChange()", ex);
            }
        }
    }


    public class NFRPlugin : StdReportPlugin
    {
        public override string DisplayName { get { return SRLangMan.LS(PLS.NamesFreqReport); } }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            using (var report = new NamesFreqReport(curBase)) {
                report.Generate(true);
            }
        }
    }


    public class PERPlugin : StdReportPlugin
    {
        public override string DisplayName { get { return SRLangMan.LS(PLS.PersonalEventsReport); } }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            var selPerson = curBase.GetSelectedPerson();
            if (selPerson == null) {
                AppHost.StdDialogs.ShowError(GKCore.Locales.LangMan.LS(LSID.NotSelectedPerson));
                return;
            }

            using (var report = new PersonalEventsReport(curBase, selPerson)) {
                report.Generate(true);
            }
        }
    }


    public class PhonPlugin : StdReportPlugin
    {
        public override string DisplayName { get { return SRLangMan.LS(PLS.PhoneticsReport); } }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            using (var report = new PhoneticsReport(curBase)) {
                report.Generate(true);
            }
        }
    }


    public class ContempPlugin : StdReportPlugin
    {
        public override string DisplayName { get { return SRLangMan.LS(PLS.ContemporariesReport); } }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            var selPerson = curBase.GetSelectedPerson();
            if (selPerson == null) {
                AppHost.StdDialogs.ShowError(GKCore.Locales.LangMan.LS(LSID.NotSelectedPerson));
                return;
            }

            using (var report = new ContemporariesReport(curBase, selPerson)) {
                report.Generate(true);
            }
        }
    }


    public class SourcesPlugin : StdReportPlugin
    {
        public override string DisplayName { get { return SRLangMan.LS(PLS.SourcesReport); } }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            using (var report = new SourcesReport(curBase)) {
                report.Generate(true);
            }
        }
    }


    public class RepositoriesPlugin : StdReportPlugin
    {
        public override string DisplayName { get { return SRLangMan.LS(PLS.RepositoriesReport); } }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            using (var report = new RepositoriesReport(curBase)) {
                report.Generate(true);
            }
        }
    }


    public class PlacesPlugin : StdReportPlugin
    {
        public override string DisplayName { get { return SRLangMan.LS(PLS.PlacesReport); } }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            using (var report = new PlacesReport(curBase)) {
                report.Generate(true);
            }
        }
    }


    public class AncStatsPlugin : StdReportPlugin
    {
        public override string DisplayName { get { return SRLangMan.LS(PLS.AncestorStatisticsReport); } }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            var selPerson = curBase.GetSelectedPerson();
            if (selPerson == null) {
                AppHost.StdDialogs.ShowError(GKCore.Locales.LangMan.LS(LSID.NotSelectedPerson));
                return;
            }

            using (var report = new AncestorStatisticsReport(curBase, selPerson)) {
                report.Generate(true);
            }
        }
    }


    public class RecCardPlugin : StdReportPlugin
    {
        public override string DisplayName { get { return SRLangMan.LS(PLS.RecordCardReport); } }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            var selRecord = curBase.GetSelectedRecordEx();
            if (selRecord == null) {
                AppHost.StdDialogs.ShowError(SRLangMan.LS(PLS.NotSelectedRecord));
                return;
            }

            using (var report = new RecordCardReport(curBase, selRecord)) {
                report.Options = GlobalOptions.Instance;
                report.Generate(true);
            }
        }
    }
}
