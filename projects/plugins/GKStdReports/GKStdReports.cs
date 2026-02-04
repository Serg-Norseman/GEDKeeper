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
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Locales;
using GKCore.Options;
using GKCore.Plugins;

[assembly: AssemblyTitle("GKStdReports")]
[assembly: AssemblyDescription("GEDKeeper standard reports plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2018-2026 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("0.8.0.0")]
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

        SearchIndexReport,
        EarliestPlace,
    }


    public static class SRLangMan
    {
        private static ILangMan fInstance;

        public static ILangMan Instance
        {
            get { return fInstance; }
            internal set { fInstance = value; }
        }

        public static string LS(Enum lsid)
        {
            return (fInstance == null) ? string.Empty : fInstance.LS(lsid);
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


    public class SearchIndexPlugin : StdReportPlugin
    {
        public override string DisplayName { get { return SRLangMan.LS(PLS.SearchIndexReport); } }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            using (var report = new SearchIndexReport(curBase)) {
                report.Options = GlobalOptions.Instance;
                report.Generate(true);
            }
        }
    }
}
