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
using BSLib.Design.Graphics;
using GKCore;
using GKCore.Interfaces;
using GKCore.Plugins;

[assembly: AssemblyTitle("GKStdReports")]
[assembly: AssemblyDescription("GEDKeeper standard reports plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2018-2021 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("0.6.0.0")]
[assembly: AssemblyCulture("")]

namespace GKStdReports
{
    public enum RLS
    {
        LSID_NFR_Title,
        LSID_PER_Title,
        LSID_Names,
        LSID_Surnames,
        LSID_Phonetics_Title,
        LSID_Contemporaries_Title,
        LSID_Sources_Title,
        LSID_Name,
        LSID_Title,
        LSID_Repositories_Title,
        LSID_Address,
        LSID_Places_Title,
        LSID_Repository,
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


    public abstract class StdReportPlugin : OrdinaryPlugin, IPlugin
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
        public override string DisplayName { get { return SRLangMan.LS(RLS.LSID_NFR_Title); } }

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
        public override string DisplayName { get { return SRLangMan.LS(RLS.LSID_PER_Title); } }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            var selPerson = curBase.GetSelectedPerson();
            if (selPerson == null) {
                AppHost.StdDialogs.ShowError(GKCore.LangMan.LS(LSID.LSID_NotSelectedPerson));
                return;
            }

            using (var report = new PersonalEventsReport(curBase, selPerson)) {
                report.Generate(true);
            }
        }
    }


    public class PhonPlugin : StdReportPlugin
    {
        public override string DisplayName { get { return SRLangMan.LS(RLS.LSID_Phonetics_Title); } }

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
        public override string DisplayName { get { return SRLangMan.LS(RLS.LSID_Contemporaries_Title); } }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            var selPerson = curBase.GetSelectedPerson();
            if (selPerson == null) {
                AppHost.StdDialogs.ShowError(GKCore.LangMan.LS(LSID.LSID_NotSelectedPerson));
                return;
            }

            using (var report = new ContemporariesReport(curBase, selPerson)) {
                report.Generate(true);
            }
        }
    }


    public class SourcesPlugin : StdReportPlugin
    {
        public override string DisplayName { get { return SRLangMan.LS(RLS.LSID_Sources_Title); } }

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
        public override string DisplayName { get { return SRLangMan.LS(RLS.LSID_Repositories_Title); } }

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
        public override string DisplayName { get { return SRLangMan.LS(RLS.LSID_Places_Title); } }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            using (var report = new PlacesReport(curBase)) {
                report.Generate(true);
            }
        }
    }
}
