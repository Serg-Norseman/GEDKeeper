/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.Runtime.InteropServices;

using GKCommon;
using GKCore;
using GKCore.Interfaces;
using GKCore.Plugins;

[assembly: AssemblyTitle("GKStdReports")]
[assembly: AssemblyDescription("GEDKeeper standard reports plugin")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2018 by Sergey V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: CLSCompliant(true)]
[assembly: ComVisible(false)]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyFileVersion("1.0.0.0")]

namespace GKStdReports
{
    public enum RLS
    {
        LSID_NFR_Title,
        LSID_PER_Title,
        LSID_Names,
        LSID_Surnames
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

    public class NFRPlugin : OrdinaryPlugin, IPlugin
    {
        public override string DisplayName { get { return SRLangMan.LS(RLS.LSID_NFR_Title); } }
        public override ILangMan LangMan { get { return SRLangMan.Instance; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Report; } }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            using (var report = new NamesFreqReport(curBase)) {
                report.Generate(true);
            }
        }

        public override void OnLanguageChange()
        {
            try {
                SRLangMan.Instance = Host.CreateLangMan(this);
            } catch (Exception ex) {
                Logger.LogWrite("NFRPlugin.OnLanguageChange(): " + ex.Message);
            }
        }
    }


    public class PERPlugin : OrdinaryPlugin, IPlugin
    {
        public override string DisplayName { get { return SRLangMan.LS(RLS.LSID_PER_Title); } }
        public override ILangMan LangMan { get { return SRLangMan.Instance; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Report; } }

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

        public override void OnLanguageChange()
        {
            try {
                SRLangMan.Instance = Host.CreateLangMan(this);
            } catch (Exception ex) {
                Logger.LogWrite("PERPlugin.OnLanguageChange(): " + ex.Message);
            }
        }
    }
}
