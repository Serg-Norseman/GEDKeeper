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
    public class TNRPlugin : OrdinaryPlugin, IPlugin
    {
        private const string DISPLAY_NAME = "Top Names Report";

        private ILangMan fLangMan;

        public override string DisplayName { get { return DISPLAY_NAME; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            using (var report = new TopNamesReport(curBase)) {
                report.Generate(true);
            }
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
            } catch (Exception ex) {
                Logger.LogWrite("TNRPlugin.OnLanguageChange(): " + ex.Message);
            }
        }
    }


    public class PEPlugin : OrdinaryPlugin, IPlugin
    {
        private const string DISPLAY_NAME = "Personal Events Report";

        private ILangMan fLangMan;

        public override string DisplayName { get { return DISPLAY_NAME; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            using (var report = new PersonalEventsReport(curBase)) {
                report.Generate(true);
            }
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
            } catch (Exception ex) {
                Logger.LogWrite("PEPlugin.OnLanguageChange(): " + ex.Message);
            }
        }
    }
}
