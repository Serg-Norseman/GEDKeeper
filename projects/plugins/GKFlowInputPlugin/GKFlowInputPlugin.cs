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
using GKCore.Design.Graphics;
using GKCore.Interfaces;
using GKCore.Plugins;

[assembly: AssemblyTitle("GKFlowInputPlugin")]
[assembly: AssemblyDescription("GEDKeeper FlowInput plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2014-2023 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

namespace GKFlowInputPlugin
{
    public enum FLS
    {
        LSID_PluginTitle,
        LSID_SexM,
        LSID_SexF,
        LSID_Surname,
        LSID_Name,
        LSID_Patronymic,
        LSID_DlgClose,
        LSID_DlgAppend,
        LSID_Note,
        LSID_Source,
        LSID_Page,
        LSID_BirthDate,
        LSID_DeathDate,
        LSID_Father,
        LSID_Mother,
        LSID_Spouse,
        LSID_FullName,
        LSID_BirthPlace,
        LSID_DeathPlace,
        LSID_Age,
        LSID_Birth,
        LSID_Death,
        LSID_RK_Unk,
        LSID_InputSimple,
        LSID_InputSource,
        LSID_SourceKind,
        LSID_SK_Rev,
        LSID_SK_Met,
        LSID_Year,
        LSID_Settlement,
        LSID_EventDate,
        LSID_EventType,
        LSID_Join,
        LSID_Comment,
        LSID_PLPerson,
        LSID_PLGodparent,
        LSID_Child,
        LSID_NameInvalid,
        LSID_BasePersonInvalid,
        LSID_SourceYearInvalid,
        LSID_Marriage
    }

    public class Plugin : OrdinaryPlugin
    {
        private string fDisplayName = "GKFlowInputPlugin";
        private ILangMan fLangMan;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Tool; } }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

#if !ETO
            using (FlowInputDlg frm = new FlowInputDlg(this, curBase)) {
                frm.ShowDialog();
            }
#else
            FlowInputDlg frm = new FlowInputDlg(this, curBase);
            frm.Show();
#endif
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(FLS.LSID_PluginTitle);
            } catch (Exception ex) {
                Logger.WriteError("GKFlowInputPlugin.OnLanguageChange()", ex);
            }
        }
    }
}
