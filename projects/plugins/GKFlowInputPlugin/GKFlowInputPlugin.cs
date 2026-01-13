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
using GKCore.Plugins;

[assembly: AssemblyTitle("GKFlowInputPlugin")]
[assembly: AssemblyDescription("GEDKeeper FlowInput and PedigreeImporter plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2014-2026 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

namespace GKFlowInputPlugin
{
    public enum PLS
    {
        FlowInput = 1,
        SexM,
        SexF,
        Surname,
        Name,
        Patronymic,
        DlgClose,
        DlgAppend,
        Note,
        Source,
        Page,
        BirthDate,
        DeathDate,
        Father,
        Mother,
        Spouse,
        FullName,
        BirthPlace,
        DeathPlace,
        Age,
        Birth,
        Death,
        RK_Unk,
        InputSimple,
        InputSource,
        SourceKind,
        SK_Rev,
        SK_Met,
        Year,
        Settlement,
        EventDate,
        EventType,
        Join,
        Comment,
        PLPerson,
        PLGodparent,
        Child,
        NameInvalid,
        BasePersonInvalid,
        SourceYearInvalid,
        Marriage
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

#if !NETCOREAPP
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
                fDisplayName = fLangMan.LS(PLS.FlowInput);
            } catch (Exception ex) {
                Logger.WriteError("GKFlowInputPlugin.OnLanguageChange()", ex);
            }
        }
    }
}
