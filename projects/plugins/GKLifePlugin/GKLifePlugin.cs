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
using GKLifePlugin.ConwayLife;

[assembly: AssemblyTitle("GKLifePlugin")]
[assembly: AssemblyDescription("GEDKeeper Conway's Game of Life plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2011-2024 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

namespace GKLifePlugin
{
    public enum PLS
    {
        LifeGame = 1,
        Step,
        Start,
        Stop,
        SetCells,
        Clear,
        Random,
        Options,
        PatternStabilisedTitle,
        RepeatingPattern,
        StaticPattern,
        Generation,
        LivingCells,
    }

    public class Plugin : OrdinaryPlugin
    {
        private string fDisplayName = "Conway's Game of Life";
        private ILangMan fLangMan;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Common; } }

        public override void Execute()
        {
            LifeForm frm = new LifeForm(fLangMan);
            frm.Show();
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.LifeGame);
            } catch (Exception ex) {
                Logger.WriteError("GKLifePlugin.OnLanguageChange()", ex);
            }
        }
    }
}
