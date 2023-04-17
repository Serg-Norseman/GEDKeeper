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
using GKLifePlugin.ConwayLife;

[assembly: AssemblyTitle("GKLifePlugin")]
[assembly: AssemblyDescription("GEDKeeper Conway's Game of Life plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2011-2015 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyCulture("")]

namespace GKLifePlugin
{
    public enum PLS
    {
        LSID_LifeGame,
        LSID_Step,
        LSID_Start,
        LSID_Stop,
        LSID_SetCells,
        LSID_Clear,
        LSID_Random,
        LSID_Options,
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
            using (LifeForm frm = new LifeForm()) {
                frm.Viewer.Options.LS_LifeGame = fLangMan.LS(PLS.LSID_LifeGame);
                frm.Viewer.Options.LS_Step = fLangMan.LS(PLS.LSID_Step);
                frm.Viewer.Options.LS_Start = fLangMan.LS(PLS.LSID_Start);
                frm.Viewer.Options.LS_Stop = fLangMan.LS(PLS.LSID_Stop);
                frm.Viewer.Options.LS_SetCells = fLangMan.LS(PLS.LSID_SetCells);
                frm.Viewer.Options.LS_Clear = fLangMan.LS(PLS.LSID_Clear);
                frm.Viewer.Options.LS_Random = fLangMan.LS(PLS.LSID_Random);
                frm.Viewer.Options.LS_Options = fLangMan.LS(PLS.LSID_Options);

                frm.ShowDialog();
            }
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.LSID_LifeGame);
            } catch (Exception ex) {
                Logger.WriteError("GKLifePlugin.OnLanguageChange()", ex);
            }
        }
    }
}
