﻿/*
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
using GKCore;
using GKCore.Interfaces;
using GKCore.Plugins;

[assembly: AssemblyTitle("GKTimeLinePlugin")]
[assembly: AssemblyDescription("GEDKeeper TimeLine plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2014 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyCulture("")]

namespace GKTimeLinePlugin
{
    public enum PLS
    {
        /* 032 */ LSID_MITimeLine,
        /* 130 */ LSID_TimeScale,
        /* 131 */ LSID_CurrentYear,
    }

    public sealed class Plugin : WidgetPlugin
    {
        private string fDisplayName = "GKTimeLinePlugin";
        private ILangMan fLangMan;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Tool; } }

        private TimeLineWidget fForm;

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
                fForm = new TimeLineWidget(this);
                fForm.Show();
            } else {
                fForm.Close();
                fForm = null;
            }
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.LSID_MITimeLine);

                if (fForm != null) fForm.SetLang();
            } catch (Exception ex) {
                Logger.LogWrite("GKTimeLinePlugin.OnLanguageChange(): " + ex.Message);
            }
        }

        public override void BaseChanged(IBaseWindow baseWin)
        {
            if (fForm != null) {
                fForm.BaseChanged(baseWin);
            }
        }

        public override void BaseClosed(IBaseWindow baseWin)
        {
            if (fForm != null) {
                fForm.BaseChanged(null);
            }
        }
    }
}
