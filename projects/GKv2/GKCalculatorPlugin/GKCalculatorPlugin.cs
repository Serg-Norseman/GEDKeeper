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
using System.Drawing;
using System.IO;
using System.Reflection;
using GKCore;
using GKCore.Interfaces;
using GKCore.Plugins;
using GKUI.Components;

[assembly: AssemblyTitle("GKCalculatorPlugin")]
[assembly: AssemblyDescription("GEDKeeper Calculator plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2014 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyCulture("")]

namespace GKCalculatorPlugin
{
    public enum PLS
    {
        /* 031 */ LSID_MICalc,
        /* 167 */ LSID_CopyResultToClipboard,
    }

    public sealed class Plugin : WidgetPlugin
    {
        private string fDisplayName = "GKCalculatorPlugin";
        private ILangMan fLangMan;
        private IImage fIcon;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return fIcon; } }
        public override PluginCategory Category { get { return PluginCategory.Tool; } }

        private CalcWidget fForm;

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
                fForm = new CalcWidget(this);
                fForm.Show();
            } else {
                fForm.Close();
            }
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.LSID_MICalc);

                if (fForm != null) fForm.SetLang();
            } catch (Exception ex) {
                Logger.LogException(ex);
            }
        }

        public override bool Startup(IHost host)
        {
            bool result = base.Startup(host);
            try {
                Assembly assembly = typeof(Plugin).Assembly;
                using (Stream stmIcon = assembly.GetManifestResourceStream("Resources.icon_calc.gif")) {
                    Image bmp = Image.FromStream(stmIcon);
                    fIcon = new ImageHandler(bmp);
                }
            } catch (Exception ex) {
                Logger.LogException(ex);
                result = false;
            }
            return result;
        }

        public override void WidgetEnable()
        {
            if (fForm != null) {
                Host.EnableWindow(fForm, true);
            }
        }
    }
}
