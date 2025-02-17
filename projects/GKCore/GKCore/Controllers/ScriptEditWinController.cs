/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

#if !MOBILE

using System;
using System.IO;
using System.Text;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Options;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    ///
    /// </summary>
    public class ScriptEditWinController : DialogController<IScriptEditWin>
    {
        private readonly ScriptEngine fScriptEngine;


        public ScriptEditWinController(IScriptEditWin view) : base(view)
        {
            fScriptEngine = new ScriptEngine(fView);
        }

        public override void UpdateView()
        {
        }

        public void NewScript()
        {
            if (!fView.CheckModified()) return;

            fView.ScriptText.Clear();
            fView.FileName = "unknown.lua";
            fView.Modified = false;
        }

        public async void LoadScript()
        {
            if (!fView.CheckModified()) return;

            string fileName = await AppHost.StdDialogs.GetOpenFile("", GlobalOptions.Instance.ScriptsLastDir, LangMan.LS(LSID.ScriptsFilter), 1, GKData.LUA_EXT);
            if (string.IsNullOrEmpty(fileName)) return;

            GlobalOptions.Instance.ScriptsLastDir = Path.GetDirectoryName(fileName);

            using (StreamReader strd = new StreamReader(File.OpenRead(fileName), Encoding.UTF8)) {
                fView.ScriptText.Text = strd.ReadToEnd();
                fView.FileName = fileName;
                fView.Modified = false;
                strd.Close();
            }
        }

        public async void SaveScript()
        {
            string fileName = await AppHost.StdDialogs.GetSaveFile("", GlobalOptions.Instance.ScriptsLastDir, LangMan.LS(LSID.ScriptsFilter), 1, GKData.LUA_EXT, fView.FileName);
            if (string.IsNullOrEmpty(fileName)) return;

            GlobalOptions.Instance.ScriptsLastDir = Path.GetDirectoryName(fileName);

            using (StreamWriter strd = new StreamWriter(fileName, false, Encoding.UTF8)) {
                strd.Write(fView.ScriptText.Text);
                fView.FileName = fileName;
                fView.Modified = false;
                strd.Close();
            }
        }

        public void RunScript()
        {
            try {
                fView.DebugOutput.Clear();
                fScriptEngine.lua_run(fView.ScriptText.Text, fBase);
            } catch (Exception ex) {
                Logger.WriteError("ScriptEditWin.Run()", ex);
            }
        }

        public override void SetLocale()
        {
            SetToolTip("tbNewScript", LangMan.LS(LSID.NewScriptTip));
            SetToolTip("tbLoadScript", LangMan.LS(LSID.LoadScriptTip));
            SetToolTip("tbSaveScript", LangMan.LS(LSID.SaveScriptTip));
            SetToolTip("tbRun", LangMan.LS(LSID.RunScriptTip));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IToolItem>("tbNewScript").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_FileNew, true);
            GetControl<IToolItem>("tbLoadScript").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_FileLoad, true);
            GetControl<IToolItem>("tbSaveScript").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_FileSave, true);
            GetControl<IToolItem>("tbRun").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Start, true);
        }
    }
}

#endif
