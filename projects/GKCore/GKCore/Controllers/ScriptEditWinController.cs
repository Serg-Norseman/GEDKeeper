/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

#if !MOBILE && !TERM

using System;
using System.IO;
using System.Text;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Options;
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
