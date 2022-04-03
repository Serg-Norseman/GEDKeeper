/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using System.IO;
using System.Text;
using GKCore.MVP;
using GKCore.MVP.Views;

namespace GKCore.Controllers
{
    /// <summary>
    ///
    /// </summary>
    public class ScriptEditWinController : DialogController<IScriptEditWin>
    {

        public ScriptEditWinController(IScriptEditWin view) : base(view)
        {
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

        public void LoadScript()
        {
            if (!fView.CheckModified()) return;

            string fileName = AppHost.StdDialogs.GetOpenFile("", "", LangMan.LS(LSID.LSID_ScriptsFilter), 1, GKData.LUA_EXT);
            if (string.IsNullOrEmpty(fileName)) return;

            using (StreamReader strd = new StreamReader(File.OpenRead(fileName), Encoding.UTF8)) {
                fView.ScriptText.Text = strd.ReadToEnd();
                fView.FileName = fileName;
                fView.Modified = false;
                strd.Close();
            }
        }

        public void SaveScript()
        {
            string fileName = AppHost.StdDialogs.GetSaveFile("", "", LangMan.LS(LSID.LSID_ScriptsFilter), 1, GKData.LUA_EXT, fView.FileName);
            if (string.IsNullOrEmpty(fileName)) return;

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
                var scrEngine = new ScriptEngine();
                scrEngine.lua_run(fView.ScriptText.Text, fBase, fView.DebugOutput);
            } catch (Exception ex) {
                Logger.WriteError("ScriptEditWin.Run()", ex);
            }
        }
    }
}
