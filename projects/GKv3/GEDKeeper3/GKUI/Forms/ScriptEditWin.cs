/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using System.ComponentModel;
using System.IO;
using System.Text;
using Eto.Forms;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class ScriptEditWin : CommonDialog, ILocalization, IScriptEditWin
    {
        private readonly IBaseWindow fBase;

        private string fFileName;
        private bool fModified;

        public string FileName
        {
            get {
                return fFileName;
            }
            set {
                fFileName = value;
                SetTitle();
            }
        }

        public bool Modified
        {
            get {
                return fModified;
            }
            set {
                fModified = value;
                SetTitle();
            }
        }

        #region View Interface

        //IScriptEditWin.
        public ITextBoxHandler ScriptText
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtScriptText); }
        }

        //IScriptEditWin.
        public ITextBoxHandler DebugOutput
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtDebugOutput); }
        }

        #endregion

        private bool CheckModified()
        {
            bool result = true;
            if (!Modified) return result;

            DialogResult dialogResult = MessageBox.Show(LangMan.LS(LSID.LSID_FileSaveQuery), GKData.APP_TITLE, MessageBoxButtons.YesNoCancel, MessageBoxType.Question);
            switch (dialogResult) {
                case DialogResult.Yes:
                    tbSaveScript_Click(this, null);
                    break;

                case DialogResult.No:
                    break;

                case DialogResult.Cancel:
                    result = false;
                    break;
            }

            return result;
        }

        private void SetTitle()
        {
            Title = Path.GetFileName(fFileName);
            if (fModified) {
                Title = @"* " + Title;
            }
        }

        private void tbNewScript_Click(object sender, EventArgs e)
        {
            if (!CheckModified()) return;

            ScriptText.Clear();
            FileName = "unknown.lua";
            Modified = false;
        }

        private void tbLoadScript_Click(object sender, EventArgs e)
        {
            if (!CheckModified()) return;

            string fileName = AppHost.StdDialogs.GetOpenFile("", "", LangMan.LS(LSID.LSID_ScriptsFilter), 1, GKData.LUA_EXT);
            if (string.IsNullOrEmpty(fileName)) return;

            using (StreamReader strd = new StreamReader(File.OpenRead(fileName), Encoding.UTF8)) {
                txtScriptText.Text = strd.ReadToEnd();
                FileName = fileName;
                Modified = false;
                strd.Close();
            }
        }

        private void tbSaveScript_Click(object sender, EventArgs e)
        {
            string fileName = AppHost.StdDialogs.GetSaveFile("", "", LangMan.LS(LSID.LSID_ScriptsFilter), 1, GKData.LUA_EXT, FileName);
            if (string.IsNullOrEmpty(fileName)) return;

            using (StreamWriter strd = new StreamWriter(fileName, false, Encoding.UTF8)) {
                strd.Write(txtScriptText.Text);
                FileName = fileName;
                Modified = false;
                strd.Close();
            }
        }

        private void tbRun_Click(object sender, EventArgs e)
        {
            try {
                DebugOutput.Clear();
                using (ScriptEngine scrEngine = new ScriptEngine()) {
                    scrEngine.lua_run(txtScriptText.Text, fBase, ((IScriptEditWin)this).DebugOutput);
                }
            } catch (Exception ex) {
                Logger.LogWrite("ScriptEditWin.Run(): " + ex.Message);
                Logger.LogWrite("ScriptEditWin.Run(): " + ex.StackTrace);
            }
        }

        private void ScriptEditWin_Closing(object sender, CancelEventArgs e)
        {
            e.Cancel = !CheckModified();
        }

        private void mmScriptText_TextChanged(object sender, EventArgs e)
        {
            Modified = true;
        }

        public ScriptEditWin(IBaseWindow baseWin)
        {
            InitializeComponent();

            tbNewScript.Image = UIHelper.LoadResourceImage("Resources.btn_create_new.gif");
            tbLoadScript.Image = UIHelper.LoadResourceImage("Resources.btn_load.gif");
            tbSaveScript.Image = UIHelper.LoadResourceImage("Resources.btn_save.gif");
            tbRun.Image = UIHelper.LoadResourceImage("Resources.btn_start.gif");

            fBase = baseWin;

            tbNewScript_Click(this, null);

            SetLang();
        }

        public void SetLang()
        {
            tbNewScript.ToolTip = LangMan.LS(LSID.LSID_NewScriptTip);
            tbLoadScript.ToolTip = LangMan.LS(LSID.LSID_LoadScriptTip);
            tbSaveScript.ToolTip = LangMan.LS(LSID.LSID_SaveScriptTip);
            tbRun.ToolTip = LangMan.LS(LSID.LSID_RunScriptTip);
        }

        private void ScriptEditWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Escape) Close();
        }
    }
}
