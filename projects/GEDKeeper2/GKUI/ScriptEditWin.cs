/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;

using GKCommon;
using GKCore;
using GKCore.Interfaces;

namespace GKUI
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class ScriptEditWin : Form, ILocalization
    {
        private readonly IBaseWindow fBase;

        private string fFileName;
        private bool fModified;

        public string FileName
        {
            get {
                return this.fFileName;
            }
            set {
                this.fFileName = value;
                this.SetTitle();
            }
        }

        public bool Modified
        {
            get {
                return this.fModified;
            }
            set {
                this.fModified = value;
                this.SetTitle();
            }
        }

        private bool CheckModified()
        {
            bool result = true;

            if (this.Modified)
            {
                DialogResult dialogResult = MessageBox.Show(LangMan.LS(LSID.LSID_FileSaveQuery), GKData.APP_TITLE, MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation);

                switch (dialogResult) {
                    case DialogResult.Yes:
                        this.tbSaveScript_Click(this, null);
                        break;
                    case DialogResult.No:
                        break;
                    case DialogResult.Cancel:
                        result = false;
                        break;
                }
            }

            return result;
        }

        private void SetTitle()
        {
            this.Text = Path.GetFileName(this.fFileName);
            if (this.fModified)
            {
                this.Text = @"* " + this.Text;
            }
        }

        private void tbNewScript_Click(object sender, EventArgs e)
        {
            if (this.CheckModified())
            {
                this.txtScriptText.Clear();
                this.FileName = "unknown.lua";
                this.Modified = false;
            }
        }

        private void tbLoadScript_Click(object sender, EventArgs e)
        {
            if (this.CheckModified())
            {
                string fileName = UIHelper.GetOpenFile("", "", LangMan.LS(LSID.LSID_ScriptsFilter), 1, GKData.LUA_EXT);
                if (!string.IsNullOrEmpty(fileName))
                {
                    using (StreamReader strd = new StreamReader(File.OpenRead(fileName), Encoding.UTF8))
                    {
                        this.txtScriptText.Text = strd.ReadToEnd();
                        this.FileName = fileName;
                        this.Modified = false;
                        strd.Close();
                    }
                }
            }
        }

        private void tbSaveScript_Click(object sender, EventArgs e)
        {
            string fileName = UIHelper.GetSaveFile("", "", LangMan.LS(LSID.LSID_ScriptsFilter), 1, GKData.LUA_EXT, this.FileName);
            if (!string.IsNullOrEmpty(fileName))
            {
                using (StreamWriter strd = new StreamWriter(fileName, false, Encoding.UTF8))
                {
                    strd.Write(this.txtScriptText.Text);
                    this.FileName = fileName;
                    this.Modified = false;
                    strd.Close();
                }
            }
        }

        private void tbRun_Click(object sender, EventArgs e)
        {
            try
            {
                this.txtDebugOutput.Clear();
                using (ScriptEngine scrEngine = new ScriptEngine()) {
                    scrEngine.lua_run(this.txtScriptText.Text, this.fBase, this.txtDebugOutput);
                }
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("ScriptEditWin.Run(): " + ex.Message);
                this.fBase.Host.LogWrite("ScriptEditWin.Run(): " + ex.StackTrace.ToString());
            }
        }

        private void ScriptEditWin_Closing(object sender, CancelEventArgs e)
        {
            e.Cancel = !this.CheckModified();
        }

        private void mmScriptText_TextChanged(object sender, EventArgs e)
        {
            this.Modified = true;
        }

        public ScriptEditWin(IBaseWindow baseWin)
        {
            this.InitializeComponent();

            this.tbNewScript.Image = (Image)MainWin.ResourceManager.GetObjectEx("iCreateNew");
            this.tbLoadScript.Image = (Image)MainWin.ResourceManager.GetObjectEx("iLoad");
            this.tbSaveScript.Image = (Image)MainWin.ResourceManager.GetObjectEx("iSave");
            this.tbRun.Image = (Image)MainWin.ResourceManager.GetObjectEx("iStart");

            this.fBase = baseWin;

            this.txtScriptText.TextChanged += mmScriptText_TextChanged;
            
            this.tbNewScript_Click(this, null);

            this.SetLang();
        }

        public void SetLang()
        {
            this.tbNewScript.ToolTipText = LangMan.LS(LSID.LSID_NewScriptTip);
            this.tbLoadScript.ToolTipText = LangMan.LS(LSID.LSID_LoadScriptTip);
            this.tbSaveScript.ToolTipText = LangMan.LS(LSID.LSID_SaveScriptTip);
            this.tbRun.ToolTipText = LangMan.LS(LSID.LSID_RunScriptTip);
        }

        private void ScriptEditWin_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode)
            {
                case Keys.Escape:
                    base.Close();
                    break;
            }
        }
    }
}
