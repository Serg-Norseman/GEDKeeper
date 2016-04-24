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
using System.IO;
using System.Text;
using System.Windows.Forms;

using GKCore;
using GKCore.Interfaces;

namespace GKUI
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TfmScriptDaemon : Form, ILocalization
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
                        this.SaveScript();
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

        private void NewScript()
        {
            if (this.CheckModified())
            {
                this.mmScriptText.Clear();
                this.FileName = "unknown.lua";
                this.Modified = false;
            }
        }

        private void LoadScript()
        {
            if (this.CheckModified() && this.OpenDialog1.ShowDialog() == DialogResult.OK)
            {
                using (StreamReader strd = new StreamReader(File.OpenRead(this.OpenDialog1.FileName), Encoding.UTF8))
                {
                    this.mmScriptText.Text = strd.ReadToEnd();
                    this.FileName = this.OpenDialog1.FileName;
                    this.Modified = false;
                    strd.Close();
                }
            }
        }

        private void SaveScript()
        {
            this.SaveDialog1.FileName = this.FileName;
            if (this.SaveDialog1.ShowDialog() == DialogResult.OK)
            {
                using (StreamWriter strd = new StreamWriter(this.SaveDialog1.FileName, false, Encoding.UTF8))
                {
                    strd.Write(this.mmScriptText.Text);
                    this.FileName = this.SaveDialog1.FileName;
                    this.Modified = false;
                    strd.Close();
                }
            }
        }

        private void Run()
        {
            try
            {
                this.mmDebugOutput.Clear();
                using (ScriptEngine scrEngine = new ScriptEngine()) {
                    scrEngine.lua_run(this.mmScriptText.Text, this.fBase, this.mmDebugOutput);
                }
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TfmScriptDaemon.Run(): " + ex.Message);
            }
        }

        void TfmScriptDaemon_Closing(object sender, CancelEventArgs e)
        {
            e.Cancel = !this.CheckModified();
        }

        void mmScriptText_TextChanged(object sender, EventArgs e)
        {
            this.Modified = true;
        }

        void ToolBar1_ButtonClick(object sender, ToolBarButtonClickEventArgs e)
        {
            if (e.Button == this.btnNewScript) {
                this.NewScript();
            } else if (e.Button == this.btnLoadScript) {
                this.LoadScript();
            } else if (e.Button == this.btnSaveScript) {
                this.SaveScript();
            } else if (e.Button == this.btnRun) {
                this.Run();
            }
        }

        public TfmScriptDaemon(IBaseWindow aBase)
        {
            this.InitializeComponent();
            this.fBase = aBase;

            this.ToolBar1.ImageList = TfmGEDKeeper.Instance.ImageList_Buttons;
            this.mmScriptText.TextChanged += mmScriptText_TextChanged;
            
            this.NewScript();

            this.SetLang();
        }

        public void SetLang()
        {
        }
    }
}
