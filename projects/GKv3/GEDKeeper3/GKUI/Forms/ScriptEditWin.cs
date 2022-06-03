﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib.Design.MVP.Controls;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;

namespace GKUI.Forms
{
    public sealed partial class ScriptEditWin : CommonDialog, IScriptEditWin
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private ToolBar ToolBar1;
        private ButtonToolItem tbLoadScript;
        private ButtonToolItem tbRun;
        private TextArea txtDebugOutput;
        private TextArea txtScriptText;
        private ButtonToolItem tbSaveScript;
        private ButtonToolItem tbNewScript;
        private Splitter splitContainer1;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly ScriptEditWinController fController;

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

        ITextBox IScriptEditWin.ScriptText
        {
            get { return GetControlHandler<ITextBox>(txtScriptText); }
        }

        ITextBox IScriptEditWin.DebugOutput
        {
            get { return GetControlHandler<ITextBox>(txtDebugOutput); }
        }

        #endregion

        public bool CheckModified()
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
            fController.NewScript();
        }

        private void tbLoadScript_Click(object sender, EventArgs e)
        {
            fController.LoadScript();
        }

        private void tbSaveScript_Click(object sender, EventArgs e)
        {
            fController.SaveScript();
        }

        private void tbRun_Click(object sender, EventArgs e)
        {
            fController.RunScript();
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
            XamlReader.Load(this);

            fController = new ScriptEditWinController(this);
            fController.Init(baseWin);

            txtScriptText.TextChanged += mmScriptText_TextChanged;

            tbNewScript_Click(this, null);
        }

        public void SetLocale()
        {
            fController.SetLocale();
        }

        private void ScriptEditWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Escape) Close();
        }
    }
}
