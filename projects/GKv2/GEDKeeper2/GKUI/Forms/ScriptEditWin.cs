/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.ComponentModel;
using System.IO;
using System.Windows.Forms;
using GKCore;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;

namespace GKUI.Forms
{
    public sealed partial class ScriptEditWin : CommonDialog, IScriptEditWin
    {
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

        void IScriptConsole.print(string text)
        {
            MethodInvoker invoker = delegate () {
                txtDebugOutput.AppendText(text + "\r\n");
                txtDebugOutput.ScrollToCaret();
            };

            try {
                if (InvokeRequired) {
                    BeginInvoke(invoker, null);
                } else {
                    invoker();
                }
            } catch {
                // dummy
            }
        }

        #endregion

        public bool CheckModified()
        {
            bool result = true;
            if (!Modified) return result;

            DialogResult dialogResult = MessageBox.Show(LangMan.LS(LSID.FileSaveQuery), GKData.APP_TITLE, MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation);
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
            string title = Path.GetFileName(fFileName);
            if (fModified) {
                title = @"* " + title;
            }
            SetTitle(title);
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
            InitializeComponent();

            fController = new ScriptEditWinController(this);
            fController.Init(baseWin);

            txtScriptText.TextChanged += mmScriptText_TextChanged;

            tbNewScript_Click(this, null);
        }

        public void SetLocale()
        {
            fController.SetLocale();
        }

        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
        }

        private void ScriptEditWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) Close();
        }
    }
}
