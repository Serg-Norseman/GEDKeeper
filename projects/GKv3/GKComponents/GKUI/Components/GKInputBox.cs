/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKCore.Locales;
using GKUI.Themes;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GKInputBox : GKUI.Forms.CommonDialog
    {
        private enum NumbersMode { nmNone, nmInt, nmFloat }

        private readonly NumbersMode fNumbersMode;
        private readonly bool fPasswordMode;

        public string Value
        {
            get { return txtValue.Text; }
            set { txtValue.Text = value; }
        }

        private GKInputBox(string caption, string prompt, string value, NumbersMode numbersMode, bool pwMode = false)
        {
            fPasswordMode = pwMode;

            InitializeComponent();

            SetTitle(caption);
            label1.Text = prompt;
            Value = value;
            fNumbersMode = numbersMode;

            DefaultButton = btnAccept;
            AbortButton = btnCancel;

            btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.DlgCancel);

            UIHelper.SetButtonThemeImage(btnAccept, ThemeElement.Glyph_Accept);
            UIHelper.SetButtonThemeImage(btnCancel, ThemeElement.Glyph_Cancel);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                // dummy
            }
            base.Dispose(disposing);
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            Close(DialogResult.Cancel);
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try {
                switch (fNumbersMode) {
                    case NumbersMode.nmNone:
                        break;

                    case NumbersMode.nmInt:
                        int.Parse(Value);
                        break;

                    case NumbersMode.nmFloat:
                        double.Parse(Value);
                        break;
                }

                Close(DialogResult.Ok);
            } catch {
                AppHost.StdDialogs.ShowError("Number format is invalid");
                DialogResult = DialogResult.None;
            }
        }


        public static bool QueryDouble(string caption, string prompt, out double value)
        {
            bool result = false;
            value = 0.0;

            using (var inputBox = new GKInputBox(caption, prompt, value.ToString(), NumbersMode.nmFloat)) {
                if (inputBox.ShowModal() == DialogResult.Ok) {
                    result = double.TryParse(inputBox.Value, out value);
                }
            }

            return result;
        }

        public static bool QueryInt(string caption, string prompt, out int value)
        {
            bool result = false;
            value = 0;

            using (var inputBox = new GKInputBox(caption, prompt, value.ToString(), NumbersMode.nmInt)) {
                if (inputBox.ShowModal() == DialogResult.Ok) {
                    result = int.TryParse(inputBox.Value, out value);
                }
            }

            return result;
        }

        public static bool QueryText(object owner, string caption, string prompt, ref string value)
        {
            bool result = false;

            using (var inputBox = new GKInputBox(caption, prompt, value, NumbersMode.nmNone)) {
                if (inputBox.ShowModal((Control)owner) == DialogResult.Ok) {
                    value = inputBox.Value.Trim();
                    result = true;
                }
            }

            return result;
        }

        public static bool QueryPassword(string caption, string prompt, ref string value)
        {
            bool result = false;

            using (var inputBox = new GKInputBox(caption, prompt, value, NumbersMode.nmNone, true)) {
                if (inputBox.ShowModal() == DialogResult.Ok) {
                    value = inputBox.Value.Trim();
                    result = true;
                }
            }

            return result;
        }

        #region Design

        private TextControl txtValue;
        private Label label1;
        private Button btnCancel;
        private Button btnAccept;

        private void InitializeComponent()
        {
            if (fPasswordMode) {
                txtValue = new PasswordBox();
                ((PasswordBox)txtValue).PasswordChar = '*';
            } else {
                txtValue = new TextBox();
            }
            txtValue.Width = 360;

            label1 = new Label();

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(120, 26);
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(120, 26);
            btnCancel.Click += btnCancel_Click;

            Content = new TableLayout {
                Padding = 8,
                Spacing = new Size(8, 8),
                Rows = {
                    new StackLayout() { Orientation = Orientation.Vertical, Spacing = 2, Items = { label1, txtValue } },
                    new StackLayout() { Orientation = Orientation.Horizontal, Spacing = 8, Items = { null, btnAccept, btnCancel } }
                }
            };

            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Topmost = true;
        }

        #endregion
    }
}
