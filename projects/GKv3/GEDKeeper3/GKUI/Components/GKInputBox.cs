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
using Eto.Forms;
using Eto.Drawing;
using GKCore;
using GKUI.Dialogs;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class GKInputBox : ModalDialog
    {
        private enum NumbersMode { nmNone, nmInt, nmFloat }

        private readonly NumbersMode fNumbersMode;

        public string Value
        {
            get { return txtValue.Text; }
            set { txtValue.Text = value; }
        }

        private GKInputBox(string caption, string prompt, string value, NumbersMode numbersMode, bool pwMode = false)
        {
            InitializeComponent();

            btnAccept.Image = Bitmap.FromResource("Resources.btn_accept.gif");
            btnCancel.Image = Bitmap.FromResource("Resources.btn_cancel.gif");

            Title = caption;
            label1.Text = prompt;
            Value = value;
            fNumbersMode = numbersMode;

            if (pwMode) {
                txtValue.PasswordChar = '*';
            }

            DefaultButton = btnAccept;
            AbortButton = btnCancel;

            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
        }

        protected override void Dispose(bool disposing)
        {
            base.Dispose(disposing);
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            DialogResult = DlgResult.Cancel;
            Close();
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                switch (fNumbersMode)
                {
                    case NumbersMode.nmNone:
                        break;

                    case NumbersMode.nmInt:
                        int.Parse(Value);
                        break;

                    case NumbersMode.nmFloat:
                        double.Parse(Value);
                        break;
                }

                DialogResult = DlgResult.OK;
            }
            catch
            {
                AppHost.StdDialogs.ShowError("Number format is invalid");
                DialogResult = DlgResult.None;
            }
        }


        public static bool QueryDouble(string caption, string prompt, out double value)
        {
            bool result = false;
            value = 0.0;

            using (var inputBox = new GKInputBox(caption, prompt, value.ToString(), NumbersMode.nmFloat))
            {
                if (inputBox.ShowModal() == DlgResult.OK)
                {
                    result = double.TryParse(inputBox.Value, out value);
                }
            }

            return result;
        }

        public static bool QueryInt(string caption, string prompt, out int value)
        {
            bool result = false;
            value = 0;

            using (var inputBox = new GKInputBox(caption, prompt, value.ToString(), NumbersMode.nmInt))
            {
                if (inputBox.ShowModal() == DlgResult.OK)
                {
                    result = int.TryParse(inputBox.Value, out value);
                }
            }

            return result;
        }

        public static bool QueryText(string caption, string prompt, ref string value)
        {
            bool result = false;

            using (var inputBox = new GKInputBox(caption, prompt, value, NumbersMode.nmNone))
            {
                if (inputBox.ShowModal() == DlgResult.OK)
                {
                    value = inputBox.Value.Trim();
                    result = true;
                }
            }

            return result;
        }

        public static bool QueryPassword(string caption, string prompt, ref string value)
        {
            bool result = false;

            using (var inputBox = new GKInputBox(caption, prompt, value, NumbersMode.nmNone, true))
            {
                if (inputBox.ShowModal() == DlgResult.OK)
                {
                    value = inputBox.Value.Trim();
                    result = true;
                }
            }

            return result;
        }

        #region Design

        private TextBox txtValue;
        private Label label1;
        private Button btnCancel;
        private Button btnAccept;

        private void InitializeComponent()
        {
            txtValue = new TextBox();
            label1 = new Label();
            btnAccept = new Button();
            btnCancel = new Button();
            SuspendLayout();

            txtValue.Location = new Point(12, 25);
            txtValue.Size = new Size(354, 20);

            label1.Location = new Point(12, 9);
            label1.Size = new Size(35, 13);
            label1.Text = "label1";

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(197, 61);
            btnAccept.Size = new Size(81, 25);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(285, 61);
            btnCancel.Size = new Size(81, 25);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            ClientSize = new Size(378, 98);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            Controls.Add(label1);
            Controls.Add(txtValue);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "InputBox";
            Topmost = true;

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }

        #endregion
    }
}
