/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using System.Drawing;
using System.Windows.Forms;
using GKCore;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GKInputBox : Form
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

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            Text = caption;
            label1.Text = prompt;
            Value = value;
            fNumbersMode = numbersMode;

            if (pwMode) {
                txtValue.PasswordChar = '*';
            }

            AcceptButton = btnAccept;
            CancelButton = btnCancel;

            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
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
            DialogResult = DialogResult.Cancel;
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

                DialogResult = DialogResult.OK;
            }
            catch
            {
                AppHost.StdDialogs.ShowError("Number format is invalid");
                DialogResult = DialogResult.None;
            }
        }


        public static bool QueryDouble(string caption, string prompt, out double value)
        {
            bool result = false;
            value = 0.0;

            using (var inputBox = new GKInputBox(caption, prompt, value.ToString(), NumbersMode.nmFloat))
            {
                if (inputBox.ShowDialog() == DialogResult.OK)
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
                if (inputBox.ShowDialog() == DialogResult.OK)
                {
                    result = int.TryParse(inputBox.Value, out value);
                }
            }

            return result;
        }

        public static bool QueryText(object owner, string caption, string prompt, ref string value)
        {
            bool result = false;

            using (var inputBox = new GKInputBox(caption, prompt, value, NumbersMode.nmNone)) {
                if (inputBox.ShowDialog() == DialogResult.OK) {
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
                if (inputBox.ShowDialog() == DialogResult.OK)
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
            SuspendLayout();

            txtValue = new TextBox();
            txtValue.Location = new Point(12, 25);
            txtValue.Name = "txtValue";
            txtValue.Size = new Size(354, 20);
            txtValue.TabIndex = 0;

            label1 = new Label();
            label1.AutoSize = true;
            label1.Location = new Point(12, 9);
            label1.Name = "label1";
            label1.Size = new Size(35, 13);
            label1.TabIndex = 3;
            label1.Text = "label1";

            btnAccept = new Button();
            btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
            btnAccept.Location = new Point(197, 61);
            btnAccept.Name = "btnAccept";
            btnAccept.Size = new Size(81, 25);
            btnAccept.TabIndex = 4;
            btnAccept.Text = "btnAccept";
            btnAccept.TextAlign = ContentAlignment.MiddleRight;
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.DialogResult = DialogResult.Cancel;
            btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
            btnCancel.Location = new Point(285, 61);
            btnCancel.Name = "btnCancel";
            btnCancel.Size = new Size(81, 25);
            btnCancel.TabIndex = 5;
            btnCancel.Text = "btnCancel";
            btnCancel.TextAlign = ContentAlignment.MiddleRight;
            btnCancel.Click += btnCancel_Click;

            AutoScaleDimensions = new SizeF(6F, 13F);
            AutoScaleMode = AutoScaleMode.Font;
            ClientSize = new Size(378, 98);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            Controls.Add(label1);
            Controls.Add(txtValue);
            FormBorderStyle = FormBorderStyle.FixedDialog;
            MaximizeBox = false;
            MinimizeBox = false;
            Name = "InputBox";
            ShowInTaskbar = false;
            StartPosition = FormStartPosition.CenterParent;
            Text = "InputBox";
            TopMost = true;
            ResumeLayout(false);
            PerformLayout();
        }

        #endregion
    }
}
