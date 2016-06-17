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
using System.Windows.Forms;
using GKCore;

namespace GKUI.Controls
{
    /// <summary>
    /// 
    /// </summary>
    public partial class GKInputBox : Form
    {
        private readonly bool fNumbersMode;

        public string Value
        {
            get { return this.textBox1.Text; }
            set { this.textBox1.Text = value; }
        }

        public GKInputBox(string caption, string prompt, string value, bool fNumbersMode, bool pwMode = false)
        {
            this.InitializeComponent();

            this.btnAccept.Image = global::GKResources.iBtnAccept;
            this.btnCancel.Image = global::GKResources.iBtnCancel;

            this.Text = caption;
            this.label1.Text = prompt;
            this.Value = value;
            this.fNumbersMode = fNumbersMode;

            if (pwMode) {
                this.textBox1.PasswordChar = '*';
            }

            base.AcceptButton = this.btnAccept;
            base.CancelButton = this.btnCancel;

            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
        }

        /*void OnKeyDown(object sender, KeyEventArgs e)
		{
			if (this.mMode == 1)
			{
				this.NoneNumberEntered = false;
				if ((e.KeyCode < Keys.D0 || e.KeyCode > Keys.D9) && (e.KeyCode < Keys.NumPad0 || e.KeyCode > Keys.NumPad9) && e.KeyCode != Keys.Back && e.KeyCode != Keys.Decimal && e.KeyCode != Keys.Oemcomma && e.KeyCode != Keys.OemMinus && e.KeyCode != Keys.Subtract)
				{
					this.NoneNumberEntered = true;
				}
			}
		}

		void OnKeyPress(object sender, KeyPressEventArgs e)
		{
			if (this.mMode == 1 && this.NoneNumberEntered)
			{
				e.Handled = true;
			}
		}*/

        private void btnCancel_Click(object sender, EventArgs e)
        {
            base.DialogResult = DialogResult.Cancel;
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            base.DialogResult = DialogResult.OK;
        }


        public static bool QueryDouble(string caption, string prompt, out double value)
        {
            bool result = false;
            value = 0.0;

            using (GKInputBox inputBox = new GKInputBox(caption, prompt, value.ToString(), true))
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

            using (GKInputBox inputBox = new GKInputBox(caption, prompt, value.ToString(), true))
            {
                if (inputBox.ShowDialog() == DialogResult.OK)
                {
                    result = int.TryParse(inputBox.Value, out value);
                }
            }

            return result;
        }

        public static bool QueryText(string caption, string prompt, ref string value)
        {
            bool result = false;

            using (GKInputBox inputBox = new GKInputBox(caption, prompt, value, false))
            {
                if (inputBox.ShowDialog() == DialogResult.OK)
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

            using (GKInputBox inputBox = new GKInputBox(caption, prompt, value, false, true))
            {
                if (inputBox.ShowDialog() == DialogResult.OK)
                {
                    value = inputBox.Value.Trim();
                    result = true;
                }
            }

            return result;
        }
    }
}
