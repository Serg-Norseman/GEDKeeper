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

            btnAccept.Image = GKResources.iBtnAccept;
            btnCancel.Image = GKResources.iBtnCancel;

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
                        int.Parse(this.Value);
                        break;

                    case NumbersMode.nmFloat:
                        double.Parse(this.Value);
                        break;
                }

                DialogResult = DialogResult.OK;
            }
            catch
            {
                GKUtils.ShowError("Number format is invalid");
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

        public static bool QueryText(string caption, string prompt, ref string value)
        {
            bool result = false;

            using (var inputBox = new GKInputBox(caption, prompt, value, NumbersMode.nmNone))
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
    }
}
