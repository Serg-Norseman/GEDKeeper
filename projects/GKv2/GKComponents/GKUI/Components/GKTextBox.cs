/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih.
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
using System.Text;
using System.Windows.Forms;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKTextBox : TextBox
    {
        private static readonly char[] NUMERIC_VALID_CHARS =
        {
            '0', '1','2','3','4','5','6','7','8','9',
            '\b', // Backspace
            (char)22, // Ctrl + c
            (char)3, // Ctrl + v
            (char)24 // Ctrl + x
        };
        private static readonly char[] NAME_SEPARATORS = new char[] { ' ', '-' };


        private bool fNameMode;
        private bool fNumeric;
        private bool fTrimmed;


        public bool NameMode
        {
            get { return fNameMode; }
            set { fNameMode = value; }
        }

        public bool Numeric
        {
            get { return fNumeric; }
            set { fNumeric = value; }
        }

        public bool Trimmed
        {
            get { return fTrimmed; }
            set { fTrimmed = value; }
        }

        public override string Text
        {
            get {
                string val = base.Text;
                if (fNumeric) {
                    return (val == "") ? 0.ToString() : val;
                } else {
                    return val;
                }
            }
            set { base.Text = value; }
        }


        public GKTextBox(): base()
        {
        }

        protected override void OnLeave(EventArgs e)
        {
            var res = new StringBuilder();

            if (fTrimmed) {
                res.Append(Text.Trim());
                res.Replace("  ", " ");
            } else {
                res.Append(Text);
            }

            if (fNameMode) {
                bool nearCapChar = true;
                for (int n = 0; n < res.Length; n++)
                {
                    char ch = res[n];
                    res[n] = nearCapChar ? char.ToUpper(ch) : char.ToLower(ch);

                    nearCapChar = Array.IndexOf(NAME_SEPARATORS, ch) >= 0;
                }
            }

            Text = res.ToString();

            base.OnLeave(e);
        }

        protected override void OnKeyPress(KeyPressEventArgs e)
        {
            base.OnKeyPress(e);

            if (fNumeric) {
                if (Array.IndexOf(NUMERIC_VALID_CHARS, e.KeyChar) >= 0) return;
                e.Handled = true;
            }
        }

        protected override void OnEnter(EventArgs e)
        {
            base.OnEnter(e);

            SelectionStart = 0;
            SelectionLength = Text.Length;
        }
    }
}
