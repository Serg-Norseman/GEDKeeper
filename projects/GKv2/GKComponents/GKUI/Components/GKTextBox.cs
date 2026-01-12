/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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


        public GKTextBox()
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
