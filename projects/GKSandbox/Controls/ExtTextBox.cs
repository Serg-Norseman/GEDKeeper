using System;
using System.Text;
using System.Windows.Forms;

namespace GKCommon.Controls
{
    /// <summary>
    /// 
    /// </summary>
    public class ExtTextBox : TextBox
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


        public override string Text
        {
            get {
                string val = base.Text;
                if (Numeric) {
                    return (val == "") ? 0.ToString() : val;
                } else {
                    return val;
                }
            }
            set { base.Text = value; }
        }


        public bool Trimmed;
        public bool Numeric;
        public bool NameMode;

        public ExtTextBox(): base()
        {
        }

        protected override void OnLeave(EventArgs e)
        {
            var res = new StringBuilder();

            if (Trimmed) {
                res.Append(Text.Trim());
                res.Replace("  ", " ");
            } else {
                res.Append(Text);
            }

            if (NameMode) {
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

            if (Numeric) {
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
