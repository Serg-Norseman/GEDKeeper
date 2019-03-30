/* MessageBox.cs
 * 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * History:  
 * 10Dec08 AlexC          Migrated from GEDmill 1.10
 *
 */

using System.Drawing;
using System.Windows.Forms;

namespace GEDmill
{
    /// <summary>
    /// Custom Message Box, with selectable text
    /// </summary>
    public partial class MessageBoxEx : System.Windows.Forms.Form
    {
        private System.Drawing.Icon m_icon;

        // Pixels border around text
        private long m_lnBorder;

        // Pixels border around client border
        private long m_lnClientBorder;

        // Padding between buttons
        private long m_lnButtonSpacing;

        // Space to accomodate icon at left
        private long m_lnIconWidth;


        public MessageBoxEx(MessageBoxIcon icon)
        {
            m_lnBorder = 8; // pixels border around text
            m_lnButtonSpacing = 8; // padding between buttons
            m_lnIconWidth = 50; // space to accomodate icon at left
            m_lnClientBorder = 8;

            // Required for Windows Form Designer support
            InitializeComponent();

            // Create icon.
            switch (icon) {
                case MessageBoxIcon.Asterisk:
                    m_icon = System.Drawing.SystemIcons.Asterisk;
                    break;
                case MessageBoxIcon.Exclamation:
                    m_icon = System.Drawing.SystemIcons.Exclamation;
                    break;
                case MessageBoxIcon.None:
                    m_icon = null;
                    break;
                case MessageBoxIcon.Question:
                    m_icon = System.Drawing.SystemIcons.Question;
                    break;
                case MessageBoxIcon.Stop:
                    m_icon = System.Drawing.SystemIcons.Hand;
                    break;
                default:
                    m_icon = System.Drawing.SystemIcons.Exclamation;
                    break;
            }
        }

        // Clean up any resources being used.
        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (components != null) {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }

        // "Runs" the message box dialog
        public static DialogResult Show(IWin32Window parent, string sText, string sTitle, MessageBoxButtons buttons, MessageBoxIcon icon, bool bSelectable)
        {
            MessageBoxEx mbox = new MessageBoxEx(icon);
            mbox.DoLayout(sText, sTitle, buttons, bSelectable);

            mbox.Location = new Point(MainForm.m_mainForm.Location.X, MainForm.m_mainForm.Location.Y);
            return mbox.ShowDialog(parent);
        }

        // Event handler
        private void textBox_TextChanged(object sender, System.EventArgs e)
        {
        }

        // Event handler, draws the message box
        private void messageBox_onPaint(object sender, System.Windows.Forms.PaintEventArgs e)
        {
            if (m_icon != null) {
                e.Graphics.DrawIcon(m_icon, (int)m_lnClientBorder, (int)m_lnClientBorder);
            }
        }

        // Calculates the positions of everything in the message box
        private void DoLayout(string sText, string sTitle, MessageBoxButtons buttons, bool bSelectable)
        {
            this.SuspendLayout();

            long lnMaxWidth = 0;
            long lnMaxHeight = 0;
            long lnButtonHeight = m_buttonOk.Height + 2 * m_lnButtonSpacing;

            // Show/Hide and measure buttons according to what buttons user has selected
            switch (buttons) {
                case MessageBoxButtons.OKCancel:
                    m_buttonOk.Visible = true;
                    m_buttonCancel.Visible = true;
                    m_buttonYes.Visible = false;
                    m_buttonNo.Visible = false;
                    m_buttonRetry.Visible = false;
                    lnMaxWidth = m_buttonOk.Width + m_lnButtonSpacing + m_buttonCancel.Width;
                    break;

                case MessageBoxButtons.YesNo:
                    m_buttonOk.Visible = false;
                    m_buttonCancel.Visible = false;
                    m_buttonYes.Visible = true;
                    m_buttonNo.Visible = true;
                    m_buttonRetry.Visible = false;
                    lnMaxWidth = m_buttonYes.Width + m_lnButtonSpacing + m_buttonNo.Width;
                    break;

                case MessageBoxButtons.YesNoCancel:
                    m_buttonOk.Visible = false;
                    m_buttonCancel.Visible = true;
                    m_buttonYes.Visible = true;
                    m_buttonNo.Visible = true;
                    m_buttonRetry.Visible = false;
                    lnMaxWidth = m_buttonYes.Width + m_lnButtonSpacing + m_buttonNo.Width + m_lnButtonSpacing + m_buttonCancel.Width;
                    break;

                case MessageBoxButtons.RetryCancel:
                    m_buttonOk.Visible = false;
                    m_buttonCancel.Visible = true;
                    m_buttonYes.Visible = false;
                    m_buttonNo.Visible = false;
                    m_buttonRetry.Visible = true;
                    lnMaxWidth = m_buttonRetry.Width + m_lnButtonSpacing + m_buttonCancel.Width;
                    break;

                case MessageBoxButtons.OK:
                // The following are not supported yet, so fall through to OK:
                case MessageBoxButtons.AbortRetryIgnore:
                default:
                    m_buttonOk.Visible = true;
                    m_buttonCancel.Visible = false;
                    m_buttonYes.Visible = false;
                    m_buttonNo.Visible = false;
                    m_buttonRetry.Visible = false;
                    lnMaxWidth = m_buttonOk.Width;
                    break;
            }

            // Calculate length of longest gedcomLine in text, and combined height of all lines
            int i = 0;
            string s = "";

            // Create a Graphics object for the Control.
            Graphics g;
            Font f;
            if (bSelectable) {
                g = m_textbox.CreateGraphics();
                f = m_textbox.Font;
            } else {
                g = m_label.CreateGraphics();
                f = m_label.Font;
            }
            Size sz;

            do {
                char c = sText[i];
                if (c != '\r' && c != '\n') {
                    s += c;
                }
                i++;
                if (i == sText.Length || c == '\n') {
                    if (s == "") {
                        // Ensure height counts
                        s = " ";
                    }
                    // Calculate size of this gedcomLine
                    sz = g.MeasureString(s, f).ToSize();

                    if (sz.Width > lnMaxWidth) {
                        lnMaxWidth = sz.Width;
                    }
                    lnMaxHeight += sz.Height; // TODO: Plus gedcomLine spacing?
                    s = "";
                }
            }
            while (i < sText.Length);

            // Clean up the Graphics object.
            g.Dispose();

            // Show/hide and position label & textbox according to bSelectable
            if (bSelectable) {
                m_textbox.ClientSize = new Size(
                    (int)(lnMaxWidth + (m_lnBorder * 2)),
                    (int)(lnMaxHeight + (m_lnBorder * 2)));

                m_textbox.Location = new Point((int)(m_lnIconWidth + m_lnClientBorder), (int)m_lnClientBorder);
                // Set text
                m_textbox.Text = sText;
                m_label.Visible = false;
                m_textbox.Visible = true;

            } else {
                m_label.ClientSize = new Size(
                    (int)(lnMaxWidth + (m_lnBorder * 2)),
                    (int)(lnMaxHeight + (m_lnBorder * 2)));
                m_label.Location = new Point((int)(m_lnIconWidth + m_lnClientBorder), (int)m_lnClientBorder);

                // Set text
                m_label.Text = sText;
                m_textbox.Visible = false;
                m_label.Visible = true;
            }

            // Set client size
            long lnClientWidth = lnMaxWidth + m_lnIconWidth + m_lnBorder * 2;
            this.ClientSize = new Size((int)(lnClientWidth + m_lnClientBorder * 2), (int)(lnMaxHeight + m_lnBorder * 2 + lnButtonHeight + m_lnClientBorder * 2));

            // Position buttons according to what buttons user has selected
            switch (buttons) {
                case MessageBoxButtons.OKCancel:
                    m_buttonOk.Location = new Point((int)(m_lnClientBorder + (lnClientWidth - m_buttonOk.Width - m_lnButtonSpacing - m_buttonCancel.Width) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder * 2 + m_lnButtonSpacing));
                    m_buttonCancel.Location = new Point((int)(m_lnClientBorder + (lnClientWidth + m_buttonOk.Width + m_lnButtonSpacing - m_buttonCancel.Width) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder * 2 + m_lnButtonSpacing));
                    m_buttonOk.TabIndex = 1;
                    m_buttonCancel.TabIndex = 2;
                    break;

                case MessageBoxButtons.YesNo:
                    m_buttonYes.Location = new Point((int)(m_lnClientBorder + (lnClientWidth - m_buttonYes.Width - m_lnButtonSpacing - m_buttonNo.Width) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder * 2 + m_lnButtonSpacing));
                    m_buttonNo.Location = new Point((int)(m_lnClientBorder + (lnClientWidth + m_buttonYes.Width + m_lnButtonSpacing - m_buttonNo.Width) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder * 2 + m_lnButtonSpacing));
                    m_buttonYes.TabIndex = 1;
                    m_buttonNo.TabIndex = 2;
                    break;

                case MessageBoxButtons.YesNoCancel:
                    m_buttonYes.Location = new Point((int)(m_lnClientBorder + (lnClientWidth - m_buttonYes.Width - m_lnButtonSpacing - m_buttonNo.Width - m_lnButtonSpacing - m_buttonCancel.Width) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder * 2 + m_lnButtonSpacing));
                    m_buttonNo.Location = new Point((int)(m_lnClientBorder + (lnClientWidth + m_buttonYes.Width + m_lnButtonSpacing - m_buttonNo.Width - m_lnButtonSpacing - m_buttonCancel.Width) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder * 2 + m_lnButtonSpacing));
                    m_buttonCancel.Location = new Point((int)(m_lnClientBorder + (lnClientWidth + m_buttonYes.Width + m_lnButtonSpacing + m_buttonNo.Width + m_lnButtonSpacing - m_buttonCancel.Width) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder * 2 + m_lnButtonSpacing));
                    m_buttonYes.TabIndex = 1;
                    m_buttonNo.TabIndex = 2;
                    m_buttonCancel.TabIndex = 3;
                    break;

                case MessageBoxButtons.RetryCancel:
                    m_buttonRetry.Location = new Point((int)(m_lnClientBorder + (lnClientWidth - m_buttonRetry.Width - m_lnButtonSpacing - m_buttonCancel.Width) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder * 2 + m_lnButtonSpacing));
                    m_buttonCancel.Location = new Point((int)(m_lnClientBorder + (lnClientWidth + m_buttonRetry.Width + m_lnButtonSpacing - m_buttonCancel.Width) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder * 2 + m_lnButtonSpacing));
                    m_buttonRetry.TabIndex = 1;
                    m_buttonCancel.TabIndex = 2;
                    break;

                case MessageBoxButtons.OK:
                // The following are not supported yet, so fall through to OK
                case MessageBoxButtons.AbortRetryIgnore:
                default:
                    m_buttonOk.Location = new Point((int)(m_lnClientBorder + (lnClientWidth - m_buttonOk.Width) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder * 2 + m_lnButtonSpacing));
                    m_buttonOk.TabIndex = 1;
                    break;
            }

            // Set title
            this.Text = sTitle;

            this.ResumeLayout(false);
        }
    }
}
