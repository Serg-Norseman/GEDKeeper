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

using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace GEDmill
{
    // Custom Message Box, with selectable text
    public class MessageBocx : System.Windows.Forms.Form
    {
        // The windows controls
        private System.Windows.Forms.TextBox m_textbox;
        private System.Windows.Forms.Button m_buttonOk;
        private System.Windows.Forms.Button m_buttonCancel;
        private System.Windows.Forms.Button m_buttonYes;
        private System.Windows.Forms.Button m_buttonNo;
        private System.Windows.Forms.Button m_buttonRetry;
        private System.Drawing.Icon m_icon;

        // Pixels border around text
        private long m_lnBorder;

        // Pixels border around client border
        private long m_lnClientBorder;

        // Padding between buttons
        private long m_lnButtonSpacing;

        // Space to accomodate icon at left
        private long m_lnIconWidth;

        // Space to accomodate icon at left
        private System.Windows.Forms.Label m_label; 

        // Required designer variable.
        private System.ComponentModel.Container components = null;

        // Constructor
        public MessageBocx( MessageBoxIcon icon )
        {
            m_lnBorder = 8; // pixels border around text
            m_lnButtonSpacing = 8; // padding between buttons
            m_lnIconWidth = 50; // space to accomodate icon at left
            m_lnClientBorder = 8;

            // Required for Windows Form Designer support
            InitializeComponent();

            // Create icon.
            switch( icon )
            {
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
        protected override void Dispose( bool bDisposing )
        {
            if( bDisposing )
            {
                if(components != null)
                {
                    components.Dispose();
                }
            }
            base.Dispose( bDisposing );
        }

        // Required method for Designer support
        private void InitializeComponent()
        {
            this.m_textbox = new System.Windows.Forms.TextBox();
            this.m_buttonOk = new System.Windows.Forms.Button();
            this.m_buttonCancel = new System.Windows.Forms.Button();
            this.m_buttonYes = new System.Windows.Forms.Button();
            this.m_buttonNo = new System.Windows.Forms.Button();
            this.m_buttonRetry = new System.Windows.Forms.Button();
            this.m_label = new System.Windows.Forms.Label();
            this.SuspendLayout();

            // 
            // textBox
            // 
            this.m_textbox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
                | System.Windows.Forms.AnchorStyles.Left) 
                | System.Windows.Forms.AnchorStyles.Right)));
            this.m_textbox.BackColor = System.Drawing.SystemColors.Control;
            this.m_textbox.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.m_textbox.CausesValidation = false;
            this.m_textbox.Location = new System.Drawing.Point(154, 0);
            this.m_textbox.Multiline = true;
            this.m_textbox.Name = "textBox";
            this.m_textbox.ReadOnly = true;
            this.m_textbox.Size = new System.Drawing.Size(345, 92);
            this.m_textbox.TabIndex = 5;
            this.m_textbox.Text = "textBox1";
            this.m_textbox.TextChanged += new System.EventHandler(this.textBox_TextChanged);

            // 
            // OKbutton
            // 
            this.m_buttonOk.Anchor = System.Windows.Forms.AnchorStyles.Bottom;
            this.m_buttonOk.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.m_buttonOk.Location = new System.Drawing.Point(150, 240);
            this.m_buttonOk.Name = "OKbutton";
            this.m_buttonOk.Size = new System.Drawing.Size(90, 27);
            this.m_buttonOk.TabIndex = 3;
            this.m_buttonOk.Text = "OK";

            // 
            // cancelButton
            // 
            this.m_buttonCancel.Anchor = System.Windows.Forms.AnchorStyles.Bottom;
            this.m_buttonCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.m_buttonCancel.Location = new System.Drawing.Point(256, 240);
            this.m_buttonCancel.Name = "cancelButton";
            this.m_buttonCancel.Size = new System.Drawing.Size(90, 27);
            this.m_buttonCancel.TabIndex = 4;
            this.m_buttonCancel.Text = "Cancel";

            // 
            // yesButton
            // 
            this.m_buttonYes.Anchor = System.Windows.Forms.AnchorStyles.Bottom;
            this.m_buttonYes.DialogResult = System.Windows.Forms.DialogResult.Yes;
            this.m_buttonYes.Location = new System.Drawing.Point(38, 240);
            this.m_buttonYes.Name = "yesButton";
            this.m_buttonYes.Size = new System.Drawing.Size(90, 27);
            this.m_buttonYes.TabIndex = 1;
            this.m_buttonYes.Text = "Yes";

            // 
            // noButton
            // 
            this.m_buttonNo.Anchor = System.Windows.Forms.AnchorStyles.Bottom;
            this.m_buttonNo.DialogResult = System.Windows.Forms.DialogResult.No;
            this.m_buttonNo.Location = new System.Drawing.Point(365, 240);
            this.m_buttonNo.Name = "noButton";
            this.m_buttonNo.Size = new System.Drawing.Size(90, 27);
            this.m_buttonNo.TabIndex = 2;
            this.m_buttonNo.Text = "No";

            // 
            // retryButton
            // 
            this.m_buttonRetry.Anchor = System.Windows.Forms.AnchorStyles.Bottom;
            this.m_buttonRetry.DialogResult = System.Windows.Forms.DialogResult.Retry;
            this.m_buttonRetry.Location = new System.Drawing.Point(474, 240);
            this.m_buttonRetry.Name = "retryButton";
            this.m_buttonRetry.Size = new System.Drawing.Size(90, 27);
            this.m_buttonRetry.TabIndex = 1;
            this.m_buttonRetry.Text = "Retry";

            // 
            // label
            // 
            this.m_label.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
                | System.Windows.Forms.AnchorStyles.Left) 
                | System.Windows.Forms.AnchorStyles.Right)));
            this.m_label.BackColor = System.Drawing.SystemColors.Control;
            this.m_label.Location = new System.Drawing.Point(0, 111);
            this.m_label.Name = "label";
            this.m_label.Size = new System.Drawing.Size(499, 111);
            this.m_label.TabIndex = 6;
            this.m_label.Text = "label";
            this.m_label.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;

            // 
            // MessageBocx
            // 
            this.AutoScaleMode = AutoScaleMode.None;
            this.AutoScaleBaseSize = new System.Drawing.Size(6, 15);
            this.ClientSize = new System.Drawing.Size(501, 275);
            this.Controls.Add(this.m_buttonNo);
            this.Controls.Add(this.m_buttonYes);
            this.Controls.Add(this.m_label);
            this.Controls.Add(this.m_buttonCancel);
            this.Controls.Add(this.m_buttonOk);
            this.Controls.Add(this.m_buttonRetry);
            this.Controls.Add(this.m_textbox);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "MessageBocx";
            this.ShowInTaskbar = false;
            this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Hide;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "MessageBox";
            this.Paint += new System.Windows.Forms.PaintEventHandler(this.messageBox_onPaint);
            this.ResumeLayout(false);

        }

        // "Runs" the message box dialog
        public static DialogResult Show( IWin32Window parent, string sText, string sTitle, MessageBoxButtons buttons, MessageBoxIcon icon, bool bSelectable )
        {
            GEDmill.MessageBocx mbox = new GEDmill.MessageBocx( icon );
            mbox.DoLayout(  sText, sTitle, buttons, bSelectable  );

            mbox.Location = new Point( MainForm.m_mainForm.Location.X, MainForm.m_mainForm.Location.Y );
            return mbox.ShowDialog( parent );
        }

        // Event handler
        private void textBox_TextChanged(object sender, System.EventArgs e)
        {   
        }

        // Event handler, draws the message box
        private void messageBox_onPaint(object sender, System.Windows.Forms.PaintEventArgs e)
        {
            if( m_icon != null )    
            {
                e.Graphics.DrawIcon(m_icon, (int)m_lnClientBorder, (int)m_lnClientBorder);
            }
        }

        // Calculates the positions of everything in the message box
        private void DoLayout(string sText, string sTitle, MessageBoxButtons buttons,  bool bSelectable)
        {           
            this.SuspendLayout();

            long lnMaxWidth = 0;
            long lnMaxHeight = 0;
            long lnButtonHeight = m_buttonOk.Height + 2*m_lnButtonSpacing;

            // Show/Hide and measure buttons according to what buttons user has selected
            switch( buttons )
            {
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
                    lnMaxWidth = m_buttonYes.Width + m_lnButtonSpacing + m_buttonNo.Width+ m_lnButtonSpacing + m_buttonCancel.Width;
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
            if( bSelectable )
            {
                g = m_textbox.CreateGraphics();
                f = m_textbox.Font;
            }
            else
            {
                g = m_label.CreateGraphics();
                f = m_label.Font;
            }
            Size sz;

            do
            {
                char c = sText[i];
                if( c != '\r' && c != '\n' )
                {
                    s += c;
                }
                i++;
                if( i == sText.Length || c == '\n' )
                {
                    if( s == "" )
                    {
                        // Ensure height counts
                        s = " ";
                    }
                    // Calculate size of this gedcomLine
                    sz = g.MeasureString( s, f ).ToSize();

                    if( sz.Width > lnMaxWidth )
                    {
                        lnMaxWidth = sz.Width;
                    }
                    lnMaxHeight += sz.Height; // TODO: Plus gedcomLine spacing?
                    s="";
                }
            }
            while( i < sText.Length );

            // Clean up the Graphics object.
            g.Dispose();

            // Show/hide and position label & textbox according to bSelectable
            if( bSelectable )
            {
                m_textbox.ClientSize = new Size(
                    (int)(lnMaxWidth + (m_lnBorder * 2)), 
                    (int)(lnMaxHeight+(m_lnBorder * 2)) );

                m_textbox.Location = new Point( (int)(m_lnIconWidth + m_lnClientBorder), (int)m_lnClientBorder );
                // Set text
                m_textbox.Text = sText;
                m_label.Visible = false;
                m_textbox.Visible = true;

            }
            else
            {
                m_label.ClientSize = new Size(
                    (int)(lnMaxWidth + (m_lnBorder * 2)), 
                    (int)(lnMaxHeight+(m_lnBorder * 2)) );
                m_label.Location = new Point( (int)(m_lnIconWidth + m_lnClientBorder), (int)m_lnClientBorder );
                
                // Set text
                m_label.Text = sText;
                m_textbox.Visible = false;
                m_label.Visible = true;
            }




            // Set client size
            long lnClientWidth = lnMaxWidth + m_lnIconWidth + m_lnBorder*2;
            this.ClientSize = new Size( (int)(lnClientWidth + m_lnClientBorder*2), (int)(lnMaxHeight + m_lnBorder*2 + lnButtonHeight + m_lnClientBorder*2) );

            // Position buttons according to what buttons user has selected
            switch( buttons )
            {
                case MessageBoxButtons.OKCancel:
                    m_buttonOk.Location = new Point( (int)(m_lnClientBorder + ( lnClientWidth - m_buttonOk.Width - m_lnButtonSpacing - m_buttonCancel.Width ) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder*2 + m_lnButtonSpacing) );
                    m_buttonCancel.Location = new Point( (int)(m_lnClientBorder + ( lnClientWidth + m_buttonOk.Width + m_lnButtonSpacing - m_buttonCancel.Width ) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder*2 + m_lnButtonSpacing) );
                    m_buttonOk.TabIndex = 1;
                    m_buttonCancel.TabIndex = 2;
                    break;

                case MessageBoxButtons.YesNo:
                    m_buttonYes.Location = new Point( (int)(m_lnClientBorder + ( lnClientWidth - m_buttonYes.Width - m_lnButtonSpacing - m_buttonNo.Width ) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder*2 + m_lnButtonSpacing ));
                    m_buttonNo.Location = new Point( (int)(m_lnClientBorder + ( lnClientWidth + m_buttonYes.Width + m_lnButtonSpacing - m_buttonNo.Width ) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder*2 + m_lnButtonSpacing ));
                    m_buttonYes.TabIndex = 1;
                    m_buttonNo.TabIndex = 2;
                    break;

                case MessageBoxButtons.YesNoCancel:
                    m_buttonYes.Location = new Point((int)(m_lnClientBorder + ( lnClientWidth - m_buttonYes.Width - m_lnButtonSpacing - m_buttonNo.Width - m_lnButtonSpacing - m_buttonCancel.Width ) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder*2 + m_lnButtonSpacing));
                    m_buttonNo.Location = new Point((int)(m_lnClientBorder + ( lnClientWidth + m_buttonYes.Width + m_lnButtonSpacing - m_buttonNo.Width - m_lnButtonSpacing - m_buttonCancel.Width ) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder*2 + m_lnButtonSpacing));
                    m_buttonCancel.Location = new Point( (int)(m_lnClientBorder + ( lnClientWidth + m_buttonYes.Width + m_lnButtonSpacing + m_buttonNo.Width + m_lnButtonSpacing - m_buttonCancel.Width ) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder*2 + m_lnButtonSpacing));
                    m_buttonYes.TabIndex = 1;
                    m_buttonNo.TabIndex = 2;
                    m_buttonCancel.TabIndex = 3;
                    break;

                case MessageBoxButtons.RetryCancel:
                    m_buttonRetry.Location = new Point( (int)(m_lnClientBorder + ( lnClientWidth - m_buttonRetry.Width - m_lnButtonSpacing - m_buttonCancel.Width ) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder*2 + m_lnButtonSpacing) );
                    m_buttonCancel.Location = new Point( (int)(m_lnClientBorder + ( lnClientWidth + m_buttonRetry.Width + m_lnButtonSpacing - m_buttonCancel.Width ) / 2), (int)(m_lnClientBorder + lnMaxHeight + m_lnBorder*2 + m_lnButtonSpacing) );
                    m_buttonRetry.TabIndex = 1;
                    m_buttonCancel.TabIndex = 2;
                    break;

                case MessageBoxButtons.OK:
                    // The following are not supported yet, so fall through to OK
                case MessageBoxButtons.AbortRetryIgnore:
                default:                    
                    m_buttonOk.Location = new Point( (int)(m_lnClientBorder + ( lnClientWidth - m_buttonOk.Width ) / 2),(int)( m_lnClientBorder + lnMaxHeight + m_lnBorder*2 + m_lnButtonSpacing));
                    m_buttonOk.TabIndex = 1;
                    break;
            }

            // Set title
            this.Text = sTitle;

            
            this.ResumeLayout(false);

        }
    }
}
