/* ProgressWindow.cs
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
    // Window used to display and update a progress bar while background task is happening
    public class ProgressWindow : System.Windows.Forms.Form, IProgressCallback
    {
        // The cancel button
        private System.Windows.Forms.Button m_buttonCancel;

        // The label describing what activity is currently taking place
        private System.Windows.Forms.Label m_label;

        // The progress bar
        private System.Windows.Forms.ProgressBar m_progressbar;

        // Required designer variable.
        private System.ComponentModel.Container components = null;

        // Callbacks
        public delegate void SetTextInvoker( String sText );
        public delegate void IncrementInvoker( int nVal );
        public delegate void StepToInvoker( int nVal );
        public delegate void RangeInvoker( int nMinimum, int nMaximum );
        public delegate void EndInvoker( CThreadError threadError );

        public CThreadError m_threaderror;
        private String m_sTitleRoot = "";
        private System.Threading.ManualResetEvent m_mreInit = new System.Threading.ManualResetEvent(false);
        private System.Threading.ManualResetEvent m_mreAbort = new System.Threading.ManualResetEvent(false);
        private bool m_bRequiresClose = true;

        // Constructor
        public ProgressWindow()
        {
            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();
            ShowInTaskbar = false;
            m_threaderror = new CThreadError( 1, "No error" );
        }

        // Call this method from the worker thread to initialize
        // the progress meter.
        public void Begin( int nMinimum, int nMaximum )
        {
            m_mreInit.WaitOne();
            Invoke( new RangeInvoker( DoBegin ), new object[] { nMinimum, nMaximum } );
        }

        // Call this method from the worker thread to initialize
        // the progress callback, without setting the range
        public void Begin()
        {
            m_mreInit.WaitOne();
            Invoke( new MethodInvoker( DoBegin ) );
        }

        // Call this method from the worker thread to reset the range in the progress callback
        // You must have called one of the Begin() methods prior to this call.
        public void SetRange( int nMinimum, int nMaximum )
        {
            m_mreInit.WaitOne();
            Invoke( new RangeInvoker( DoSetRange ), new object[] { nMinimum, nMaximum } );
        }

        // Call this method from the worker thread to update the progress text.
        public void SetText( String sText )
        {
            Invoke( new SetTextInvoker(DoSetText), new object[] { sText } );
        }

        // Call this method from the worker thread to increase the progress counter by a specified value.
        public void Increment( int nVal )
        {
            Invoke( new IncrementInvoker( DoIncrement ), new object[] { nVal } );
        }

        // Call this method from the worker thread to step the progress meter to a particular value.
        public void StepTo( int nVal )
        {
            Invoke( new StepToInvoker( DoStepTo ), new object[] { nVal } );
        }
        
        // If this property is true, then you should abort work
        public bool IsAborting
        {
            get
            {
                return m_mreAbort.WaitOne( 0, false );
            }
        }

        // Call this method from the worker thread to finalize the progress meter
        public void End( CThreadError threadError )
        {
            if( m_bRequiresClose )
            {
                Invoke( new EndInvoker( DoEnd ), new object[] { threadError } );
            }
        }

        // Partner of SetText(). Sets label text.
        private void DoSetText( String sText )
        {
            m_label.Text = sText;
        }

        // Partner of Increment(). Moves the progress bar and updates the status text.
        private void DoIncrement( int nVal )
        {
            m_progressbar.Increment( nVal );
            UpdateStatusText();
        }

        // Partner of StepTo(). Moves the progress bar.
        private void DoStepTo( int nVal )
        {
            m_progressbar.Value = nVal;
            UpdateStatusText();
        }

        // Partner of Begin(). Sets the up the progress bar.
        private void DoBegin( int nMinimum, int nMaximum )
        {
            DoBegin();
            DoSetRange( nMinimum, nMaximum );
        }

        // Starts the progress bar.
        private void DoBegin()
        {
            m_buttonCancel.Enabled = true;
            ControlBox = true;
        }

        // Partner of SetRange(). Sets the limits of the progress bar.
        private void DoSetRange( int nMinimum, int nMaximum )
        {
            m_progressbar.Minimum = nMinimum;
            m_progressbar.Maximum = nMaximum;
            m_progressbar.Value = nMinimum;
            m_sTitleRoot = Text;
        }

        // The partner of End(). Handles finalising the progres meter.
        private void DoEnd( CThreadError threadError )
        {
            m_threaderror.m_nError = threadError.m_nError;
            m_threaderror.m_sMessage = threadError.m_sMessage;

            if (m_threaderror.m_nError == 1)
            {
                DialogResult = DialogResult.Cancel;
            }
            else if (m_threaderror.m_nError == 2)
            {
                DialogResult = DialogResult.Abort;
            }
            else if (m_threaderror.m_nError == 3)
            {
                DialogResult = DialogResult.Retry;
            }
            else
            {
                DialogResult = DialogResult.OK;
            }
        }

        // Handles the form load, and sets an event to ensure that
        // intialization is synchronized with the appearance of the form.
        protected override void OnLoad(System.EventArgs e)
        {
            base.OnLoad( e );
            ControlBox = false;
            m_mreInit.Set();
        }

        // Clean up any resources being used.
        protected override void Dispose( bool disposing )
        {
            if( disposing )
            {
                if(components != null)
                {
                    components.Dispose();
                }
            }
            base.Dispose( disposing );
        }

        // Handler for 'Close' clicking
        protected override void OnClosing(System.ComponentModel.CancelEventArgs e)
        {
            m_bRequiresClose = false;
            AbortWork();
            base.OnClosing( e );
        }
        
        // Utility function that formats and updates the title bar text
        private void UpdateStatusText()
        {
            Text = m_sTitleRoot;
        }
        
        // Utility function to terminate the thread
        private void AbortWork()
        {
            m_mreAbort.Set();
        }

        // Build the form
        private void InitializeComponent()
        {
            this.m_progressbar = new System.Windows.Forms.ProgressBar();
            this.m_label = new System.Windows.Forms.Label();
            this.m_buttonCancel = new System.Windows.Forms.Button();
            this.SuspendLayout();

            // 
            // progressBar
            // 
            this.m_progressbar.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left) 
                | System.Windows.Forms.AnchorStyles.Right)));
            this.m_progressbar.Location = new System.Drawing.Point(10, 55);
            this.m_progressbar.Name = "progressBar";
            this.m_progressbar.Size = new System.Drawing.Size(230, 27);
            this.m_progressbar.TabIndex = 1;

            // 
            // label
            // 
            this.m_label.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
                | System.Windows.Forms.AnchorStyles.Left) 
                | System.Windows.Forms.AnchorStyles.Right)));
            this.m_label.Location = new System.Drawing.Point(10, 9);
            this.m_label.Name = "label";
            this.m_label.Size = new System.Drawing.Size(326, 37);
            this.m_label.TabIndex = 0;

            // 
            // cancelButton
            // 
            this.m_buttonCancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.m_buttonCancel.Enabled = false;
            this.m_buttonCancel.Location = new System.Drawing.Point(250, 55);
            this.m_buttonCancel.Name = "cancelButton";
            this.m_buttonCancel.Size = new System.Drawing.Size(90, 27);
            this.m_buttonCancel.TabIndex = 2;
            this.m_buttonCancel.Text = "Cancel";
            this.m_buttonCancel.Click += new System.EventHandler(this.OnCancelClicked);

            // 
            // ProgressWindow
            // 
            this.AutoScaleMode = AutoScaleMode.None;
            this.AutoScaleBaseSize = new System.Drawing.Size(6, 15);
            this.ClientSize = new System.Drawing.Size(348, 91);
            this.Controls.Add(this.m_buttonCancel);
            this.Controls.Add(this.m_progressbar);
            this.Controls.Add(this.m_label);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.Name = "ProgressWindow";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "ProgressWindow";
            this.ResumeLayout(false);
        }

        // Handler for the cancel button
        private void OnCancelClicked(object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();
            AbortWork();
        }
    }
}
