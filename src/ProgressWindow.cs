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
using System.Windows.Forms;

namespace GEDmill
{
    /// <summary>
    /// Window used to display and update a progress bar while background task is happening.
    /// </summary>
    public partial class ProgressWindow : Form, IProgressCallback
    {
        // Callbacks
        public delegate void SetTextInvoker(String sText);
        public delegate void IncrementInvoker(int nVal);
        public delegate void StepToInvoker(int nVal);
        public delegate void RangeInvoker(int nMinimum, int nMaximum);
        public delegate void EndInvoker(ThreadError threadError);

        public ThreadError ThreadError;

        private String fTitleRoot = "";
        private System.Threading.ManualResetEvent fInitEvent = new System.Threading.ManualResetEvent(false);
        private System.Threading.ManualResetEvent fAbortEvent = new System.Threading.ManualResetEvent(false);
        private bool fRequiresClose = true;


        public ProgressWindow()
        {
            InitializeComponent();
            ShowInTaskbar = false;
            ThreadError = new ThreadError(1, "No error");
        }

        // Call this method from the worker thread to initialize the progress meter.
        public void Begin(int nMinimum, int nMaximum)
        {
            fInitEvent.WaitOne();
            Invoke(new RangeInvoker(DoBegin), new object[] { nMinimum, nMaximum });
        }

        // Call this method from the worker thread to initialize
        // the progress callback, without setting the range
        public void Begin()
        {
            fInitEvent.WaitOne();
            Invoke(new MethodInvoker(DoBegin));
        }

        // Call this method from the worker thread to reset the range in the progress callback
        // You must have called one of the Begin() methods prior to this call.
        public void SetRange(int nMinimum, int nMaximum)
        {
            fInitEvent.WaitOne();
            Invoke(new RangeInvoker(DoSetRange), new object[] { nMinimum, nMaximum });
        }

        // Call this method from the worker thread to update the progress text.
        public void SetText(String sText)
        {
            Invoke(new SetTextInvoker(DoSetText), new object[] { sText });
        }

        // Call this method from the worker thread to increase the progress counter by a specified value.
        public void Increment(int nVal)
        {
            Invoke(new IncrementInvoker(DoIncrement), new object[] { nVal });
        }

        // Call this method from the worker thread to step the progress meter to a particular value.
        public void StepTo(int nVal)
        {
            Invoke(new StepToInvoker(DoStepTo), new object[] { nVal });
        }

        // If this property is true, then you should abort work
        public bool IsAborting
        {
            get {
                return fAbortEvent.WaitOne(0, false);
            }
        }

        // Call this method from the worker thread to finalize the progress meter
        public void End(ThreadError threadError)
        {
            if (fRequiresClose) {
                Invoke(new EndInvoker(DoEnd), new object[] { threadError });
            }
        }

        // Partner of SetText(). Sets label text.
        private void DoSetText(String sText)
        {
            m_label.Text = sText;
        }

        // Partner of Increment(). Moves the progress bar and updates the status text.
        private void DoIncrement(int nVal)
        {
            m_progressbar.Increment(nVal);
            UpdateStatusText();
        }

        // Partner of StepTo(). Moves the progress bar.
        private void DoStepTo(int nVal)
        {
            m_progressbar.Value = nVal;
            UpdateStatusText();
        }

        // Partner of Begin(). Sets the up the progress bar.
        private void DoBegin(int nMinimum, int nMaximum)
        {
            DoBegin();
            DoSetRange(nMinimum, nMaximum);
        }

        // Starts the progress bar.
        private void DoBegin()
        {
            btnCancel.Enabled = true;
            ControlBox = true;
        }

        // Partner of SetRange(). Sets the limits of the progress bar.
        private void DoSetRange(int nMinimum, int nMaximum)
        {
            m_progressbar.Minimum = nMinimum;
            m_progressbar.Maximum = nMaximum;
            m_progressbar.Value = nMinimum;
            fTitleRoot = Text;
        }

        // The partner of End(). Handles finalising the progres meter.
        private void DoEnd(ThreadError threadError)
        {
            ThreadError.Error = threadError.Error;
            ThreadError.Message = threadError.Message;

            if (ThreadError.Error == 1) {
                DialogResult = DialogResult.Cancel;
            } else if (ThreadError.Error == 2) {
                DialogResult = DialogResult.Abort;
            } else if (ThreadError.Error == 3) {
                DialogResult = DialogResult.Retry;
            } else {
                DialogResult = DialogResult.OK;
            }
        }

        // Handles the form load, and sets an event to ensure that
        // intialization is synchronized with the appearance of the form.
        protected override void OnLoad(System.EventArgs e)
        {
            base.OnLoad(e);
            ControlBox = false;
            fInitEvent.Set();
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

        // Handler for 'Close' clicking
        protected override void OnClosing(System.ComponentModel.CancelEventArgs e)
        {
            fRequiresClose = false;
            AbortWork();
            base.OnClosing(e);
        }

        // Utility function that formats and updates the title bar text
        private void UpdateStatusText()
        {
            Text = fTitleRoot;
        }

        // Utility function to terminate the thread
        private void AbortWork()
        {
            fAbortEvent.Set();
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
