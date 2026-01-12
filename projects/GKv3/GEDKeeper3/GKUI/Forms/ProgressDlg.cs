/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.ComponentModel;
using System.Threading;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore.Design;
using GKCore.Locales;

namespace GKUI.Forms
{
    public sealed partial class ProgressDlg : CommonDialog, IProgressDialog
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Label lblTimePassed;
        private Label lblTimeRemain;
        private Label lblPassedVal;
        private Label lblRemainVal;
        private Label lblTimeTotal;
        private Label lblTotalVal;
        private ProgressBar ProgressBar1;
        private Label lblTitle;
        private Button btnCancel;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly ManualResetEvent fInitEvent;
        private readonly ManualResetEvent fBeginEvent;
        private readonly ManualResetEvent fCancelEvent;
        private bool fRequiresClose = true;
        private DateTime fStartTime;
        private int fVal;


        public ThreadError ThreadError { get; set; }


        public ProgressDlg()
        {
            XamlReader.Load(this);

            ThreadError = new ThreadError(1, "No error");
            fInitEvent = new ManualResetEvent(false);
            fBeginEvent = new ManualResetEvent(false);
            fCancelEvent = new ManualResetEvent(false);

            SetTitle(LangMan.LS(LSID.Progress));
            lblTimePassed.Text = LangMan.LS(LSID.TimePassed);
            lblTimeRemain.Text = LangMan.LS(LSID.TimeRemain);
            lblTimeTotal.Text = LangMan.LS(LSID.TimeTotal);
            btnCancel.Text = LangMan.LS(LSID.DlgCancel);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fCancelEvent.Dispose();
                fBeginEvent.Dispose();
                fInitEvent.Dispose();
            }
            base.Dispose(disposing);
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            fRequiresClose = true;
            //ControlBox = false;
            fInitEvent.Set();
        }

        protected override void OnClosing(CancelEventArgs e)
        {
            fRequiresClose = false;
            fCancelEvent.Set();
            base.OnClosing(e);
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            fCancelEvent.Set();
        }

        #region Progression methods

        private void DoSetText(string text)
        {
            lblTitle.Text = text;
        }

        private void DoBegin(int maximum, bool cancelable)
        {
            ProgressBar1.MaxValue = maximum;
            ProgressBar1.MinValue = 0;
            ProgressBar1.Value = 0;
            fStartTime = DateTime.Now;
            fVal = -1;
            btnCancel.Enabled = cancelable;

            fBeginEvent.Set();

            DoStep(0);
        }

        private void DoBegin(string title, int maximum, bool cancelable = false)
        {
            DoSetText(title);
            DoBegin(maximum, cancelable);
        }

        private void DoEnd()
        {
            Close();
        }

        private void DoEnd(ThreadError threadError)
        {
            ThreadError.Error = threadError.Error;
            ThreadError.Message = threadError.Message;

            /*if (ThreadError.Error == 1) {
                DialogResult = DialogResult.Cancel;
            } else if (ThreadError.Error == 2) {
                DialogResult = DialogResult.Abort;
            } else if (ThreadError.Error == 3) {
                DialogResult = DialogResult.Retry;
            } else {
                DialogResult = DialogResult.OK;
            }*/
        }

        private void DoIncrement(int val)
        {
            DoStep(fVal + val);
        }

        private void DoStep(int value)
        {
            fBeginEvent.WaitOne();

            if (fVal == value) return;

            // strange float bug
            if (fStartTime.Ticks == 0)
                fStartTime = DateTime.Now;

            fVal = value;
            ProgressBar1.Value = fVal;

            double max = ProgressBar1.MaxValue;
            double pos = fVal;
            if (pos == 0.0d) pos = 1;

            TimeSpan passTime = DateTime.Now - fStartTime;
            TimeSpan restTime = new TimeSpan((long)((passTime.Ticks / pos) * (max - pos)));
            TimeSpan sumTime = passTime + restTime;

            lblPassedVal.Text = TimeSpanToString(passTime);
            lblRemainVal.Text = TimeSpanToString(restTime);
            lblTotalVal.Text = TimeSpanToString(sumTime);
        }

        private static string TimeSpanToString(TimeSpan ts)
        {
            return string.Format(null, "{0:00}:{1:00}:{2:00}", ts.Hours, ts.Minutes, ts.Seconds);
        }

        public bool IsCanceled
        {
            get {
                return fCancelEvent.WaitOne(0, false);
            }
        }

        public void Begin(int maximum, bool cancelable)
        {
            fInitEvent.WaitOne();
            InvokeEx(delegate {
                DoBegin(maximum, cancelable);
            });
        }

        public void Begin(string title, int maximum, bool cancelable = false)
        {
            fInitEvent.WaitOne();
            InvokeEx(delegate {
                DoBegin(title, maximum, cancelable);
            });
        }

        public void End()
        {
            InvokeEx(delegate {
                if (fRequiresClose) {
                    DoEnd();
                }
            });
        }

        public void End(ThreadError threadError)
        {
            if (fRequiresClose) {
                InvokeEx(delegate {
                    DoEnd(threadError);
                });
            }
        }

        public void SetText(string text)
        {
            InvokeEx(delegate {
                DoSetText(text);
            });
        }

        public void Increment(int value = 1)
        {
            InvokeEx(delegate {
                DoIncrement(value);
            });
        }

        public void StepTo(int value)
        {
            InvokeEx(delegate {
                DoStep(value);
            });
        }

        public void InvokeEx(Action action)
        {
            try {
                Application.Instance.Invoke(action);
            } catch {
                // dummy
            }
        }

        #endregion
    }
}
