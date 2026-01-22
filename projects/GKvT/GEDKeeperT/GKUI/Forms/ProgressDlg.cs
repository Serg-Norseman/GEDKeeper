/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Threading;
using GKCore.Design;
using GKCore.Locales;
using Terminal.Gui;

namespace GKUI.Forms
{
    public sealed partial class ProgressDlg : CommonDialog, IProgressDialog
    {
        private readonly ManualResetEvent fInitEvent;
        private readonly ManualResetEvent fBeginEvent;
        private readonly ManualResetEvent fCancelEvent;
        private bool fRequiresClose = true;
        private DateTime fStartTime;
        private int fMaximum;
        private int fVal;


        public ThreadError ThreadError { get; set; }


        public ProgressDlg()
        {
            InitializeComponent();

            this.Closed += ProgressDlg_Closed;

            ThreadError = new ThreadError(1, "No error");
            fInitEvent = new ManualResetEvent(false);
            fBeginEvent = new ManualResetEvent(false);
            fCancelEvent = new ManualResetEvent(false);

            Title = LangMan.LS(LSID.Progress);
            lblTimePassed.Text = LangMan.LS(LSID.TimePassed);
            lblTimeRemain.Text = LangMan.LS(LSID.TimeRemain);
            lblTimeTotal.Text = LangMan.LS(LSID.TimeTotal);
            btnCancel.Text = LangMan.LS(LSID.DlgCancel);

            lblPassedVal.Text = "00:00:00";
            lblRemainVal.Text = "00:00:00";
            lblTotalVal.Text = "00:00:00";
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

        public override void OnLoaded()
        {
            base.OnLoaded();
            fRequiresClose = true;
            fInitEvent.Set();
        }

        private void ProgressDlg_Closed(Terminal.Gui.Toplevel obj)
        {
            fRequiresClose = false;
            fCancelEvent.Set();
        }

        private void btnCancel_Click(MouseEventArgs e)
        {
            //Cursor.Current = Cursors.WaitCursor;
            //Cursor.Show();
            fCancelEvent.Set();
        }

        #region Progression methods

        private void DoSetText(string text)
        {
            lblTitle.Text = text;
        }

        private void DoBegin(int maximum, bool cancelable)
        {
            fMaximum = maximum;
            ProgressBar1.Fraction = 0.0f;
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
            double max = fMaximum;
            double pos = fVal;
            if (pos == 0.0d) pos = 1;

            TimeSpan passTime = DateTime.Now - fStartTime;
            TimeSpan restTime = new TimeSpan((long)((passTime.Ticks / pos) * (max - pos)));
            TimeSpan sumTime = passTime + restTime;

            Application.MainLoop?.Invoke(() => {
                ProgressBar1.Fraction = (float)fVal / fMaximum;
                lblPassedVal.Text = TimeSpanToString(passTime);
                lblRemainVal.Text = TimeSpanToString(restTime);
                lblTotalVal.Text = TimeSpanToString(sumTime);
                SetNeedsDisplay();
            });
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
                /*if (InvokeRequired) {
                    BeginInvoke(action, null);
                } else*/
                {
                    action();
                }
            } catch {
                // dummy
            }
        }

        #endregion
    }
}
