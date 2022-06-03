/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using System.ComponentModel;
using System.Threading;
using System.Windows.Forms;
using GKCore;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class ProgressDlg : Form
    {
        private readonly ManualResetEvent fCancelEvent;
        private bool fRequiresClose;
        private DateTime fStartTime;
        private int fVal;

        public ProgressDlg()
        {
            InitializeComponent();

            fCancelEvent = new ManualResetEvent(false);

            Text = LangMan.LS(LSID.LSID_Progress);
            lblTimePassed.Text = LangMan.LS(LSID.LSID_TimePassed);
            lblTimeRemain.Text = LangMan.LS(LSID.LSID_TimeRemain);
            lblTimeTotal.Text = LangMan.LS(LSID.LSID_TimeTotal);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            fCancelEvent.Set();
        }

        #region Private methods

        private void DoInit(string title, int max, bool cancelable = false)
        {
            lblTitle.Text = title;
            ProgressBar1.Maximum = max;
            ProgressBar1.Minimum = 0;
            ProgressBar1.Value = 0;
            fStartTime = DateTime.Now;
            fVal = -1;
            btnCancel.Enabled = cancelable;

            DoStep(0);
        }

        private void DoDone()
        {
            Close();
        }

        private void DoStep(int value)
        {
            if (fVal == value) return;

            fVal = value;
            ProgressBar1.Value = fVal;

            double max = ProgressBar1.Maximum;
            double pos = fVal;
            if (pos == 0.0d) pos = 1;

            TimeSpan passTime = DateTime.Now - fStartTime;
            TimeSpan restTime = new TimeSpan((long)Math.Truncate((passTime.Ticks / pos) * (max - pos)));
            TimeSpan sumTime = passTime + restTime;

            lblPassedVal.Text = TimeSpanToString(passTime);
            lblRemainVal.Text = TimeSpanToString(restTime);
            lblTotalVal.Text = TimeSpanToString(sumTime);
        }

        private static string TimeSpanToString(TimeSpan ts)
        {
            return string.Format(null, "{0:00}:{1:00}:{2:00}", ts.Hours, ts.Minutes, ts.Seconds);
        }

        private void InvokeEx(MethodInvoker method)
        {
            try {
                if (InvokeRequired) {
                    BeginInvoke(method, null);
                } else {
                    method();
                }
            } catch {
                // dummy
            }
        }

        #endregion

        #region Protected methods

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            fRequiresClose = true;
        }

        protected override void OnClosing(CancelEventArgs e)
        {
            fRequiresClose = false;
            fCancelEvent.Set();
            base.OnClosing(e);
        }

        internal void ProgressInit(string title, int max, bool cancelable = false)
        {
            InvokeEx(delegate {
                DoInit(title, max, cancelable);
            });
        }

        internal void ProgressDone()
        {
            InvokeEx(delegate {
                if (fRequiresClose) {
                    DoDone();
                }
            });
        }

        internal void ProgressStep()
        {
            InvokeEx(delegate {
                DoStep(fVal + 1);
            });
        }

        internal void ProgressStep(int value)
        {
            InvokeEx(delegate {
                DoStep(value);
            });
        }

        public bool IsCanceled
        {
            get {
                return fCancelEvent.WaitOne(0, false);
            }
        }

        #endregion
    }

    public sealed class ProgressController : IProgressController
    {
        private bool fCancelable;
        private volatile bool fFormLoaded;
        private int fMax;
        //private ManualResetEvent fMRE = new ManualResetEvent(false);
        private IntPtr fParentHandle;
        private ProgressDlg fProgressForm;
        private Thread fThread;
        private string fTitle;
        private int fVal;

        public void ProgressInit(string title, int max, bool cancelable = false)
        {
            if (fProgressForm != null) {
                fProgressForm.ProgressInit(title, max, cancelable);
            } else {
                fFormLoaded = false;
                fTitle = title;
                fMax = max;
                fCancelable = cancelable;
                fParentHandle = AppHost.Instance.GetTopWindowHandle();

                fThread = new Thread(ShowProgressForm);
                fThread.SetApartmentState(ApartmentState.STA);
                fThread.Start();

                while (!fFormLoaded) {
                    Thread.Sleep(50);
                }
                //fMRE.WaitOne();
            }

            fVal = 0;
        }

        public void ProgressDone()
        {
            if (fProgressForm != null) {
                fProgressForm.ProgressDone();
                //fProgressForm.Dispose();
                fProgressForm = null;
            }
        }

        public void ProgressStep()
        {
            if (fProgressForm != null) {
                fProgressForm.ProgressStep(fVal++);
            }
        }

        public void ProgressStep(int value)
        {
            if (fProgressForm != null) {
                fProgressForm.ProgressStep(value);
            }
        }

        private void ShowProgressForm()
        {
            fProgressForm = new ProgressDlg();
            fProgressForm.ProgressInit(fTitle, fMax, fCancelable);
            fProgressForm.Load += ProgressForm_Load;

            if (fParentHandle != IntPtr.Zero) {
                UIHelper.CenterFormByParent(fProgressForm, fParentHandle);
            }

            fProgressForm.ShowDialog();
            //fProgressForm.Close();
        }

        private void ProgressForm_Load(object sender, EventArgs e)
        {
            //fMRE.Set();
            fFormLoaded = true;
        }

        public bool IsCanceled
        {
            get {
                return (fProgressForm != null) && fProgressForm.IsCanceled;
            }
        }
    }
}
