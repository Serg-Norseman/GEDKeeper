/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using GKCommon;
using GKCore;
using GKCore.Interfaces;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class ProgressDlg : Form, IProgressController
    {
        private DateTime fStartTime;

        public ProgressDlg()
        {
            InitializeComponent();
            Text = LangMan.LS(LSID.LSID_Progress);
            lblTimePassed.Text = LangMan.LS(LSID.LSID_TimePassed);
            lblTimeRemain.Text = LangMan.LS(LSID.LSID_TimeRemain);
            lblTimeTotal.Text = LangMan.LS(LSID.LSID_TimeTotal);
        }

        private static string TimeSpanToString(TimeSpan ts)
        {
            return string.Format(null, "{0:00}:{1:00}:{2:00}", ts.Hours, ts.Minutes, ts.Seconds);
        }

        #region Temp methods for future

        private readonly ManualResetEvent initEvent = new ManualResetEvent(false);
        private readonly ManualResetEvent abortEvent = new ManualResetEvent(false);
        private bool requiresClose;
        private int fVal;

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            initEvent.Set();
            requiresClose = true;
        }

        protected override void OnClosing(CancelEventArgs e)
        {
            requiresClose = false;
            abortEvent.Set();
            base.OnClosing(e);
        }

        void IProgressController.ProgressInit(string title, int max)
        {
            initEvent.WaitOne();
            Invoke( new PInit( DoInit ), new object[] { title, max } );
        }

        void IProgressController.ProgressDone()
        {
            if (requiresClose) {
                Invoke( new MethodInvoker( DoDone ) );
            }
        }

        void IProgressController.ProgressStep()
        {
            Invoke( new PStep( DoStep ), new object[] { fVal + 1 } );
        }

        void IProgressController.ProgressStep(int value)
        {
            Invoke( new PStep( DoStep ), new object[] { value } );
        }

        public bool IsAborting
        {
            get {
                return abortEvent.WaitOne( 0, false );
            }
        }

        #endregion

        #region Protected methods

        internal void DoInit(string title, int max)
        {
            lblTitle.Text = title;
            ProgressBar1.Maximum = max;
            ProgressBar1.Minimum = 0;
            ProgressBar1.Value = 0;
            fStartTime = DateTime.Now;
            fVal = 0;
        }

        internal void DoDone()
        {
            Close();
        }

        internal void DoStep(int value)
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

            Update();
        }

        #endregion

        public delegate void PInit(string title, int max);
        public delegate void PStep(int value);
    }

    public static class ProgressController
    {
        private static ProgressProxy fProxy;
        private static int fVal;

        public static void ProgressInit(string title, int max)
        {
            if (fProxy != null) {
                fProxy.ProgressReset(title, max);
            } else {
                fProxy = new ProgressProxy(title, max);
            }

            fVal = 0;
        }

        public static void ProgressDone()
        {
            if (fProxy == null) return;

            fProxy.Close();
            fProxy = null;
        }

        public static void ProgressStep()
        {
            fProxy.UpdateProgress(fVal++);
            //System.Threading.Thread.Sleep(0); // debug
        }

        public static void ProgressStep(int value)
        {
            fProxy.UpdateProgress(value);
            //System.Threading.Thread.Sleep(0); // debug
        }
    }

    internal sealed class ProgressProxy
    {
        private ProgressDlg fProgressForm;
        private readonly string fTitle;
        private readonly int fMax;
        //private ManualResetEvent fMRE = new ManualResetEvent(false);
        private bool fFormLoaded;
        private readonly Thread fThread;
        private readonly IntPtr fParentHandle;

        public ProgressProxy(string title, int max)
        {
            fFormLoaded = false;
            fTitle = title;
            fMax = max;
            fParentHandle = MainWin.Instance.Handle;

            fThread = new Thread(ShowProgressForm);
            fThread.SetApartmentState(ApartmentState.STA);
            fThread.Start();

            while (!fFormLoaded)
            {
                Thread.Sleep(100);
            }
            //fMRE.WaitOne();
        }

        private void ShowProgressForm()
        {
            fProgressForm = new ProgressDlg();
            fProgressForm.DoInit(fTitle, fMax);
            fProgressForm.Load += ProgressForm_Load;

            //fProgressForm.StartPosition = FormStartPosition.CenterScreen;
            UIHelper.CenterFormByParent(fProgressForm, fParentHandle);

            fProgressForm.ShowDialog();
            fProgressForm.Close();
        }

        private void ProgressForm_Load(object sender, EventArgs e)
        {
            //fMRE.Set();
            fFormLoaded = true;
        }

        public void ProgressReset(string title, int max)
        {
            try
            {
                if (fProgressForm.InvokeRequired)
                {
                    fProgressForm.Invoke(new DelReset(ProgressReset), new object[] { title, max });
                }
                else
                {
                    fProgressForm.DoInit(title, max);
                }
            }
            catch
            {
            }
        }

        public void UpdateProgress(int percent)
        {
            try
            {
                if (fProgressForm.InvokeRequired)
                {
                    fProgressForm.Invoke(new DelUpdateProgress(UpdateProgress), new object[] { percent });
                }
                else
                {
                    fProgressForm.DoStep(percent);
                }
            }
            catch
            {
            }
        }

        public void Close()
        {
            if (fProgressForm.InvokeRequired)
            {
                fProgressForm.Invoke(new MethodInvoker(Close));
            }
            else
            {
                fProgressForm.DialogResult = DialogResult.OK;
            }
        }

        private delegate void DelReset(string title, int max);
        private delegate void DelUpdateProgress(int percent);
        private delegate void DelClose();
    }
}
