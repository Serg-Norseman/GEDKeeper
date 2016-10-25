/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
            this.InitializeComponent();
            this.Text = LangMan.LS(LSID.LSID_Progress);
            this.lblTimePassed.Text = LangMan.LS(LSID.LSID_TimePassed);
            this.lblTimeRemain.Text = LangMan.LS(LSID.LSID_TimeRemain);
            this.lblTimeTotal.Text = LangMan.LS(LSID.LSID_TimeTotal);
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
            Invoke( new PStep( DoStep ), new object[] { this.fVal + 1 } );
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
            this.lblTitle.Text = title;
            this.ProgressBar1.Maximum = max;
            this.ProgressBar1.Minimum = 0;
            this.ProgressBar1.Value = 0;
            this.fStartTime = DateTime.Now;
            this.fVal = 0;
        }

        internal void DoDone()
        {
            this.Close();
        }

        internal void DoStep(int value)
        {
            if (this.fVal == value) return;

            this.fVal = value;
            this.ProgressBar1.Value = this.fVal;

            double max = this.ProgressBar1.Maximum;
            double pos = this.fVal;
            if (pos == 0) pos = 1;

            TimeSpan passTime = DateTime.Now - this.fStartTime;
            TimeSpan restTime = new TimeSpan((long)Math.Truncate((passTime.Ticks / pos) * (max - pos)));
            TimeSpan sumTime = passTime + restTime;

            this.lblPassedVal.Text = TimeSpanToString(passTime);
            this.lblRemainVal.Text = TimeSpanToString(restTime);
            this.lblTotalVal.Text = TimeSpanToString(sumTime);

            this.Update();
        }

        #endregion

        public delegate void PInit(string title, int max);
        public delegate void PStep(int value);
    }

    public static class ProgressController
    {
        private static ProgressProxy pfrm;
        private static int fVal;

        public static void ProgressInit(string title, int max)
        {
            if (pfrm != null) {
                //pfrm.Close();
                pfrm.ProgressReset(title, max);
            } else {
                pfrm = new ProgressProxy(title, max);
            }

            fVal = 0;
        }

        public static void ProgressDone()
        {
            if (pfrm != null) {
                pfrm.Close();
                pfrm = null;
            }
        }

        public static void ProgressStep()
        {
            pfrm.UpdateProgress(fVal++);
            //System.Threading.Thread.Sleep(0); // debug
        }

        public static void ProgressStep(int value)
        {
            pfrm.UpdateProgress(value);
            //System.Threading.Thread.Sleep(0); // debug
        }
    }

    internal sealed class ProgressProxy
    {
        private ProgressDlg fProgressForm;
        private readonly string fTitle;
        private readonly int fMax;
        //private ManualResetEvent fMRE = new ManualResetEvent(false);
        private bool fFormLoaded = false;
        private readonly Thread fThread;
        private IntPtr fParentHandle;

        public ProgressProxy(string title, int max)
        {
            this.fTitle = title;
            this.fMax = max;
            this.fParentHandle = MainWin.Instance.Handle;

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
            fProgressForm.Load += new EventHandler(ProgressForm_Load);

            //fProgressForm.StartPosition = FormStartPosition.CenterScreen;
            UIHelper.CenterFormByParent(fProgressForm, this.fParentHandle);

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

        delegate void DelReset(string title, int max);
        delegate void DelUpdateProgress(int percent);
        delegate void DelClose();
    }
}
