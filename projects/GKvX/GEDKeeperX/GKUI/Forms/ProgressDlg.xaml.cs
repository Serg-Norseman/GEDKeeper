/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore;
using GKCore.Design;
using GKCore.Locales;

namespace GKUI.Forms
{
    public sealed partial class ProgressDlg : CommonDialog, IProgressController
    {
        private int fMaximum;
        private DateTime fStartTime;
        private bool fRequiresClose;
        private int fVal;

        public ProgressDlg()
        {
            InitializeComponent();

            //Title = LangMan.LS(LSID.Progress);
            lblTimePassed.Text = LangMan.LS(LSID.TimePassed);
            lblTimeRemain.Text = LangMan.LS(LSID.TimeRemain);
            lblTimeTotal.Text = LangMan.LS(LSID.TimeTotal);
        }

        public bool ShowModalX(IView owner)
        {
            return false;//(ShowModal((Control)owner) == DialogResult.Ok);
        }

        #region Private methods

        private void DoInit(string title, int max)
        {
            lblTitle.Text = title;
            fMaximum = max;
            ProgressBar1.Progress = 0;
            fStartTime = DateTime.Now;
            fVal = 0;
        }

        private void DoDone()
        {
            //Close();
        }

        private void DoStep(int value)
        {
            if (fVal == value) return;

            fVal = value;

            ProgressBar1.Progress = fVal / (double)fMaximum;

            double max = fMaximum;
            double pos = fVal;
            if (pos == 0.0d) pos = 1;

            TimeSpan passTime = DateTime.Now - fStartTime;
            TimeSpan restTime = new TimeSpan((long)Math.Truncate((passTime.Ticks / pos) * (max - pos)));
            TimeSpan sumTime = passTime + restTime;

            lblPassedVal.Text = TimeSpanToString(passTime);
            lblRemainVal.Text = TimeSpanToString(restTime);
            lblTotalVal.Text = TimeSpanToString(sumTime);

            //Invalidate();
        }

        private static string TimeSpanToString(TimeSpan ts)
        {
            return string.Format(null, "{0:00}:{1:00}:{2:00}", ts.Hours, ts.Minutes, ts.Seconds);
        }

        #endregion

        #region Protected methods

        /*protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            fRequiresClose = true;
        }

        protected override void OnClosing(CancelEventArgs e)
        {
            fRequiresClose = false;
            base.OnClosing(e);
        }*/

        public void Begin(int maximum, bool cancelable)
        {
        }

        public void Begin(string title, int maximum, bool cancelable = false)
        {
            try {
                //DoInit(title, max);
                //Application.Instance.Invoke(delegate { DoInit(title, max); });
            } catch (Exception ex) {
                Logger.WriteError("ProgressDlg.ProgressInit()", ex);
            }
        }

        public void End()
        {
            try {
                if (fRequiresClose) {
                    //DoDone();
                    //Application.Instance.Invoke(delegate { DoDone(); });
                }
            } catch (Exception ex) {
                Logger.WriteError("ProgressDlg.ProgressDone()", ex);
            }
        }

        public void End(ThreadError threadError)
        {
        }

        public void SetText(string text)
        {
        }

        public void Increment(int value = 1)
        {
            try {
                //DoStep(fVal + 1);
                //Application.Instance.Invoke(delegate { DoStep(fVal + 1); });
            } catch {
                // dummy
            }
        }

        public void StepTo(int value)
        {
            try {
                //DoStep(value);
                //Application.Instance.Invoke(delegate { DoStep(value); });
            } catch {
                // dummy
            }
        }

        public bool IsCanceled
        {
            get {
                return false; //fCancelEvent.WaitOne(0, false);
            }
        }

        public void InvokeEx(Action action)
        {
        }

        public bool ShowModalX(object owner)
        {
            return true;
        }

        #endregion
    }
}
