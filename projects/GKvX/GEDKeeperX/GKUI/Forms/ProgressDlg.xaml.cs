/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018-2023 by Sergey V. Zhdanovskih.
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
using GKCore;
using GKCore.Design;
using GKCore.Interfaces;

namespace GKUI.Forms
{
    public sealed partial class ProgressDlg : CommonForm, IProgressController
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
