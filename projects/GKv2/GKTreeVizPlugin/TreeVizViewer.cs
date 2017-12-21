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
using System.Diagnostics;
using System.Drawing;
using System.Threading;
using System.Windows.Forms;
using GKCore.Interfaces;

namespace GKTreeVizPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class TreeVizViewer : Form
    {
        private delegate void RenderDelegate();

        private readonly IBaseWindow fBase;
        private readonly int fMinGens;
        private readonly Stopwatch fHighresTimer;
        private readonly Thread fBackThread;
        private readonly ManualResetEvent fStopEvent;

        private long fLastCalculationTime;
        private long fFramesDrawn;

        private StatusBar fStatusBar;
        private StatusBarPanel sbpCurrentFps;
        private StatusBarPanel sbpKeysControl;
        private StatusBarPanel sbpTimeControl;
        private TreeVizControl fTreeVizView;

        public TreeVizViewer(IBaseWindow baseWin, int minGens)
        {
            InitializeComponent();

            fBase = baseWin;
            fMinGens = minGens;

            fHighresTimer = new Stopwatch();

            fStopEvent = new ManualResetEvent(false);
            fBackThread = new Thread(DoRenderThread);
            fBackThread.IsBackground = true;
            fBackThread.Start();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            fStatusBar = new StatusBar();
            sbpCurrentFps = new StatusBarPanel();
            sbpKeysControl = new StatusBarPanel();
            sbpTimeControl = new StatusBarPanel();

            SuspendLayout();

            sbpCurrentFps.Alignment = HorizontalAlignment.Center;
            sbpCurrentFps.AutoSize = StatusBarPanelAutoSize.Contents;
            sbpCurrentFps.Text = "Current: -- FPS";

            sbpKeysControl.Alignment = HorizontalAlignment.Left;
            sbpKeysControl.AutoSize = StatusBarPanelAutoSize.Contents;
            sbpKeysControl.Text = "Debug (D); Free-rotate (R)";

            sbpTimeControl.Alignment = HorizontalAlignment.Left;
            sbpTimeControl.AutoSize = StatusBarPanelAutoSize.Contents;
            sbpTimeControl.Text = "Time stop (T)";

            fStatusBar.Panels.AddRange(new StatusBarPanel[] { sbpCurrentFps, sbpKeysControl, sbpTimeControl });
            fStatusBar.ShowPanels = true;

            fTreeVizView = new TreeVizControl();
            fTreeVizView.Parent = this;

            Controls.AddRange(new Control[] { fTreeVizView, fStatusBar });

            Size = new Size(800, 600);
            StartPosition = FormStartPosition.CenterScreen;
            SizeChanged += Form_SizeChanged;
            Activated += Form_Activated;
            Load += Form_Load;
            Closing += Form_Closed;

            ResumeLayout();
        }

        private void Form_Load(object sender, EventArgs e)
        {
            SetActive(true);
            fTreeVizView.CreateArborGraph(fBase, fMinGens, true);
        }

        private void Form_Closed(object sender, CancelEventArgs e)
        {
            SetActive(false);
        }

        private void SetActive(bool value)
        {
            if (value) {
                fStopEvent.Reset();
            } else {
                fStopEvent.Set();
                //fBackThread.Join();
            }
        }

        private void DoRenderThread()
        {
            try {
                while (!fStopEvent.WaitOne(0)) {
                    Invoke((RenderDelegate)RenderStep);
                }
            } catch (Exception) {
            }
        }

        private void RenderStep()
        {
            fTreeVizView.Invalidate(false);

            fFramesDrawn++;
            long currentFrameTime = fHighresTimer.ElapsedMilliseconds;
            long timerFrequency = 1000;

            if ((currentFrameTime - fLastCalculationTime) > timerFrequency) {
                double fps = (double)(fFramesDrawn * timerFrequency) / (currentFrameTime - fLastCalculationTime);
                fLastCalculationTime = currentFrameTime;
                fFramesDrawn = 0;

                sbpCurrentFps.Text = "FPS: " + fps.ToString("0.00") + "; Selected: " + fTreeVizView.SelectedObject +
                    "; Current year: " + fTreeVizView.CurYear;
            }
        }

        private void ResetFramerate()
        {
            fHighresTimer.Reset();
            fLastCalculationTime = 0;
            fFramesDrawn = 0;
            fHighresTimer.Start();
        }

        private void Form_Activated(object sender, EventArgs e)
        {
            ResetFramerate();
        }

        private void Form_SizeChanged(object sender, EventArgs e)
        {
            ResetFramerate();
        }
    }
}