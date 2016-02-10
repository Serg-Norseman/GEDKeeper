using System;
using System.Drawing;
using System.Threading;
using System.Windows.Forms;

using GKCore.Interfaces;

namespace GKTreeVizPlugin
{
    /// <summary>
    /// Localization: dirty
    /// </summary>
    public sealed class TreeVizViewer : Form
	{
		private delegate void RenderDelegate();

        private readonly IBaseWindow fBase;
        private readonly int fMinGens;

		private StatusBar statusBar;
		private StatusBarPanel sbpCurrentFps;
		private StatusBarPanel sbpKeysControl;
		private StatusBarPanel sbpTimeControl;
		private TreeVizControl view;

		private readonly HighResolutionTimer fHighresTimer;
        private readonly Thread fBackThread;

        private ulong fLastCalculationTime;
		private ulong fFramesDrawn;
		private double fCurrentFramerate;

		public TreeVizViewer(IBaseWindow aBase, int minGens)
		{
			statusBar = new StatusBar();
			sbpCurrentFps = new StatusBarPanel();
			sbpKeysControl = new StatusBarPanel();
			sbpTimeControl = new StatusBarPanel();
			
			this.SuspendLayout();

			sbpCurrentFps.Alignment = HorizontalAlignment.Center;
			sbpCurrentFps.AutoSize = StatusBarPanelAutoSize.Contents;
			sbpCurrentFps.Text = "Current: -- FPS";

			sbpKeysControl.Alignment = HorizontalAlignment.Left;
			sbpKeysControl.AutoSize = StatusBarPanelAutoSize.Contents;
			sbpKeysControl.Text = "Debug (D); Free-rotate (R)";

			sbpTimeControl.Alignment = HorizontalAlignment.Left;
			sbpTimeControl.AutoSize = StatusBarPanelAutoSize.Contents;
			sbpTimeControl.Text = "Time stop (T)";

			statusBar.Panels.AddRange(new StatusBarPanel[] { sbpCurrentFps, sbpKeysControl, sbpTimeControl });
			statusBar.ShowPanels = true;

			view = new TreeVizControl();
			view.Parent = this;

			this.Controls.AddRange(new Control[] { view, statusBar });

			this.Size = new Size(800, 600);
			this.StartPosition = FormStartPosition.CenterScreen;
			this.SizeChanged += Form_SizeChanged;
			this.Activated += Form_Activated;
			this.Load += Form_Load;

			this.ResumeLayout();

			this.fHighresTimer = new HighResolutionTimer();
			this.fBackThread = new Thread(DoRenderThread);
			this.fBackThread.IsBackground = true;

			this.fBase = aBase;
			this.fMinGens = minGens;
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing) {
				this.fBackThread.Suspend();
				this.fHighresTimer.Dispose();

				sbpCurrentFps.Dispose();
				view.Dispose();
			}

			base.Dispose(disposing);
		}

        private void Form_Load(object sender, EventArgs e)
        {
        	this.fBackThread.Start();
			view.CreateArborGraph(this.fBase, this.fMinGens, true);
        }

		private void DoRenderThread()
		{
			while (true) {
				Invoke(new RenderDelegate(this.RenderStep));
			}
		}

		private void ResetFramerate()
		{
			this.fLastCalculationTime = this.fHighresTimer.Count;
			this.fFramesDrawn = 0;
			this.fCurrentFramerate = 0.0f;
		}

		private void RenderStep()
		{
			//view.Redraw();
			view.Refresh();

			this.fFramesDrawn++;
			ulong currentFrameTime = this.fHighresTimer.Count;
			ulong timerFrequency = this.fHighresTimer.Frequency;

			if ((currentFrameTime - this.fLastCalculationTime) > timerFrequency) {
				this.fCurrentFramerate = (fFramesDrawn * timerFrequency) / (currentFrameTime - this.fLastCalculationTime);
				this.fLastCalculationTime = currentFrameTime;
				this.fFramesDrawn = 0;
			}

			sbpCurrentFps.Text = "Current: " + fCurrentFramerate.ToString() + " FPS; Selected: " + view.SelectedObject;

			Application.DoEvents();
		}

		void Form_Activated(object sender, EventArgs e)
		{
			this.ResetFramerate();
		}

		void Form_SizeChanged(object sender, EventArgs e)
		{
			this.ResetFramerate();
		}

	}
}