using System;
using System.ComponentModel;
using System.Drawing;
using System.Threading;
using System.Windows.Forms;

using GKUI;

namespace GKSandbox
{
	public delegate void RenderDelegate();

	public sealed class TreeVizViewer : Form
	{
		private bool isDisposed = false;

		// gui
		private StatusBar statusBar = new StatusBar();
		private StatusBarPanel sbpCurrentFps = new StatusBarPanel();
		private TreeVizControl view;

		// runtime		
		private HighResolutionTimer highresTimer = new HighResolutionTimer();
		private ulong lastCalculationTime;
		private ulong framesDrawn;
		private double currentFramerate;
		private Thread backThread;

		public TreeVizViewer(TfmBase aBase, int minGens)
		{
			this.SuspendLayout();

			sbpCurrentFps.Alignment = HorizontalAlignment.Center;
			sbpCurrentFps.AutoSize = StatusBarPanelAutoSize.Contents;
			sbpCurrentFps.Text = "Current: -- FPS";

			statusBar.Panels.AddRange(new StatusBarPanel[] { sbpCurrentFps });
			statusBar.ShowPanels = true;

			view = new TreeVizControl();
			view.Dock = DockStyle.Fill;

			this.Controls.AddRange(new Control[] { view, statusBar });

			this.Size = new Size(800, 600);
			this.StartPosition = FormStartPosition.CenterScreen;
			this.SizeChanged += new EventHandler(Form_SizeChanged);
			this.Activated += new EventHandler(Form_Activated);
			this.statusBar.Visible = true;

			this.ResumeLayout();

			view.CreateArborGraph(aBase, minGens, true);

			backThread = new System.Threading.Thread(new ThreadStart(DoRenderThread));
			backThread.IsBackground = true;
        	backThread.Start();
		}

		protected override void Dispose(bool disposing)
		{
			if (!isDisposed) {
				if (disposing) {
					backThread.Suspend();

					sbpCurrentFps.Dispose();

					if (highresTimer != null) {
						highresTimer.Dispose();
					}

					GC.SuppressFinalize(this);

					if (view != null) {
						view.Dispose();
					}
				}

				// Release Any Unmanaged Resources Here, If disposing Was false, Only The Following Code Is Executed
				sbpCurrentFps = null;
				highresTimer = null;
				view = null;
				base.Dispose(disposing);
			}
			isDisposed = true;
		}

		private void DoRenderThread()
		{
			while (true) {
				Invoke(new RenderDelegate(RenderStep));
			}
		}

		private void ResetFramerate()
		{
			lastCalculationTime = highresTimer.Count;
			framesDrawn = 0;
			currentFramerate = 0.0f;
		}

		private void RenderStep()
		{
			view.Redraw();

			framesDrawn++;
			ulong currentFrameTime = highresTimer.Count;
			ulong timerFrequency = highresTimer.Frequency;

			if ((currentFrameTime - lastCalculationTime) > timerFrequency) {
				currentFramerate = (framesDrawn * timerFrequency) / (currentFrameTime - lastCalculationTime);
				lastCalculationTime = currentFrameTime;
				framesDrawn = 0;
			}

			sbpCurrentFps.Text = "Current: " + currentFramerate.ToString() + " FPS; Selected: " + view.selObject;

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