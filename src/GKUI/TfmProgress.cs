using System;
using System.Windows.Forms;

using GKCore;
using GKCore.Sys;

namespace GKUI
{
	public partial class TfmProgress : Form
	{
		private static TfmProgress frm;
		private DateTime StartTime;

		private void Step()
		{
			TimeSpan PassTime = DateTime.Now - this.StartTime;
			this.Label7.Text = SysUtils.TimeSpanToString(PassTime);
			int count = this.ProgressBar1.Maximum;
			int pos = this.ProgressBar1.Value;
			if (pos == 0)
			{
				pos = 1;
			}
			TimeSpan RestTime = new TimeSpan(SysUtils.Trunc(((double)PassTime.Ticks) / (double)pos * (double)(count - pos)));
			this.Label8.Text = SysUtils.TimeSpanToString(RestTime);
			TimeSpan tmp = PassTime + RestTime;
			this.Label9.Text = SysUtils.TimeSpanToString(tmp);
			this.ProgressBar1.Increment(1);
			base.Update();
		}

		public TfmProgress()
		{
			this.InitializeComponent();
			this.Text = GKL.LSList[135];
			this.Label2.Text = GKL.LSList[136];
			this.Label3.Text = GKL.LSList[137];
			this.Label4.Text = GKL.LSList[138];
		}

		public static void ProgressInit(int aMax, string aTitle)
		{
			TfmProgress.ProgressDone();
			TfmProgress.frm = new TfmProgress();
			TfmProgress.frm.ProgressBar1.Minimum = 0;
			TfmProgress.frm.ProgressBar1.Maximum = aMax;
			TfmProgress.frm.Label1.Text = aTitle;
			TfmProgress.frm.Show();
			TfmProgress.frm.StartTime = DateTime.Now;
			Application.DoEvents();
		}

		public static void ProgressDone()
		{
			if (TfmProgress.frm != null)
			{
				TfmProgress.frm.Dispose();
				TfmProgress.frm = null;
			}
		}

		public static void ProgressStep()
		{
			if (TfmProgress.frm != null)
			{
				TfmProgress.frm.Step();
			}
		}
	}
}
