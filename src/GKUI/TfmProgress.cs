using System;
using System.Windows.Forms;

using Ext.Utils;
using GKCore;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI
{
	public partial class TfmProgress : Form
	{
		private static TfmProgress form;
		private DateTime StartTime;

		public TfmProgress()
		{
			this.InitializeComponent();
			this.Text = LangMan.LSList[135];
			this.Label2.Text = LangMan.LSList[136];
			this.Label3.Text = LangMan.LSList[137];
			this.Label4.Text = LangMan.LSList[138];
		}

		private string TimeSpanToString(TimeSpan ts)
		{
			return string.Format("{0:00}:{1:00}:{2:00}", new object[] { ts.Hours, ts.Minutes, ts.Seconds });
		}

		private void ProgressUpdate()
		{
			double count = (double)this.ProgressBar1.Maximum;
			double pos = (double)this.ProgressBar1.Value;
			if (pos == 0) pos = 1;
			//Text = pos.ToString() + " / " + count.ToString();

			TimeSpan pass_time = DateTime.Now - this.StartTime;
			TimeSpan rest_time = new TimeSpan(SysUtils.Trunc((double)(pass_time.Ticks) / pos * (count - pos)));
			TimeSpan sum_time = pass_time + rest_time;

			this.Label7.Text = TimeSpanToString(pass_time);
			this.Label8.Text = TimeSpanToString(rest_time);
			this.Label9.Text = TimeSpanToString(sum_time);

			base.Update();
		}

		public static void ProgressInit(int aMax, string aTitle)
		{
			TfmProgress.ProgressDone();
			TfmProgress.form = new TfmProgress();
			TfmProgress.form.ProgressBar1.Minimum = 0;
			TfmProgress.form.ProgressBar1.Maximum = aMax;
			TfmProgress.form.Label1.Text = aTitle;
			TfmProgress.form.Show();
			TfmProgress.form.StartTime = DateTime.Now;
			Application.DoEvents();
		}

		public static void ProgressDone()
		{
			if (TfmProgress.form != null)
			{
				TfmProgress.form.Hide();
				TfmProgress.form.Dispose();
				TfmProgress.form = null;
			}
		}

		public static int Progress
		{
			get {
				return ((TfmProgress.form == null) ? 0 : TfmProgress.form.ProgressBar1.Value);
			}
			set {
				if (TfmProgress.form != null) {
					TfmProgress.form.ProgressBar1.Value = value;
					TfmProgress.form.ProgressUpdate();
					//Thread.Sleep(1);
				}
			}
		}

		public static void ProgressStep()
		{
			if (TfmProgress.form != null)
			{
				TfmProgress.form.ProgressBar1.Increment(1);
				TfmProgress.form.ProgressUpdate();
			}
		}
	}
}
