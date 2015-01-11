using System;
using System.ComponentModel;
using System.Threading;
using System.Windows.Forms;

using ExtUtils;
using GKCore;
using GKCore.Interfaces;

/// <summary>
/// 
/// </summary>

namespace GKUI
{
	public partial class TfmProgress : Form, IProgressController
	{
		private static TfmProgress form;
		private DateTime StartTime;

		public TfmProgress()
		{
			this.InitializeComponent();
			this.Text = LangMan.LS(LSID.LSID_Progress);
			this.Label2.Text = LangMan.LS(LSID.LSID_TimePassed);
			this.Label3.Text = LangMan.LS(LSID.LSID_TimeRemain);
			this.Label4.Text = LangMan.LS(LSID.LSID_TimeTotal);
		}

		private static string TimeSpanToString(TimeSpan ts)
		{
			return string.Format(null, "{0:00}:{1:00}:{2:00}", new object[] { ts.Hours, ts.Minutes, ts.Seconds });
		}

		#region Old funcs

		public static void ProgressInit(string title, int max)
		{
			TfmProgress.ProgressDone();

			TfmProgress.form = new TfmProgress();
			TfmProgress.form.DoInit(title, max);
			TfmProgress.form.Show();
			TfmProgress.form.Update();
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

		public static void ProgressStep()
		{
			TfmProgress pFrm = TfmProgress.form;

			if (pFrm != null) {
				pFrm.DoStep(pFrm.fVal + 1);
				pFrm.Update();
			}
		}
		
		public static void ProgressStep(int value)
		{
			TfmProgress pFrm = TfmProgress.form;

			if (pFrm != null) {
				pFrm.DoStep(value);
				pFrm.Update();
			}
		}

		#endregion

		#region New Funcs

		private readonly ManualResetEvent initEvent = new ManualResetEvent(false);
		private readonly ManualResetEvent abortEvent = new ManualResetEvent(false);
		private bool requiresClose;
		private int fVal;

		public delegate void PInit(string title, int max);
		public delegate void PStep(int value);

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

		#region Common

		private void DoInit(string title, int max)
		{
			this.Label1.Text = title;
			this.ProgressBar1.Maximum = max;
			this.ProgressBar1.Minimum = 0;
			this.ProgressBar1.Value = 0;
			this.StartTime = DateTime.Now;
			this.fVal = 0;
		}

		private void DoDone()
		{
			this.Close();
		}

		private void DoStep(int value)
		{
			this.fVal = value;
			this.ProgressBar1.Value = this.fVal;

			double max = (double)this.ProgressBar1.Maximum;
			double pos = (double)this.fVal;
			if (pos == 0) pos = 1;

			TimeSpan pass_time = DateTime.Now - this.StartTime;
			TimeSpan rest_time = new TimeSpan(SysUtils.Trunc((pass_time.Ticks / pos) * (max - pos)));
			TimeSpan sum_time = pass_time + rest_time;

			this.Label7.Text = TimeSpanToString(pass_time);
			this.Label8.Text = TimeSpanToString(rest_time);
			this.Label9.Text = TimeSpanToString(sum_time);
		}

		#endregion
	}
}
