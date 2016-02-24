using System;
using System.ComponentModel;
using System.Threading;
using System.Windows.Forms;

using GKCore;
using GKCore.Interfaces;

namespace GKUI
{
	/// <summary>
	/// 
	/// </summary>
	public partial class TfmProgress : Form, IProgressController
	{
		private static TfmProgress fInstance;
		private DateTime fStartTime;

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
			ProgressDone();

			fInstance = new TfmProgress();
			fInstance.DoInit(title, max);
			fInstance.Show();
			fInstance.Update();
		}

		public static void ProgressDone()
		{
		    if (fInstance == null) return;

            fInstance.Hide();
		    fInstance.Dispose();
		    fInstance = null;
		}

		public static void ProgressStep()
		{
            if (fInstance == null) return;

            fInstance.DoStep(fInstance.fVal + 1);
            fInstance.Update();
		}
		
		public static void ProgressStep(int value)
		{
            if (fInstance == null) return;

            fInstance.DoStep(value);
            fInstance.Update();
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
			this.fStartTime = DateTime.Now;
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

			double max = this.ProgressBar1.Maximum;
			double pos = this.fVal;
			if (pos == 0) pos = 1;

			TimeSpan passTime = DateTime.Now - this.fStartTime;
			TimeSpan restTime = new TimeSpan((long)Math.Truncate((passTime.Ticks / pos) * (max - pos)));
			TimeSpan sumTime = passTime + restTime;

			this.Label7.Text = TimeSpanToString(passTime);
			this.Label8.Text = TimeSpanToString(restTime);
			this.Label9.Text = TimeSpanToString(sumTime);
		}

		#endregion
	}
}
