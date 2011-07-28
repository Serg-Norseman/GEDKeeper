using GKCore;
using GKSys;
using System;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmProgress : Form
	{
		private static TfmProgress frm;
		private Label Label2;
		private Label Label3;
		private Label Label7;
		private Label Label8;
		private Label Label4;
		private Label Label9;
		private ProgressBar ProgressBar1;
		private Label Label1;
		private DateTime StartTime;

		private void Step()
		{
			TimeSpan PassTime = DateTime.Now - this.StartTime;
			this.Label7.Text = TGKSys.TimeSpanToString(PassTime);
			int count = this.ProgressBar1.Maximum;
			int pos = this.ProgressBar1.Value;
			if (pos == 0)
			{
				pos = 1;
			}
			TimeSpan RestTime = new TimeSpan(BDSSystem.Trunc(((double)PassTime.Ticks) / (double)pos * (double)(count - pos)));
			this.Label8.Text = TGKSys.TimeSpanToString(RestTime);
			TimeSpan tmp = PassTime + RestTime;
			this.Label9.Text = TGKSys.TimeSpanToString(tmp);
			this.ProgressBar1.Increment(1);
			base.Update();
		}

		private void InitializeComponent()
		{
			this.ProgressBar1 = new ProgressBar();
			this.Label1 = new Label();
			this.Label2 = new Label();
			this.Label3 = new Label();
			this.Label7 = new Label();
			this.Label8 = new Label();
			this.Label4 = new Label();
			this.Label9 = new Label();
			base.SuspendLayout();
			this.ProgressBar1.Location = new Point(8, 24);
			this.ProgressBar1.Name = "ProgressBar1";
			this.ProgressBar1.Size = new Size(401, 16);
			this.ProgressBar1.TabIndex = 0;
			this.Label1.Location = new Point(8, 8);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(300, 13);
			this.Label1.TabIndex = 1;
			this.Label1.Text = "Label1";
			this.Label2.Location = new Point(8, 48);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(96, 13);
			this.Label2.TabIndex = 2;
			this.Label2.Text = "Времени прошло";
			this.Label3.Location = new Point(8, 64);
			this.Label3.Name = "Label3";
			this.Label3.Size = new Size(104, 13);
			this.Label3.TabIndex = 3;
			this.Label3.Text = "Времени осталось";
			this.Label7.Font = new Font("Tahoma", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 204);
			this.Label7.Location = new Point(184, 48);
			this.Label7.Name = "Label7";
			this.Label7.Size = new Size(225, 16);
			this.Label7.TabIndex = 4;
			this.Label7.TextAlign = ContentAlignment.TopRight;
			this.Label8.Font = new Font("Tahoma", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 204);
			this.Label8.Location = new Point(184, 64);
			this.Label8.Name = "Label8";
			this.Label8.Size = new Size(225, 16);
			this.Label8.TabIndex = 5;
			this.Label8.TextAlign = ContentAlignment.TopRight;
			this.Label4.Location = new Point(8, 80);
			this.Label4.Name = "Label4";
			this.Label4.Size = new Size(73, 13);
			this.Label4.TabIndex = 6;
			this.Label4.Text = "Времени всего";
			this.Label9.Font = new Font("Tahoma", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 204);
			this.Label9.Location = new Point(184, 80);
			this.Label9.Name = "Label9";
			this.Label9.Size = new Size(225, 16);
			this.Label9.TabIndex = 7;
			this.Label9.Text = "?";
			this.Label9.TextAlign = ContentAlignment.TopRight;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.ClientSize = new Size(417, 105);
			base.Controls.Add(this.ProgressBar1);
			base.Controls.Add(this.Label1);
			base.Controls.Add(this.Label2);
			base.Controls.Add(this.Label3);
			base.Controls.Add(this.Label7);
			base.Controls.Add(this.Label8);
			base.Controls.Add(this.Label4);
			base.Controls.Add(this.Label9);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedSingle;
			base.ControlBox = false;
			base.MinimizeBox = false;
			base.MaximizeBox = false;
			base.Name = "TfmProgress";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			base.TopMost = true;
			this.Text = "Прогресс";
			base.ResumeLayout(false);
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
