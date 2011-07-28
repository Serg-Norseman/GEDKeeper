using GKCore;
using GKSys;
using GKUI.Lists;
using System;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmTimeLine : Form
	{
		private StatusBarPanel StatusBarPanel1;
		private StatusBarPanel StatusBarPanel2;
		private TrackBar tbTimeLine;
		private StatusBar StatusBar1;
		private TfmBase FBase;

		private void StatusUpdate()
		{
			if (this.FBase != null)
			{
				this.StatusBarPanel1.Text = string.Concat(new string[]
				{
					GKL.LSList[129], 
					": ", 
					this.GetIListMan().YearMin.ToString(), 
					" - ", 
					this.GetIListMan().YearMax.ToString()
				});
				this.StatusBarPanel2.Text = GKL.LSList[130] + ": " + this.FBase.TimeLine_GetYear().ToString();
			}
		}
		private TIndividualListMan GetIListMan()
		{
			return this.FBase.ListPersons.ListMan as TIndividualListMan;
		}
		private void InitializeComponent()
		{
			this.tbTimeLine = new TrackBar();
			this.StatusBar1 = new StatusBar();
			this.StatusBarPanel1 = new StatusBarPanel();
			this.StatusBarPanel2 = new StatusBarPanel();
			((ISupportInitialize)this.tbTimeLine).BeginInit();
			((ISupportInitialize)this.StatusBarPanel1).BeginInit();
			((ISupportInitialize)this.StatusBarPanel2).BeginInit();
			base.SuspendLayout();
			this.tbTimeLine.Dock = DockStyle.Fill;
			this.tbTimeLine.LargeChange = 1;
			this.tbTimeLine.Location = new Point(0, 0);
			this.tbTimeLine.Minimum = 5;
			this.tbTimeLine.Name = "tbTimeLine";
			this.tbTimeLine.Size = new Size(524, 42);
			this.tbTimeLine.TabIndex = 0;
			this.tbTimeLine.Value = 5;
			this.tbTimeLine.ValueChanged += new EventHandler(this.tbTimeLine_ValueChanged);
			this.StatusBar1.Location = new Point(0, 42);
			this.StatusBar1.Name = "StatusBar1";
			StatusBar.StatusBarPanelCollection arg_145_0 = this.StatusBar1.Panels;
			StatusBarPanel[] array = null;
			StatusBarPanel[] array2 = array;
			StatusBarPanel[] array3;
			StatusBarPanel[] expr_117 = array3 = new StatusBarPanel[2];
			if (array2 != null)
			{
				int num;
				if ((num = array2.Length) > 2)
				{
					num = 2;
				}
				if (num > 0)
				{
					Array.Copy(array2, array3, num);
				}
			}
			array = expr_117;
			array[0] = this.StatusBarPanel1;
			array[1] = this.StatusBarPanel2;
			arg_145_0.AddRange(array);
			this.StatusBar1.ShowPanels = true;
			this.StatusBar1.Size = new Size(524, 19);
			this.StatusBar1.TabIndex = 1;
			this.StatusBarPanel1.Text = " ";
			this.StatusBarPanel1.Width = 250;
			this.StatusBarPanel2.Text = " ";
			this.StatusBarPanel2.Width = 250;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.ClientSize = new Size(524, 61);
			base.Controls.Add(this.tbTimeLine);
			base.Controls.Add(this.StatusBar1);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedToolWindow;
			base.Name = "TfmTimeLine";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.Manual;
			this.Text = "Линия времени";
			base.TopMost = true;
			base.Closed += new EventHandler(this.TfmTimeLine_Closed);
			((ISupportInitialize)this.tbTimeLine).EndInit();
			((ISupportInitialize)this.StatusBarPanel1).EndInit();
			((ISupportInitialize)this.StatusBarPanel2).EndInit();
			base.ResumeLayout(false);
		}
		private void TfmTimeLine_Closed(object sender, EventArgs e)
		{
			int arg_18_0 = 0;
			Form[] mdiChildren = GKL.fmGEDKeeper.MdiChildren;
			int num = ((mdiChildren != null) ? mdiChildren.Length : 0) - 1;
			int i = arg_18_0;
			if (num >= i)
			{
				num++;
				do
				{
					if (GKL.fmGEDKeeper.MdiChildren[i] is TfmBase)
					{
						(GKL.fmGEDKeeper.MdiChildren[i] as TfmBase).TimeLine_Done();
					}
					i++;
				}
				while (i != num);
			}
			GKL.fmGEDKeeper.miTimeLine.Checked = false;
			GKL.fmGEDKeeper.fmTimeLine = null;
		}
		private void tbTimeLine_ValueChanged(object sender, EventArgs e)
		{
			if (this.FBase != null)
			{
				this.FBase.TimeLine_SetYear(this.tbTimeLine.Value);
			}
			this.StatusUpdate();
		}
		public TfmTimeLine()
		{
			this.InitializeComponent();
			this.CheckTimeWin(GKL.fmGEDKeeper.GetCurrentFile());
			this.Text = GKL.LSList[33];
		}
		public void CheckTimeWin(TfmBase aBase)
		{
			this.FBase = aBase;
			if (this.FBase != null)
			{
				try
				{
					int max = this.GetIListMan().YearMax + 1;
					int min = this.GetIListMan().YearMin - 1;
					int cur = this.FBase.TimeLine_GetYear();
					if (min > max)
					{
						int x = min;
						min = max;
						max = x;
					}
					if (cur < min)
					{
						cur = min;
					}
					if (cur > max)
					{
						cur = max;
					}
					this.tbTimeLine.ValueChanged -= new EventHandler(this.tbTimeLine_ValueChanged);
					this.tbTimeLine.Maximum = max;
					this.tbTimeLine.Minimum = min;
					this.tbTimeLine.Value = cur;
					this.tbTimeLine.ValueChanged += new EventHandler(this.tbTimeLine_ValueChanged);
					this.StatusUpdate();
					this.FBase.TimeLine_Init();
				}
				finally
				{
				}
			}
		}
	}
}
