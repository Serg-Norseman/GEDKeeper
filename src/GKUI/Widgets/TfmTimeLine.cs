using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

using GKCore;
using GKUI.Lists;

namespace GKUI
{
	public partial class TfmTimeLine : Form
	{
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

		private void TfmTimeLine_Closed(object sender, EventArgs e)
		{
			Form[] mdiChildren = GKUI.TfmGEDKeeper.Instance.MdiChildren;
			int num = ((mdiChildren != null) ? mdiChildren.Length : 0) - 1;
			for (int i = 0; i <= num; i++)
			{
				if (GKUI.TfmGEDKeeper.Instance.MdiChildren[i] is TfmBase)
				{
					(GKUI.TfmGEDKeeper.Instance.MdiChildren[i] as TfmBase).TimeLine_Done();
				}
			}
			GKUI.TfmGEDKeeper.Instance.miTimeLine.Checked = false;
			GKUI.TfmGEDKeeper.Instance.fmTimeLine = null;
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
			this.CheckTimeWin(GKUI.TfmGEDKeeper.Instance.GetCurrentFile());
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
