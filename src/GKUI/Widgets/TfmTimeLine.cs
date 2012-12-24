using System;
using System.Windows.Forms;

using GKCore;
using GKUI.Lists;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI
{
	public partial class TfmTimeLine : Form
	{
		private TfmBase FBase;

		public TfmTimeLine()
		{
			this.InitializeComponent();
			this.CheckTimeLine(GKUI.TfmGEDKeeper.Instance.GetCurrentFile());
			this.Text = LangMan.LSList[33];
		}

		private void TfmTimeLine_Closed(object sender, EventArgs e)
		{
			Form[] mdiChildren = GKUI.TfmGEDKeeper.Instance.MdiChildren;
			int num = ((mdiChildren != null) ? mdiChildren.Length : 0) - 1;
			for (int i = 0; i <= num; i++)
			{
				Form frm = GKUI.TfmGEDKeeper.Instance.MdiChildren[i];
				if (frm is TfmBase)
				{
					(frm as TfmBase).TimeLine_Done();
				}
			}
			GKUI.TfmGEDKeeper.Instance.miTimeLine.Checked = false;
			GKUI.TfmGEDKeeper.Instance.fmTimeLine = null;
		}

		private void tbTimeLine_ValueChanged(object sender, EventArgs e)
		{
			if (this.FBase != null)
			{
				this.FBase.TimeLineYear = this.tbTimeLine.Value;
			}
			this.StatusUpdate();
		}

		private void StatusUpdate()
		{
			if (this.FBase != null)
			{
				TIndividualListMan listman = this.FBase.ListPersons.ListMan as TIndividualListMan;
				this.StatusBarPanel1.Text = LangMan.LSList[129] + ": " + listman.YearMin.ToString() + " - " + listman.YearMax.ToString();
				this.StatusBarPanel2.Text = LangMan.LSList[130] + ": " + this.FBase.TimeLineYear.ToString();
			}
			else
			{
				this.StatusBarPanel1.Text = "";
				this.StatusBarPanel2.Text = "";
			}
		}

		public void UpdateControls()
		{
			if (this.FBase != null)
			{
				TIndividualListMan listman = this.FBase.ListPersons.ListMan as TIndividualListMan;

				int max = listman.YearMax + 1;
				int min = listman.YearMin - 1;
				int cur = this.FBase.TimeLineYear;
				if (min > max)
				{
					int x = min;
					min = max;
					max = x;
				}
				if (cur < min) cur = min;
				if (cur > max) cur = max;

				this.tbTimeLine.ValueChanged -= new EventHandler(this.tbTimeLine_ValueChanged);
				this.tbTimeLine.Maximum = max;
				this.tbTimeLine.Minimum = min;
				this.tbTimeLine.Value = cur;
				this.tbTimeLine.ValueChanged += new EventHandler(this.tbTimeLine_ValueChanged);

				this.StatusUpdate();
			}
		}

		public void CheckTimeLine(TfmBase aBase)
		{
			this.FBase = aBase;
			if (this.FBase != null)
			{
				this.UpdateControls();
				this.FBase.TimeLine_Init();
			}
		}
	}
}
