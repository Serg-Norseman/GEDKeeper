using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GKCore;
using GKCore.Sys;

namespace GKUI
{
	public partial class TfmTipsDialog : Form
	{
		private TStringList FTips;

		private void GetNextTip()
		{
			if (this.FTips.Count > 0)
			{
				this.TipWindow.Text = this.FTips[0];
				this.FTips.Delete(0);
			}
			this.NextTipBtn.Enabled = (this.FTips.Count > 0);
		}

		private void NextTipBtn_Click(object sender, EventArgs e)
		{
			this.GetNextTip();
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.FTips.Free();
			}
			base.Dispose(Disposing);
		}

		public TfmTipsDialog()
		{
			this.InitializeComponent();
			this.FTips = new TStringList();
			this.btnClose.Text = GKL.LSList[99];
			this.ShowCheck.Text = GKL.LSList[263];
			this.NextTipBtn.Text = GKL.LSList[384];
			this.TitleLabel.Text = GKL.LSList[385];
		}

		public static bool ShowTipsEx([In] string ACaption, bool ShowTipsChecked, TStrings Tips)
		{
			TfmTipsDialog dlg = new TfmTipsDialog();
			bool result = false;
			try
			{
				dlg.ShowCheck.Checked = ShowTipsChecked;
				dlg.Text = ACaption;
				dlg.TitleLabel.Text = ACaption;
				dlg.FTips.Assign(Tips);
				dlg.GetNextTip();
				dlg.ShowDialog();
				result = dlg.ShowCheck.Checked;
			}
			finally
			{
				dlg.Dispose();
			}
			return result;
		}
	}
}
