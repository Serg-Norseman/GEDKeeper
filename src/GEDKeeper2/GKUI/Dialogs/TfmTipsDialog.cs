using System;
using System.Windows.Forms;

using ExtUtils;
using GKCommon;
using GKCore;

/// <summary>
/// 
/// </summary>

namespace GKUI.Dialogs
{
	public partial class TfmTipsDialog : Form
	{
		private readonly StringList fTips;

        public TfmTipsDialog()
        {
            this.InitializeComponent();

            this.fTips = new StringList();

            this.btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            this.ShowCheck.Text = LangMan.LS(LSID.LSID_StartupTips);
            this.NextTipBtn.Text = LangMan.LS(LSID.LSID_Next);
            this.TitleLabel.Text = LangMan.LS(LSID.LSID_YouKnowWhat);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fTips.Dispose();
            }
            base.Dispose(disposing);
        }

        private void GetNextTip()
		{
			if (this.fTips.Count > 0)
			{
				this.TipWindow.Text = this.fTips[0];
				this.fTips.Delete(0);
			}
			this.NextTipBtn.Enabled = (this.fTips.Count > 0);
		}

		private void NextTipBtn_Click(object sender, EventArgs e)
		{
			this.GetNextTip();
		}

		public static bool ShowTipsEx(string caption, bool showTipsChecked, StringList tips)
		{
            bool result;
            using (TfmTipsDialog dlg = new TfmTipsDialog())
			{
				dlg.ShowCheck.Checked = showTipsChecked;
				dlg.Text = caption;
				dlg.TitleLabel.Text = caption;
				dlg.fTips.Assign(tips);
				dlg.GetNextTip();
				dlg.ShowDialog();

                result = dlg.ShowCheck.Checked;
			}
			return result;
		}
	}
}
