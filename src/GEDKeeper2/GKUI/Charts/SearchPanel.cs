using System;
using System.Windows.Forms;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;

namespace GKUI.Charts
{
	public sealed class SearchPanel : Form, ILocalization
	{
		private IWorkWindow fWorkWindow;
		private ISearchStrategy fStrategy;

		private TextBox txtSearchPattern;
		private Button btnPrev;
		private Button btnNext;

		public SearchPanel(IWorkWindow workWindow)
		{
			this.InitializeComponent();
			this.fWorkWindow = workWindow;
			this.SetLang();
		}
		
		private void InitializeComponent()
		{
			this.txtSearchPattern = new TextBox();
			this.btnPrev = new Button();
			this.btnNext = new Button();
			this.SuspendLayout();

			this.txtSearchPattern.Location = new System.Drawing.Point(3, 3);
			this.txtSearchPattern.Width = 150;
			this.txtSearchPattern.Height = 24;
			this.txtSearchPattern.Margin = new Padding(3, 3, 3, 0);
			this.txtSearchPattern.TextChanged += this.SearchPattern_TextChanged;
			
			this.btnPrev.Location = new System.Drawing.Point(156, 3);
			this.btnPrev.Margin = new Padding(3);
			this.btnPrev.Height = 24;
			this.btnPrev.Width = 24;
			this.btnPrev.Click += FindPrev_Click;
			this.btnPrev.Image = GKResources.iLeft1;
			
			this.btnNext.Location = new System.Drawing.Point(156+27, 3);
			this.btnNext.Margin = new Padding(3);
			this.btnNext.Height = 24;
			this.btnNext.Width = 24;
			this.btnNext.Click += FindNext_Click;
			this.btnNext.Image = GKResources.iRight1;
			
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.ClientSize = new System.Drawing.Size(210, 30);
			this.Controls.Add(this.txtSearchPattern);
			this.Controls.Add(this.btnPrev);
			this.Controls.Add(this.btnNext);
			this.Font = new System.Drawing.Font("Tahoma", 8.25f, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, 204);
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
			this.KeyPreview = true;
			this.KeyDown += this.SearchPanel_KeyDown;
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
			this.Text = "TreeSearch";
			this.TopMost = true;

			this.ResumeLayout(false);
		}

		private void SearchPattern_TextChanged(object sender, EventArgs e)
		{
			this.fStrategy = new TreeSearchStrategy(this.fWorkWindow, this.txtSearchPattern.Text);
		}
		
		private void FindNext_Click(object sender, EventArgs e)
		{
			if (this.fStrategy == null) return;
			
			if (!this.fStrategy.HasResults()) {
				GKUtils.ShowError(LangMan.LS(LSID.LSID_NoMatchesFound));
				return;
			}
			
			ISearchResult result = this.fStrategy.FindNext();
			if (result != null) {
				SelectResult(result as SearchResult);
			}
		}

		private void FindPrev_Click(object sender, EventArgs e)
		{
			if (this.fStrategy == null) return;
			
			if (!this.fStrategy.HasResults()) {
				GKUtils.ShowError(LangMan.LS(LSID.LSID_NoMatchesFound));
				return;
			}
			
			ISearchResult result = this.fStrategy.FindPrev();
			if (result != null) {
				SelectResult(result as SearchResult);
			}
		}
		
		private void SelectResult(SearchResult result)
		{
			if (result == null || result.Result == null) return;
			
			this.fWorkWindow.SelectByRec(result.Result as GEDCOMIndividualRecord);
		}
		
		private void SearchPanel_KeyDown(object sender, KeyEventArgs e)
		{
			switch (e.KeyCode) {
				case Keys.Enter:
					e.Handled = true;
					if (e.Shift)
						FindPrev_Click(this, null);
					else
						FindNext_Click(this, null);
					break;

				case Keys.Escape:
					e.Handled = true;
					Close();
					break;
			}
		}

		public void SetLang()
		{
			//this.txtSearchPattern.Text = LangMan.LS(LSID.LSID_NoMatchesFound);
			//this.btnPrev.Text = LangMan.LS(LSID.LSID_FindPrevious);
			//this.btnNext.Text = LangMan.LS(LSID.LSID_FindNext);
		}
	}
}
