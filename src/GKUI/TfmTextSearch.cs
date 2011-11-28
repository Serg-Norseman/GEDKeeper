using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Controls;

namespace GKUI
{
	public partial class TfmTextSearch : Form
	{
		private TfmBase FBase;
		private TGKHyperView FResultsText;
		private SearchManager FSearchManager;

		public TfmBase Base
		{
			get { return this.FBase; }
			set {
				this.FBase = value;
				this.Text = "Полнотекстовый поиск по базе " + Path.GetFileName(FBase.FileName);
			}
		}

		public TfmTextSearch(TfmBase aBase)
		{
			InitializeComponent();
			this.Base = aBase;

			this.SuspendLayout();
			this.FResultsText = new TGKHyperView();
			this.FResultsText.Dock = DockStyle.Fill;
			this.FResultsText.Location = new Point(0, 0);
			this.FResultsText.Size = new Size(300, 200);
			this.FResultsText.OnLink += mTextLink;
			this.Controls.Add(this.FResultsText);
			this.ResumeLayout(false);
			this.Controls.SetChildIndex(this.FResultsText, 0);

			FSearchManager = new SearchManager(FBase);
		}

		private void Write(string text)
		{
			FResultsText.Lines.Add(text);
		}

		private void mTextLink(object sender, string LinkName)
		{
			FBase.SelectRecordByXRef(LinkName);
		}

		void btnReindex_Click(object sender, EventArgs e)
		{
			FSearchManager.ReindexBase();
		}

		void btnSearch_Click(object sender, EventArgs e)
		{
			FResultsText.Lines.BeginUpdate();
			try
			{
				FResultsText.Lines.Clear();
				List<SearchManager.SearchEntry> search_results = FSearchManager.Search(textBox2.Text);

				Write(string.Format("Найдено: {0} результат(ов).\r\n", search_results.Count));

				for (int i = 0; i <= search_results.Count - 1; i++)
				{
					Write("__________________________________________________________________________________________");
					Write("");

					SearchManager.SearchEntry entry = search_results[i];
					Write(String.Format("~bu+1~{0}: {1}%~u~ ~^{2}:[{2}]~", entry.Rank, entry.Percent, entry.XRef) + "~b-1~");

					TGEDCOMRecord rec = FBase.Tree.XRefIndex_Find(entry.XRef);
					TStrings ctx = FBase.GetRecordContext(rec);
					FResultsText.Lines.AddStrings(ctx);
					Write("");
				}
			}
			finally
			{
				FResultsText.Lines.EndUpdate();
			}
		}

		void TfmTextSearchLoad(object sender, EventArgs e)
		{
			if (!FSearchManager.IsIndexed()) {
				FSearchManager.ReindexBase();
			}
		}
	}
}
