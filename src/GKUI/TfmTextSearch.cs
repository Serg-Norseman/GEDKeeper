using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKCore;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI
{
	public partial class TfmTextSearch : Form, ILocalization
	{
		private TfmBase FBase;
		private GKHyperView FResultsText;

		public TfmBase Base
		{
			get { return this.FBase; }
			set {
				this.FBase = value;
				this.Text = string.Format(LangMan.LS(LSID.LSID_FullTextSearch)+" [{0}]", Path.GetFileName(FBase.Tree.FileName));
			}
		}

		public TfmTextSearch(TfmBase aBase)
		{
			InitializeComponent();
			this.Base = aBase;

			this.SuspendLayout();
			this.FResultsText = new GKHyperView();
			this.FResultsText.Dock = DockStyle.Fill;
			this.FResultsText.Location = new Point(0, 0);
			this.FResultsText.Size = new Size(300, 200);
			this.FResultsText.OnLink += mTextLink;
			this.Controls.Add(this.FResultsText);
			this.ResumeLayout(false);
			this.Controls.SetChildIndex(this.FResultsText, 0);
			
			(this as ILocalization).SetLang();
		}

		private void Write(string text)
		{
			FResultsText.Lines.Add(text);
		}

		private void mTextLink(object sender, string LinkName)
		{
			FBase.SelectRecordByXRef(LinkName);
		}

		void btnSearch_Click(object sender, EventArgs e)
		{
			this.btnSearch.Enabled = false;
			FResultsText.Lines.BeginUpdate();
			try
			{
				FResultsText.Lines.Clear();
				List<SearchManager.SearchEntry> search_results = TfmGEDKeeper.Instance.SearchMan.Search(FBase, textBox2.Text);

				Write(string.Format(LangMan.LS(LSID.LSID_SearchResults), search_results.Count));

				for (int i = 0; i <= search_results.Count - 1; i++)
				{
					Write("__________________________________________________________________________________________");
					Write("");

					SearchManager.SearchEntry entry = search_results[i];
					Write(String.Format("~bu+1~{0}: {1}%~u~ ~^{2}:[{2}]~", entry.Rank, entry.Percent, entry.XRef) + "~b-1~");

					TGEDCOMRecord rec = FBase.Tree.XRefIndex_Find(entry.XRef);
					StringList ctx = FBase.GetRecordContext(rec);
					FResultsText.Lines.AddStrings(ctx);
					Write("");
				}
			}
			finally
			{
				FResultsText.Lines.EndUpdate();
				this.btnSearch.Enabled = true;
			}
		}

		void TfmTextSearchLoad(object sender, EventArgs e)
		{
			TfmGEDKeeper.Instance.SearchMan.ReindexBase(FBase);
		}

		void ILocalization.SetLang()
		{
		}
	}
}
