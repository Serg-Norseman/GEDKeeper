using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

using ExtUtils;
using ExtUtils.Controls;
using GedCom551;
using GKCore.Interfaces;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKTextSearchPlugin
{
    public sealed partial class TfmTextSearch : Form, ILocalization
	{
    	private readonly Plugin fPlugin;
		private readonly IBase fBase;
		private readonly HyperView fResultsText;

		public TfmTextSearch(Plugin plugin, IBase aBase)
		{
			InitializeComponent();

			this.fPlugin = plugin;
            this.fBase = aBase;
            this.Text = string.Format(fPlugin.LangMan.LS(TLS.LSID_PluginTitle) + " [{0}]", Path.GetFileName(fBase.Tree.FileName));

			this.SuspendLayout();
			this.fResultsText = new HyperView();
			this.fResultsText.Dock = DockStyle.Fill;
			this.fResultsText.Location = new Point(0, 0);
			this.fResultsText.Size = new Size(300, 200);
			this.fResultsText.OnLink += mTextLink;
			this.Controls.Add(this.fResultsText);
			this.ResumeLayout(false);
			this.Controls.SetChildIndex(this.fResultsText, 0);
			
			(this as ILocalization).SetLang();
		}

		private void Write(string text)
		{
			fResultsText.Lines.Add(text);
		}

		private void mTextLink(object sender, string LinkName)
		{
			fBase.SelectRecordByXRef(LinkName);
		}

		void btnSearch_Click(object sender, EventArgs e)
		{
			this.btnSearch.Enabled = false;
			fResultsText.Lines.BeginUpdate();
			try
			{
				fResultsText.Lines.Clear();
				List<SearchManager.SearchEntry> search_results = fPlugin.SearchMan.Search(fBase, textBox2.Text);

				Write(string.Format(fPlugin.LangMan.LS(TLS.LSID_SearchResults) + "\r\n", search_results.Count));

				for (int i = 0; i <= search_results.Count - 1; i++)
				{
					Write("__________________________________________________________________________________________");
					Write("");

					SearchManager.SearchEntry entry = search_results[i];
					Write(String.Format("~bu+1~{0}: {1}%~u~ ~^{2}:[{2}]~", entry.Rank, entry.Percent, entry.XRef) + "~b-1~");

					TGEDCOMRecord rec = fBase.Tree.XRefIndex_Find(entry.XRef);
					StringList ctx = fBase.GetRecordContent(rec);
					fResultsText.Lines.AddStrings(ctx);
					Write("");
				}
			}
			finally
			{
				fResultsText.Lines.EndUpdate();
				this.btnSearch.Enabled = true;
			}
		}

		void TfmTextSearchLoad(object sender, EventArgs e)
		{
			fPlugin.SearchMan.ReindexBase(fBase);
		}

		void ILocalization.SetLang()
		{
		}
	}
}
