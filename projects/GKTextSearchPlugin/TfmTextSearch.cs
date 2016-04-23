using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

using GKCommon;
using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCore.Interfaces;

namespace GKTextSearchPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TfmTextSearch : Form, ILocalization
	{
    	private readonly Plugin fPlugin;
		private readonly IBaseWindow fBase;
		private readonly HyperView fResultsText;

		public TfmTextSearch(Plugin plugin, IBaseWindow aBase)
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

		private void mTextLink(object sender, string linkName)
		{
		    if (string.IsNullOrEmpty(linkName)) return;

			fBase.SelectRecordByXRef(linkName);
		}

		void btnSearch_Click(object sender, EventArgs e)
		{
			this.btnSearch.Enabled = false;
			fResultsText.Lines.BeginUpdate();
			try
			{
				fResultsText.Lines.Clear();
				List<SearchManager.SearchEntry> searchResults = fPlugin.SearchMan.Search(fBase, textBox2.Text);

				Write(string.Format(fPlugin.LangMan.LS(TLS.LSID_SearchResults) + "\r\n", searchResults.Count));

				int num = searchResults.Count;
				for (int i = 0; i < num; i++)
				{
					Write("__________________________________________________________________________________________");
					Write("");

					SearchManager.SearchEntry entry = searchResults[i];
					Write(String.Format("~bu+1~{0}: {1}%~u~ ~^{2}:[{2}]~", entry.Rank, entry.Percent, entry.XRef) + "~b-1~");

					GEDCOMRecord rec = fBase.Tree.XRefIndex_Find(entry.XRef);
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
