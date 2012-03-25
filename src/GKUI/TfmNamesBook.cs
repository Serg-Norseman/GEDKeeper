using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
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
	public partial class TfmNamesBook : Form
	{
		private class TNameRecord
		{
			public string Name;
			public string Desc;
			public TGEDCOMSex Sex;
			public int ChIndex;

			public void Free()
			{
				SysUtils.Free(this);
			}
		}

		private List<TNameRecord> FNames;
		private StringList FChurchFNames;
		private StringList FChurchMNames;

		private void TfmNamesBook_Closed(object sender, EventArgs e)
		{
			GKUI.TfmGEDKeeper.Instance.miNamesBook.Checked = false;
			GKUI.TfmGEDKeeper.Instance.fmNamesBook = null;
		}

		private void cbNames_SelectedIndexChanged(object sender, EventArgs e)
		{
			int idx = this.cbNames.SelectedIndex;
			if (idx >= 0 && idx < this.cbNames.Items.Count)
			{
				TNameRecord rec = (this.cbNames.Items[idx] as GKComboItem).Data as TNameRecord;

				this.mmDesc.Text = "";
				this.mmDesc.AppendText(rec.Name + "\r\n");
				this.mmDesc.AppendText(rec.Desc + "\r\n");
				if (rec.ChIndex >= 0)
				{
					this.mmDesc.AppendText("\r\n");
					this.mmDesc.AppendText("Святцы:\r\n");
					StringList lst = null;
					if (rec.Sex == TGEDCOMSex.svMale)
					{
						lst = this.FChurchMNames;
					}
					else
					{
						if (rec.Sex == TGEDCOMSex.svFemale)
						{
							lst = this.FChurchFNames;
						}
					}

					int num = lst.Count - 1;
					for (int i = rec.ChIndex + 1; i <= num; i++)
					{
						string st = lst[i].Trim();
						if (st[0] == '-')
						{
							break;
						}
						st = st.Remove(0, 1);
						this.mmDesc.AppendText(st + "\r\n");
					}
				}
			}
		}

		private bool ExtractFlags(ref string st)
		{
			bool res = (st != null);
			if (res) {
				res = (st.Length >= 2 && st[0] == '[' && st[st.Length - 1] == ']');
				if (res) {
					st = st.Substring(1, st.Length - 2);
				}
			}
			return res;
		}

		private void PrepareList()
		{
			StreamReader strd;
			strd = new StreamReader(new MemoryStream(global::GKResources.book_names), Encoding.GetEncoding(1251));
			try
			{
				while (strd.Peek() != -1)
				{
					string ns = strd.ReadLine().Trim();
					if (ns != "")
					{
						string[] toks = ns.Split('/');
						if (toks.Length >= 3)
						{
							TNameRecord rec = new TNameRecord();

							rec.Name = toks[0].Trim();
							rec.Desc = toks[2].Trim();
							string st = toks[1].Trim();

							if (ExtractFlags(ref st))
							{
								char c = st[0];
								switch (c) {
									case 'f':
										rec.Sex = TGEDCOMSex.svFemale;
										break;
									case 'm':
										rec.Sex = TGEDCOMSex.svMale;
										break;
								}
							}

							this.FNames.Add(rec);
						}
					}
				}
			}
			finally
			{
				strd.Dispose();
			}

			strd = new StreamReader(new MemoryStream(global::GKResources.book_names_cf), Encoding.GetEncoding(1251));
			try
			{
				while (strd.Peek() != -1) {
					string ns = strd.ReadLine().Trim();
					this.FChurchFNames.Add(ns);
				}
			}
			finally
			{
				strd.Dispose();
			}

			strd = new StreamReader(new MemoryStream(global::GKResources.book_names_cm), Encoding.GetEncoding(1251));
			try
			{
				while (strd.Peek() != -1) {
					string ns = strd.ReadLine().Trim();
					this.FChurchMNames.Add(ns);
				}
			}
			finally
			{
				strd.Dispose();
			}

			this.cbNames.BeginUpdate();
			try
			{
				this.cbNames.Items.Clear();
				int num = this.FNames.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TNameRecord rec = this.FNames[i];
					string ns = rec.Name;
					this.cbNames.Items.Add(new GKComboItem(ns, rec));

					rec.ChIndex = -1;
					ns = ns.ToUpper();

					StringList lst = null;
					if (rec.Sex == TGEDCOMSex.svMale)
					{
						lst = this.FChurchMNames;
					}
					else if (rec.Sex == TGEDCOMSex.svFemale)
					{
						lst = this.FChurchFNames;
					}

					int num2 = lst.Count - 1;
					for (int j = 0; j <= num2; j++)
					{
						string st = lst[j];
						if (st[0] == '-' && st.IndexOf(ns) >= 0)
						{
							rec.ChIndex = j;
							break;
						}
					}
				}
			}
			finally
			{
				this.cbNames.EndUpdate();
			}
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.FChurchFNames.Free();
				this.FChurchMNames.Free();
				//this.FNames.Dispose();
			}
			base.Dispose(Disposing);
		}

		public TfmNamesBook()
		{
			this.InitializeComponent();
			this.FNames = new List<TNameRecord>();
			this.FChurchFNames = new StringList();
			this.FChurchMNames = new StringList();
			this.PrepareList();
			this.Text = LangMan.LSList[31];
		}
	}
}
