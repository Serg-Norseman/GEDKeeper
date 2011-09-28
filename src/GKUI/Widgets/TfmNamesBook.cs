using System;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Controls;

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
				TObjectHelper.Free(this);
			}
		}

		private TObjectList FNames;
		private TStringList FChurchFNames;
		private TStringList FChurchMNames;

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
				TfmNamesBook.TNameRecord rec = (this.cbNames.Items[idx] as TComboItem).Data as TfmNamesBook.TNameRecord;
				this.mmDesc.Text = "";
				this.mmDesc.AppendText(rec.Name + "\r\n");
				this.mmDesc.AppendText(rec.Desc + "\r\n");
				if (rec.ChIndex >= 0)
				{
					this.mmDesc.AppendText("\r\n");
					this.mmDesc.AppendText("Святцы:\r\n");
					TStringList lst = null;
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

					int arg_F2_0 = rec.ChIndex + 1;
					int num = lst.Count - 1;
					int i = arg_F2_0;
					if (num >= i)
					{
						num++;
						do
						{
							string st = lst[i].Trim();
							if (st[0] == '-')
							{
								break;
							}
							st = st.Remove(0, 1);
							this.mmDesc.AppendText(st + "\r\n");
							i++;
						}
						while (i != num);
					}
				}
			}
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
					if (ns != "" && SysUtils.GetTokensCount(ns, '/') >= 3)
					{
						TfmNamesBook.TNameRecord rec = new TfmNamesBook.TNameRecord();
						rec.Name = SysUtils.GetToken(ns, '/', 1).Trim();
						rec.Desc = SysUtils.GetToken(ns, '/', 3).Trim();
						string st = SysUtils.GetToken(ns, '/', 2).Trim();
						if (TfmNamesBook._PrepareList_ExtractFlags(ref st))
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
				int i = 0;
				if (num >= i)
				{
					num++;
					do
					{
						TfmNamesBook.TNameRecord rec = this.FNames[i] as TfmNamesBook.TNameRecord;
						string ns = rec.Name;
						this.cbNames.Items.Add(new TComboItem(ns, rec));
						rec.ChIndex = -1;
						ns = ns.ToUpper();
						TStringList lst = null;
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
						int num2 = lst.Count - 1;
						int j = 0;
						if (num2 >= j)
						{
							num2++;
							while (true)
							{
								string st = lst[j];
								if (st[0] == '-' && SysUtils.Pos(ns, st) > 0)
								{
									break;
								}
								j++;
								if (j == num2)
								{
									goto IL_248;
								}
							}
							rec.ChIndex = j;
						}
						IL_248:
						i++;
					}
					while (i != num);
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
				this.FNames.Free();
			}
			base.Dispose(Disposing);
		}

		public TfmNamesBook()
		{
			this.InitializeComponent();
			this.FNames = new TObjectList(true);
			this.FChurchFNames = new TStringList();
			this.FChurchMNames = new TStringList();
			this.PrepareList();
			this.Text = GKL.LSList[31];
		}

		private static bool _PrepareList_ExtractFlags(ref string st)
		{
			string text = st;
			bool arg_3E_0;
			if (((text != null) ? text.Length : 0) >= 2 && st[0] == '[')
			{
				string arg_31_0 = st;
				string text2 = st;
				if (arg_31_0[((text2 != null) ? text2.Length : 0) - 1] == ']')
				{
					arg_3E_0 = true;
					goto IL_3E;
				}
			}
			arg_3E_0 = false;
			IL_3E:
			bool Result = arg_3E_0;
			if (Result)
			{
				string arg_57_0 = st;
				int arg_57_1 = 2;
				string text3 = st;
				st = SysUtils.WStrCopy(arg_57_0, arg_57_1, ((text3 != null) ? text3.Length : 0) - 2);
			}
			return Result;
		}
	}
}
