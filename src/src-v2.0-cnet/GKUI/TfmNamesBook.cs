using GedCom551;
using GKCore;
using GKUI.Controls;
using GKSys;
using System;
using System.Drawing;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmNamesBook : Form
	{
		private class TNameRecord
		{
			public string Name;
			public string Desc;
			public TGEDCOMObject.TGEDCOMSex Sex;
			public int ChIndex;

			public void Free()
			{
				TObjectHelper.Free(this);
			}
		}

		private ComboBox cbNames;
		private TextBox mmDesc;
		private TObjectList FNames;
		private TStringList FChurchFNames;
		private TStringList FChurchMNames;

		private void InitializeComponent()
		{
			this.cbNames = new ComboBox();
			this.mmDesc = new TextBox();
			base.SuspendLayout();
			this.cbNames.DropDownStyle = ComboBoxStyle.Simple;
			this.cbNames.Location = new Point(8, 8);
			this.cbNames.Name = "cbNames";
			this.cbNames.Size = new Size(257, 168);
			this.cbNames.Sorted = true;
			this.cbNames.TabIndex = 0;
			this.cbNames.SelectedIndexChanged += new EventHandler(this.cbNames_SelectedIndexChanged);
			this.mmDesc.BackColor = SystemColors.Control;
			this.mmDesc.Location = new Point(8, 184);
			this.mmDesc.Multiline = true;
			this.mmDesc.Name = "mmDesc";
			this.mmDesc.ReadOnly = true;
			this.mmDesc.ScrollBars = ScrollBars.Vertical;
			this.mmDesc.Size = new Size(257, 161);
			this.mmDesc.TabIndex = 1;
			this.mmDesc.Text = "";
			this.AutoScaleBaseSize = new Size(5, 14);
			base.ClientSize = new Size(274, 353);
			base.Controls.Add(this.cbNames);
			base.Controls.Add(this.mmDesc);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedToolWindow;
			base.Name = "TfmNamesBook";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.Manual;
			this.Text = "TfmNamesBook";
			base.TopMost = true;
			base.Closed += new EventHandler(this.TfmNamesBook_Closed);
			base.ResumeLayout(false);
		}
		private void TfmNamesBook_Closed(object sender, EventArgs e)
		{
			GKL.fmGEDKeeper.miNamesBook.Checked = false;
			GKL.fmGEDKeeper.fmNamesBook = null;
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
					if (rec.Sex == TGEDCOMObject.TGEDCOMSex.svMale)
					{
						lst = this.FChurchMNames;
					}
					else
					{
						if (rec.Sex == TGEDCOMObject.TGEDCOMSex.svFemale)
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
			TResourceStream fs = new TResourceStream(VCLUtils.HInstance(), "NAMES_DATA", 10);
			StreamReader strd = new StreamReader(TStreamToCLRStream.GetStream(fs), Encoding.GetEncoding(1251));
			try
			{
				while (strd.Peek() != -1)
				{
					string ns = strd.ReadLine().Trim();
					if (BDSSystem.WStrCmp(ns, "") != 0 && TGKSys.GetTokensCount(ns, '/') >= 3)
					{
						TfmNamesBook.TNameRecord rec = new TfmNamesBook.TNameRecord();
						rec.Name = TGKSys.GetToken(ns, '/', 1).Trim();
						rec.Desc = TGKSys.GetToken(ns, '/', 3).Trim();
						string st = TGKSys.GetToken(ns, '/', 2).Trim();
						if (TfmNamesBook._PrepareList_ExtractFlags(ref st))
						{
							char c = st[0];
							switch (c) {
								case 'f':
									rec.Sex = TGEDCOMObject.TGEDCOMSex.svFemale;
									break;
								case 'm':
									rec.Sex = TGEDCOMObject.TGEDCOMSex.svMale;
									break;
							}
						}
						this.FNames.Add(rec);
					}
				}
			}
			finally
			{
				TObjectHelper.Free(strd);
				fs.Free();
			}

			fs = new TResourceStream(VCLUtils.HInstance(), "CHURCH_FNAMES_DATA", 10);
			try
			{
				this.FChurchFNames.LoadFromStream(fs);
			}
			finally
			{
				fs.Free();
			}

			fs = new TResourceStream(VCLUtils.HInstance(), "CHURCH_MNAMES_DATA", 10);
			try
			{
				this.FChurchMNames.LoadFromStream(fs);
			}
			finally
			{
				fs.Free();
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
						if (rec.Sex == TGEDCOMObject.TGEDCOMSex.svMale)
						{
							lst = this.FChurchMNames;
						}
						else
						{
							if (rec.Sex == TGEDCOMObject.TGEDCOMSex.svFemale)
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
								if (st[0] == '-' && BDSSystem.Pos(ns, st) > 0)
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
				st = BDSSystem.WStrCopy(arg_57_0, arg_57_1, ((text3 != null) ? text3.Length : 0) - 2);
			}
			return Result;
		}
	}
}
