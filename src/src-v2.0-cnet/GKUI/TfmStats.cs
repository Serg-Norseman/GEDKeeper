using GedCom551;
using GKCore;
using GKUI.Controls;
using GKSys;
using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using ZedGraph;

namespace GKUI
{
	public class TfmStats : Form
	{
		[StructLayout(LayoutKind.Auto)]
		private struct TTitleRec
		{
			public LSID Title;
			public LSID Cap;
		}

		private enum TChartStyle : byte
		{
			csBar,
			csPoint
		}

		private static readonly TfmStats.TTitleRec[] Titles;
		private GroupBox GroupBox1;
		private Panel Panel1;
		private Panel ToolBar1;
		private ComboBox cbType;
		private ListView ListCommon;
		private ColumnHeader ColumnHeader1;
		private ColumnHeader ColumnHeader2;
		private ColumnHeader ColumnHeader3;
		private ColumnHeader ColumnHeader4;
		private TGKListView ListStats;
		private ZedGraphControl zgc;
		private TfmBase FBase;
		private string ChartTitle;
		private string ChartXTitle;
		private string ChartYTitle;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		private void PrepareArray(GraphPane gPane, TfmStats.TChartStyle aStyle, bool aExcludeUnknowns)
		{
			gPane.Title.Text = "ChartTitle";
			gPane.XAxis.Title.Text = "ChartXTitle";
			gPane.YAxis.Title.Text = "ChartYTitle";
			PointPairList ppList = new PointPairList();

			for (int i = 0; i <= this.ListStats.Items.Count - 1; i++)
			{
				string s = this.ListStats.Items[i].Text;
				int lab;
				if (s == "?")
				{
					lab = 0;
				}
				else
				{
					lab = int.Parse(s);
				}
				if (lab != 0 || !aExcludeUnknowns)
				{
					int val = int.Parse(this.ListStats.Items[i].SubItems[1].Text);
					ppList.Add((double)lab, (double)val);
				}
			}
			ppList.Sort();
			
			switch (aStyle) {
				case TChartStyle.csBar:
					gPane.AddBar("-", ppList, Color.Green);
					break;
				case TChartStyle.csPoint:
					gPane.AddCurve("-", ppList, Color.Green, SymbolType.Diamond);
					break;
			}
		}

		private void CalcStats(TGEDCOMTree aTree, TGenEngine.TStatMode aMode)
		{
			this.ListStats.Columns[0].Text = GKL.LSList[(int)TfmStats.Titles[(int)aMode].Cap - 1];
			this.ListStats.Columns[1].Text = GKL.LSList[202];
			this.ListStats.BeginUpdate();
			this.ListStats.Items.Clear();
			Hashtable vals = new Hashtable();
			try
			{
				this.Base.Engine.GetSpecStats(aMode, vals);
				IEnumerator Enumerator = vals.GetEnumerator();
				while (Enumerator.MoveNext())
				{
					DictionaryEntry Entry = (DictionaryEntry)Enumerator.Current;
					ListViewItem item = this.ListStats.Items.Add(Entry.Key as string);
					item.SubItems.Add(Entry.Value.ToString());
				}
			}
			finally
			{
				TObjectHelper.Free(vals);
				this.ListStats.EndUpdate();
			}

			/*GraphPane gPane = this.zgc.GraphPane;
			try
			{
				gPane.CurveList.Clear();
				this.ChartTitle = GKL.LSList[(int)TfmStats.Titles[(int)aMode].Title - 1];
				if (aMode != TGenEngine.TStatMode.smAge)
				{
					if (aMode != TGenEngine.TStatMode.smLifeExpectancy)
					{
						if ((byte)aMode - (byte)TGenEngine.TStatMode.smBirthYears >= (byte)TGenEngine.TStatMode.smNames)
						{
							if (aMode == TGenEngine.TStatMode.smChildsDistribution)
							{
								this.ChartXTitle = GKL.LSList[118];
								this.ChartYTitle = GKL.LSList[152];
								this.PrepareArray(gPane, TfmStats.TChartStyle.csBar, true);
							}
						}
						else
						{
							if (aMode != TGenEngine.TStatMode.smBirthYears)
							{
								if (aMode != TGenEngine.TStatMode.smBirthTenYears)
								{
									if (aMode == TGenEngine.TStatMode.smDeathYears)
									{
										goto IL_1B9;
									}
									if (aMode != TGenEngine.TStatMode.smDeathTenYears)
									{
										goto IL_1DD;
									}
								}
								this.ChartXTitle = GKL.LSList[535];
								goto IL_1DD;
							}
							IL_1B9:
							this.ChartXTitle = GKL.LSList[534];
							IL_1DD:
							if ((byte)aMode - (byte)TGenEngine.TStatMode.smBirthYears >= (byte)TGenEngine.TStatMode.smDescGenerations)
							{
								if ((byte)aMode - (byte)TGenEngine.TStatMode.smDeathYears < (byte)TGenEngine.TStatMode.smDescGenerations)
								{
									this.ChartYTitle = GKL.LSList[537];
								}
							}
							else
							{
								this.ChartYTitle = GKL.LSList[536];
							}
							this.PrepareArray(gPane, TfmStats.TChartStyle.csPoint, true);
						}
					}
					else
					{
						this.ChartXTitle = GKL.LSList[306];
						this.ChartYTitle = GKL.LSList[533];
						this.PrepareArray(gPane, TfmStats.TChartStyle.csPoint, true);
					}
				}
				else
				{
					this.ChartXTitle = GKL.LSList[305];
					this.ChartYTitle = GKL.LSList[533];
					this.PrepareArray(gPane, TfmStats.TChartStyle.csPoint, true);
				}
			}
			finally
			{
				this.zgc.AxisChange();
			}*/
		}

		private void InitializeComponent()
		{
			this.GroupBox1 = new GroupBox();
			this.ListCommon = new ListView();
			this.ColumnHeader1 = new ColumnHeader();
			this.ColumnHeader2 = new ColumnHeader();
			this.ColumnHeader3 = new ColumnHeader();
			this.ColumnHeader4 = new ColumnHeader();
			this.Panel1 = new Panel();
			this.ToolBar1 = new Panel();
			this.cbType = new ComboBox();
			this.GroupBox1.SuspendLayout();
			this.Panel1.SuspendLayout();
			this.ToolBar1.SuspendLayout();
			base.SuspendLayout();
			this.GroupBox1.Controls.Add(this.ListCommon);
			this.GroupBox1.Dock = DockStyle.Top;
			this.GroupBox1.Location = new Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new Size(754, 210);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			this.GroupBox1.Text = "Сводка";
			ListView.ColumnHeaderCollection arg_165_0 = this.ListCommon.Columns;
			ColumnHeader[] array = null;
			ColumnHeader[] array2 = array;
			ColumnHeader[] array3;
			ColumnHeader[] expr_125 = array3 = new ColumnHeader[4];
			if (array2 != null)
			{
				int num;
				if ((num = array2.Length) > 4)
				{
					num = 4;
				}
				if (num > 0)
				{
					Array.Copy(array2, array3, num);
				}
			}
			array = expr_125;
			array[0] = this.ColumnHeader1;
			array[1] = this.ColumnHeader2;
			array[2] = this.ColumnHeader3;
			array[3] = this.ColumnHeader4;
			arg_165_0.AddRange(array);
			this.ListCommon.Dock = DockStyle.Fill;
			this.ListCommon.FullRowSelect = true;
			this.ListCommon.Location = new Point(3, 17);
			this.ListCommon.Name = "ListCommon";
			this.ListCommon.Size = new Size(748, 172);
			this.ListCommon.TabIndex = 0;
			this.ListCommon.View = View.Details;
			this.ColumnHeader1.Text = "Параметр";
			this.ColumnHeader1.Width = 300;
			this.ColumnHeader2.Text = "Всего";
			this.ColumnHeader2.Width = 100;
			this.ColumnHeader3.Text = "Мужчины";
			this.ColumnHeader3.Width = 100;
			this.ColumnHeader4.Text = "Женщины";
			this.ColumnHeader4.Width = 100;
			this.Panel1.Controls.Add(this.ToolBar1);
			this.Panel1.Dock = DockStyle.Fill;
			this.Panel1.Location = new Point(0, 192);
			this.Panel1.Name = "Panel1";
			this.Panel1.Size = new Size(754, 311);
			this.Panel1.TabIndex = 2;
			this.ToolBar1.Controls.Add(this.cbType);
			this.ToolBar1.Dock = DockStyle.Top;
			this.ToolBar1.Location = new Point(0, 0);
			this.ToolBar1.Name = "ToolBar1";
			this.ToolBar1.Size = new Size(754, 21);
			this.ToolBar1.TabIndex = 0;
			this.cbType.DropDownStyle = ComboBoxStyle.DropDownList;
			this.cbType.Location = new Point(8, 0);
			this.cbType.Name = "cbType";
			this.cbType.Size = new Size(233, 21);
			this.cbType.TabIndex = 0;
			this.cbType.SelectedIndexChanged += new EventHandler(this.cbType_SelectedIndexChanged);
			this.AutoScaleBaseSize = new Size(5, 14);
			base.ClientSize = new Size(754, 503);
			base.Controls.Add(this.Panel1);
			base.Controls.Add(this.GroupBox1);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.KeyPreview = true;
			base.Name = "TfmStats";
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Статистика";
			base.KeyDown += new KeyEventHandler(this.TfmStats_KeyDown);
			base.Load += new EventHandler(this.TfmStats_Load);
			this.GroupBox1.ResumeLayout(false);
			this.Panel1.ResumeLayout(false);
			this.ToolBar1.ResumeLayout(false);
			base.ResumeLayout(false);
		}

		private void TfmStats_Load(object sender, EventArgs e)
		{
			TGenEngine.TCommonStats stats;
			this.Base.Engine.GetCommonStats(out stats);
			this.ListCommon.Items.Clear();
			ListViewItem item = this.ListCommon.Items.Add(GKL.LSList[533]);
			item.SubItems.Add(stats.persons.ToString());
			item.SubItems.Add(stats.persons_m.ToString() + TfmStats._TfmStats_Load_GetPercent(stats.persons_m, stats.persons));
			item.SubItems.Add(stats.persons_f.ToString() + TfmStats._TfmStats_Load_GetPercent(stats.persons_f, stats.persons));
			item = this.ListCommon.Items.Add(GKL.LSList[538]);
			item.SubItems.Add(stats.lives.ToString());
			item.SubItems.Add(stats.lives_m.ToString());
			item.SubItems.Add(stats.lives_f.ToString());
			item = this.ListCommon.Items.Add(GKL.LSList[539]);
			item.SubItems.Add((stats.persons - stats.lives).ToString());
			item.SubItems.Add((stats.persons_m - stats.lives_m).ToString());
			item.SubItems.Add((stats.persons_f - stats.lives_f).ToString());
			checked
			{
				item = this.ListCommon.Items.Add(GKL.LSList[540]);
				item.SubItems.Add(((long)Math.Round(TGKSys.SafeDiv((double)stats.age, (double)stats.age_cnt))).ToString());
				item.SubItems.Add(((long)Math.Round(TGKSys.SafeDiv((double)stats.age_m, (double)stats.age_m_cnt))).ToString());
				item.SubItems.Add(((long)Math.Round(TGKSys.SafeDiv((double)stats.age_f, (double)stats.age_f_cnt))).ToString());
				item = this.ListCommon.Items.Add(GKL.LSList[541]);
				item.SubItems.Add(((long)Math.Round(TGKSys.SafeDiv((double)stats.life, (double)stats.life_cnt))).ToString());
				item.SubItems.Add(((long)Math.Round(TGKSys.SafeDiv((double)stats.life_m, (double)stats.life_m_cnt))).ToString());
				item.SubItems.Add(((long)Math.Round(TGKSys.SafeDiv((double)stats.life_f, (double)stats.life_f_cnt))).ToString());
				item = this.ListCommon.Items.Add(GKL.LSList[542]);
				item.SubItems.Add(string.Format("{0:0.00}", new object[]
				{
					TGKSys.SafeDiv((double)stats.childs, (double)stats.childs_cnt)
				}));
				item.SubItems.Add(string.Format("{0:0.00}", new object[]
				{
					TGKSys.SafeDiv((double)stats.childs_m, (double)stats.childs_m_cnt)
				}));
				item.SubItems.Add(string.Format("{0:0.00}", new object[]
				{
					TGKSys.SafeDiv((double)stats.childs_f, (double)stats.childs_f_cnt)
				}));
				item = this.ListCommon.Items.Add(GKL.LSList[543]);
				item.SubItems.Add(string.Format("{0:0.00}", new object[]
				{
					TGKSys.SafeDiv((double)stats.fba, (double)stats.fba_cnt)
				}));
				item.SubItems.Add(string.Format("{0:0.00}", new object[]
				{
					TGKSys.SafeDiv((double)stats.fba_m, (double)stats.fba_m_cnt)
				}));
				item.SubItems.Add(string.Format("{0:0.00}", new object[]
				{
					TGKSys.SafeDiv((double)stats.fba_f, (double)stats.fba_f_cnt)
				}));
				item = this.ListCommon.Items.Add(GKL.LSList[544]);
				item.SubItems.Add(string.Format("{0:0.00}", new object[]
				{
					TGKSys.SafeDiv((double)stats.marr, (double)stats.marr_cnt)
				}));
				item.SubItems.Add(string.Format("{0:0.00}", new object[]
				{
					TGKSys.SafeDiv((double)stats.marr_m, (double)stats.marr_m_cnt)
				}));
				item.SubItems.Add(string.Format("{0:0.00}", new object[]
				{
					TGKSys.SafeDiv((double)stats.marr_f, (double)stats.marr_f_cnt)
				}));
				item = this.ListCommon.Items.Add(GKL.LSList[545]);
				item.SubItems.Add(string.Format("{0:0.00}", new object[]
				{
					TGKSys.SafeDiv((double)stats.mage, (double)stats.mage_cnt)
				}));
				item.SubItems.Add(string.Format("{0:0.00}", new object[]
				{
					TGKSys.SafeDiv((double)stats.mage_m, (double)stats.mage_m_cnt)
				}));
				item.SubItems.Add(string.Format("{0:0.00}", new object[]
				{
					TGKSys.SafeDiv((double)stats.mage_f, (double)stats.mage_f_cnt)
				}));
			}
		}

		private void TfmStats_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Escape) base.Close();
		}

		private void cbType_SelectedIndexChanged(object sender, EventArgs e)
		{
			this.CalcStats(this.Base.Tree, (TGenEngine.TStatMode)this.cbType.SelectedIndex);
		}

		public TfmStats(TfmBase aBase)
		{
			this.InitializeComponent();
			base.MdiParent = GKL.fmGEDKeeper;
			this.FBase = aBase;
			this.zgc = new ZedGraphControl();
			this.zgc.IsShowPointValues = true;
			this.zgc.Dock = DockStyle.Right;
			this.zgc.Size = new Size(400, 200);
			Splitter spl = new Splitter();
			spl.Dock = DockStyle.Right;
			spl.Size = new Size(4, 290);
			spl.MinExtra = 100;
			spl.MinSize = 100;
			this.Panel1.Controls.Add(this.zgc);
			this.Panel1.Controls.Add(spl);
			this.Base.CreateListView(this.Panel1, ref this.ListStats);
			this.ListStats.AddListColumn("-", 250, false);
			this.ListStats.AddListColumn("-", 150, false);
			this.Panel1.Controls.SetChildIndex(this.ListStats, 0);
			this.Panel1.Controls.SetChildIndex(spl, 2);
			this.Panel1.Controls.SetChildIndex(this.zgc, 3);
			this.Panel1.Controls.SetChildIndex(this.ToolBar1, 4);
			this.cbType.Items.Clear();

			for (TGenEngine.TStatMode i = TGenEngine.TStatMode.smAncestors; i <= TGenEngine.TStatMode.smMiliRank; i++)
			{
				this.cbType.Items.Add(GKL.LSList[(int)TfmStats.Titles[(int)i].Title - 1]);
			}

			this.SetLang();
		}

		public void SetLang()
		{
			this.Text = GKL.LSList[29];
		}

		static TfmStats()
		{
			TfmStats.TTitleRec[] array = new TfmStats.TTitleRec[32];
			TfmStats.TTitleRec[] arg_31_0_cp_0 = array;
			int arg_31_0_cp_1 = 0;
			TfmStats.TTitleRec tTitleRec;
			tTitleRec.Title = LSID.LSID_AncestorsCount;
			tTitleRec.Cap = LSID.LSID_Name;
			arg_31_0_cp_0[arg_31_0_cp_1] = tTitleRec;
			TfmStats.TTitleRec[] arg_54_0_cp_0 = array;
			int arg_54_0_cp_1 = 1;
			TfmStats.TTitleRec tTitleRec2;
			tTitleRec2.Title = LSID.LSID_DescendantsCount;
			tTitleRec2.Cap = LSID.LSID_Name;
			arg_54_0_cp_0[arg_54_0_cp_1] = tTitleRec2;
			TfmStats.TTitleRec[] arg_77_0_cp_0 = array;
			int arg_77_0_cp_1 = 2;
			TfmStats.TTitleRec tTitleRec3;
			tTitleRec3.Title = LSID.LSID_GenerationsCount;
			tTitleRec3.Cap = LSID.LSID_Name;
			arg_77_0_cp_0[arg_77_0_cp_1] = tTitleRec3;
			TfmStats.TTitleRec[] arg_97_0_cp_0 = array;
			int arg_97_0_cp_1 = 3;
			TfmStats.TTitleRec tTitleRec4;
			tTitleRec4.Title = LSID.LSID_Surname;
			tTitleRec4.Cap = LSID.LSID_Surname;
			arg_97_0_cp_0[arg_97_0_cp_1] = tTitleRec4;
			TfmStats.TTitleRec[] arg_B8_0_cp_0 = array;
			int arg_B8_0_cp_1 = 4;
			TfmStats.TTitleRec tTitleRec5;
			tTitleRec5.Title = LSID.LSID_Name;
			tTitleRec5.Cap = LSID.LSID_Name;
			arg_B8_0_cp_0[arg_B8_0_cp_1] = tTitleRec5;
			TfmStats.TTitleRec[] arg_D9_0_cp_0 = array;
			int arg_D9_0_cp_1 = 5;
			TfmStats.TTitleRec tTitleRec6;
			tTitleRec6.Title = LSID.LSID_Patronymic;
			tTitleRec6.Cap = LSID.LSID_Patronymic;
			arg_D9_0_cp_0[arg_D9_0_cp_1] = tTitleRec6;
			TfmStats.TTitleRec[] arg_100_0_cp_0 = array;
			int arg_100_0_cp_1 = 6;
			TfmStats.TTitleRec tTitleRec7;
			tTitleRec7.Title = LSID.LSID_Age;
			tTitleRec7.Cap = LSID.LSID_Age;
			arg_100_0_cp_0[arg_100_0_cp_1] = tTitleRec7;
			TfmStats.TTitleRec[] arg_127_0_cp_0 = array;
			int arg_127_0_cp_1 = 7;
			TfmStats.TTitleRec tTitleRec8;
			tTitleRec8.Title = LSID.LSID_LifeExpectancy;
			tTitleRec8.Cap = LSID.LSID_Age;
			arg_127_0_cp_0[arg_127_0_cp_1] = tTitleRec8;
			TfmStats.TTitleRec[] arg_14E_0_cp_0 = array;
			int arg_14E_0_cp_1 = 8;
			TfmStats.TTitleRec tTitleRec9;
			tTitleRec9.Title = LSID.LSID_BirthYears;
			tTitleRec9.Cap = LSID.LSID_BirthYears;
			arg_14E_0_cp_0[arg_14E_0_cp_1] = tTitleRec9;
			TfmStats.TTitleRec[] arg_176_0_cp_0 = array;
			int arg_176_0_cp_1 = 9;
			TfmStats.TTitleRec tTitleRec10;
			tTitleRec10.Title = LSID.LSID_BirthYearsDec;
			tTitleRec10.Cap = LSID.LSID_BirthYears;
			arg_176_0_cp_0[arg_176_0_cp_1] = tTitleRec10;
			TfmStats.TTitleRec[] arg_19E_0_cp_0 = array;
			int arg_19E_0_cp_1 = 10;
			TfmStats.TTitleRec tTitleRec11;
			tTitleRec11.Title = LSID.LSID_DeathYears;
			tTitleRec11.Cap = LSID.LSID_DeathYears;
			arg_19E_0_cp_0[arg_19E_0_cp_1] = tTitleRec11;
			TfmStats.TTitleRec[] arg_1C6_0_cp_0 = array;
			int arg_1C6_0_cp_1 = 11;
			TfmStats.TTitleRec tTitleRec12;
			tTitleRec12.Title = LSID.LSID_DeathYearsDec;
			tTitleRec12.Cap = LSID.LSID_DeathYears;
			arg_1C6_0_cp_0[arg_1C6_0_cp_1] = tTitleRec12;
			TfmStats.TTitleRec[] arg_1EB_0_cp_0 = array;
			int arg_1EB_0_cp_1 = 12;
			TfmStats.TTitleRec tTitleRec13;
			tTitleRec13.Title = LSID.LSID_ChildsCount;
			tTitleRec13.Cap = LSID.LSID_Name;
			arg_1EB_0_cp_0[arg_1EB_0_cp_1] = tTitleRec13;
			TfmStats.TTitleRec[] arg_213_0_cp_0 = array;
			int arg_213_0_cp_1 = 13;
			TfmStats.TTitleRec tTitleRec14;
			tTitleRec14.Title = LSID.LSID_DistrChilds;
			tTitleRec14.Cap = LSID.LSID_ChildsCount;
			arg_213_0_cp_0[arg_213_0_cp_1] = tTitleRec14;
			TfmStats.TTitleRec[] arg_23B_0_cp_0 = array;
			int arg_23B_0_cp_1 = 14;
			TfmStats.TTitleRec tTitleRec15;
			tTitleRec15.Title = LSID.LSID_BirthPlace;
			tTitleRec15.Cap = LSID.LSID_BirthPlace;
			arg_23B_0_cp_0[arg_23B_0_cp_1] = tTitleRec15;
			TfmStats.TTitleRec[] arg_263_0_cp_0 = array;
			int arg_263_0_cp_1 = 15;
			TfmStats.TTitleRec tTitleRec16;
			tTitleRec16.Title = LSID.LSID_DeathPlace;
			tTitleRec16.Cap = LSID.LSID_DeathPlace;
			arg_263_0_cp_0[arg_263_0_cp_1] = tTitleRec16;
			TfmStats.TTitleRec[] arg_28B_0_cp_0 = array;
			int arg_28B_0_cp_1 = 16;
			TfmStats.TTitleRec tTitleRec17;
			tTitleRec17.Title = LSID.LSID_Residence;
			tTitleRec17.Cap = LSID.LSID_Residence;
			arg_28B_0_cp_0[arg_28B_0_cp_1] = tTitleRec17;
			TfmStats.TTitleRec[] arg_2B3_0_cp_0 = array;
			int arg_2B3_0_cp_1 = 17;
			TfmStats.TTitleRec tTitleRec18;
			tTitleRec18.Title = LSID.LSID_Occupation;
			tTitleRec18.Cap = LSID.LSID_Occupation;
			arg_2B3_0_cp_0[arg_2B3_0_cp_1] = tTitleRec18;
			TfmStats.TTitleRec[] arg_2DB_0_cp_0 = array;
			int arg_2DB_0_cp_1 = 18;
			TfmStats.TTitleRec tTitleRec19;
			tTitleRec19.Title = LSID.LSID_Religion;
			tTitleRec19.Cap = LSID.LSID_Religion;
			arg_2DB_0_cp_0[arg_2DB_0_cp_1] = tTitleRec19;
			TfmStats.TTitleRec[] arg_303_0_cp_0 = array;
			int arg_303_0_cp_1 = 19;
			TfmStats.TTitleRec tTitleRec20;
			tTitleRec20.Title = LSID.LSID_Nationality;
			tTitleRec20.Cap = LSID.LSID_Nationality;
			arg_303_0_cp_0[arg_303_0_cp_1] = tTitleRec20;
			TfmStats.TTitleRec[] arg_32B_0_cp_0 = array;
			int arg_32B_0_cp_1 = 20;
			TfmStats.TTitleRec tTitleRec21;
			tTitleRec21.Title = LSID.LSID_Education;
			tTitleRec21.Cap = LSID.LSID_Education;
			arg_32B_0_cp_0[arg_32B_0_cp_1] = tTitleRec21;
			TfmStats.TTitleRec[] arg_353_0_cp_0 = array;
			int arg_353_0_cp_1 = 21;
			TfmStats.TTitleRec tTitleRec22;
			tTitleRec22.Title = LSID.LSID_Caste;
			tTitleRec22.Cap = LSID.LSID_Caste;
			arg_353_0_cp_0[arg_353_0_cp_1] = tTitleRec22;
			TfmStats.TTitleRec[] arg_378_0_cp_0 = array;
			int arg_378_0_cp_1 = 22;
			TfmStats.TTitleRec tTitleRec23;
			tTitleRec23.Title = LSID.LSID_AgeFirstborn;
			tTitleRec23.Cap = LSID.LSID_Name;
			arg_378_0_cp_0[arg_378_0_cp_1] = tTitleRec23;
			TfmStats.TTitleRec[] arg_39D_0_cp_0 = array;
			int arg_39D_0_cp_1 = 23;
			TfmStats.TTitleRec tTitleRec24;
			tTitleRec24.Title = LSID.LSID_MarriagesCount;
			tTitleRec24.Cap = LSID.LSID_Name;
			arg_39D_0_cp_0[arg_39D_0_cp_1] = tTitleRec24;
			TfmStats.TTitleRec[] arg_3C2_0_cp_0 = array;
			int arg_3C2_0_cp_1 = 24;
			TfmStats.TTitleRec tTitleRec25;
			tTitleRec25.Title = LSID.LSID_MarriagesAge;
			tTitleRec25.Cap = LSID.LSID_Name;
			arg_3C2_0_cp_0[arg_3C2_0_cp_1] = tTitleRec25;
			TfmStats.TTitleRec[] arg_3E7_0_cp_0 = array;
			int arg_3E7_0_cp_1 = 25;
			TfmStats.TTitleRec tTitleRec26;
			tTitleRec26.Title = LSID.LSID_DiffSpouses;
			tTitleRec26.Cap = LSID.LSID_Family;
			arg_3E7_0_cp_0[arg_3E7_0_cp_1] = tTitleRec26;
			TfmStats.TTitleRec[] arg_40F_0_cp_0 = array;
			int arg_40F_0_cp_1 = 26;
			TfmStats.TTitleRec tTitleRec27;
			tTitleRec27.Title = LSID.LSID_Hobby;
			tTitleRec27.Cap = LSID.LSID_Hobby;
			arg_40F_0_cp_0[arg_40F_0_cp_1] = tTitleRec27;
			TfmStats.TTitleRec[] arg_437_0_cp_0 = array;
			int arg_437_0_cp_1 = 27;
			TfmStats.TTitleRec tTitleRec28;
			tTitleRec28.Title = LSID.LSID_Award;
			tTitleRec28.Cap = LSID.LSID_Award;
			arg_437_0_cp_0[arg_437_0_cp_1] = tTitleRec28;
			TfmStats.TTitleRec[] arg_45F_0_cp_0 = array;
			int arg_45F_0_cp_1 = 28;
			TfmStats.TTitleRec tTitleRec29;
			tTitleRec29.Title = LSID.LSID_Mili;
			tTitleRec29.Cap = LSID.LSID_Mili;
			arg_45F_0_cp_0[arg_45F_0_cp_1] = tTitleRec29;
			TfmStats.TTitleRec[] arg_487_0_cp_0 = array;
			int arg_487_0_cp_1 = 29;
			TfmStats.TTitleRec tTitleRec30;
			tTitleRec30.Title = LSID.LSID_MiliInd;
			tTitleRec30.Cap = LSID.LSID_MiliInd;
			arg_487_0_cp_0[arg_487_0_cp_1] = tTitleRec30;
			TfmStats.TTitleRec[] arg_4AF_0_cp_0 = array;
			int arg_4AF_0_cp_1 = 30;
			TfmStats.TTitleRec tTitleRec31;
			tTitleRec31.Title = LSID.LSID_MiliDis;
			tTitleRec31.Cap = LSID.LSID_MiliDis;
			arg_4AF_0_cp_0[arg_4AF_0_cp_1] = tTitleRec31;
			TfmStats.TTitleRec[] arg_4D7_0_cp_0 = array;
			int arg_4D7_0_cp_1 = 31;
			TfmStats.TTitleRec tTitleRec32;
			tTitleRec32.Title = LSID.LSID_MiliRank;
			tTitleRec32.Cap = LSID.LSID_MiliRank;
			arg_4D7_0_cp_0[arg_4D7_0_cp_1] = tTitleRec32;
			TfmStats.Titles = array;
		}

		private static string _TfmStats_Load_GetPercent(int aDividend, int aDivisor)
		{
			double value;
			if (aDivisor == 0)
			{
				value = 0.0;
			}
			else
			{
				value = ((double)aDividend / (double)aDivisor * 100.0);
			}
			return string.Format(" ({0:0.00}%)", new object[] { value });
		}
	}
}
