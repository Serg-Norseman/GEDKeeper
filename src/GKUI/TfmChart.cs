using System;
using System.Drawing;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Charts;

namespace GKUI
{
	public partial class TfmChart : Form
	{
		private TBackManager FBackman;
		private TfmBase FBase;
		private TAncestryChartBox.TChartKind FChartKind;
		private bool FDown;
		private string FFileName;
		private int FGensLimit;
		private TGEDCOMIndividualRecord FPerson;
		private int FScale;
		private TGEDCOMTree FTree;
		private TAncestryChartBox FTreeBox;
		private int FX;
		private int FY;

		public TfmBase Base
		{
			get { return this.FBase; }
			set { this.FBase = value; }
		}

		public TAncestryChartBox.TChartKind ChartKind
		{
			get { return this.FChartKind; }
			set { this.SetChartKind(value); }
		}

		public string FileName
		{
			get { return this.FFileName; }
			set { this.FFileName = value; }
		}

		public TGEDCOMIndividualRecord Person
		{
			get { return this.FPerson; }
			set { this.FPerson = value; }
		}

		public TGEDCOMTree Tree
		{
			get { return this.FTree; }
			set { this.FTree = value; }
		}

		private void DoFilter()
		{
			TfmTreeFilter fmTreeFilter = new TfmTreeFilter(this.Base);
			try
			{
				fmTreeFilter.Filter = this.FTreeBox.Filter;
				if (fmTreeFilter.ShowDialog() == DialogResult.OK)
				{
					this.GenChart(true);
				}
			}
			finally
			{
				TObjectHelper.Free(fmTreeFilter);
			}
		}

		private void DoImageSave()
		{
			if (this.SaveDialog1.ShowDialog() == DialogResult.OK)
			{
				this.FTreeBox.SaveSnapshot(this.SaveDialog1.FileName);
			}
		}

		private void DoNext()
		{
			this.FBackman.BeginNav();
			try
			{
				this.FPerson = (this.FBackman.Next() as TGEDCOMIndividualRecord);
				this.GenChart(true);
				this.NavRefresh();
			}
			finally
			{
				this.FBackman.EndNav();
			}
		}

		private void DoPrev()
		{
			this.FBackman.BeginNav();
			try
			{
				this.FPerson = (this.FBackman.Back() as TGEDCOMIndividualRecord);
				this.GenChart(true);
				this.NavRefresh();
			}
			finally
			{
				this.FBackman.EndNav();
			}
		}

		private void InternalChildAdd(TGEDCOMSex aNeedSex)
		{
			TPerson p = this.FTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				TGEDCOMIndividualRecord i_rec = p.Rec;
				if (i_rec.SpouseToFamilyLinks.Count == 0)
				{
					SysUtils.ShowError(GKL.LSList[211]);
				}
				else
				{
					if (i_rec.SpouseToFamilyLinks.Count > 1)
					{
						SysUtils.ShowError("У данной персоны несколько семей. Выбор еще не реализован.");
					}
					else
					{
						TGEDCOMFamilyRecord fam = i_rec.SpouseToFamilyLinks[0].Family;
						TGEDCOMIndividualRecord i_child = this.Base.SelectPerson(fam.Husband.Value as TGEDCOMIndividualRecord, TGenEngine.TTargetMode.tmParent, aNeedSex);
						if (i_child != null && this.Base.Engine.AddFamilyChild(fam, i_child))
						{
							this.UpdateChart();
						}
					}
				}
			}
		}

		private void NavRefresh()
		{
			this.tbPrev.Enabled = this.FBackman.CanBackward();
			this.tbNext.Enabled = this.FBackman.CanForward();
		}

		private void NavAdd(TGEDCOMIndividualRecord aRec)
		{
			if (aRec != null && !this.FBackman.Busy)
			{
				this.FBackman.Current = aRec;
				this.NavRefresh();
			}
		}

		private void UpdateChart()
		{
			if (this.FBase != null)
			{
				this.FBase.ListsRefresh(false);
			}
			this.GenChart(true);
		}

		private void TfmChart_KeyDown(object sender, KeyEventArgs e)
		{
			switch (e.KeyCode) {
				case Keys.F6: {
					this.miRebuildTreeClick(null, null);
					break;
				}

				case Keys.F7: {
					this.FTreeBox.RebuildKinships();
					break;
				}

				case Keys.Escape: {
					base.Close();
					break;
				}
			}
		}

		private void ToolBar1_ButtonClick(object sender, ToolBarButtonClickEventArgs e)
		{
			if (object.Equals(e.Button, this.tbImageSave))
			{
				this.DoImageSave();
			}
			else
			{
				if (object.Equals(e.Button, this.tbPrev))
				{
					this.DoPrev();
				}
				else
				{
					if (object.Equals(e.Button, this.tbNext))
					{
						this.DoNext();
					}
					else
					{
						if (object.Equals(e.Button, this.tbFilter))
						{
							this.DoFilter();
						}
					}
				}
			}
		}

		private void ImageTree_MouseDown(object sender, MouseEventArgs e)
		{
			this.FX = e.X;
			this.FY = e.Y;
			if (e.Button == MouseButtons.Right)
			{
				this.FTreeBox.Cursor = Cursors.SizeAll;
				this.FDown = true;
			}
		}

		private void ImageTree_MouseMove(object sender, MouseEventArgs e)
		{
			if (this.FDown)
			{
				this.FTreeBox.LeftPos = this.FTreeBox.LeftPos - (e.X - this.FX);
				this.FTreeBox.TopPos = this.FTreeBox.TopPos - (e.Y - this.FY);
				this.FX = e.X;
				this.FY = e.Y;
			}
		}

		private void ImageTree_MouseUp(object sender, MouseEventArgs e)
		{
			if (this.FDown)
			{
				this.FTreeBox.Cursor = Cursors.Default;
				this.FDown = false;
			}
			this.FTreeBox.SelectBy(e.X, e.Y);
			if (this.FTreeBox.Selected != null && this.FTreeBox.Selected.Rec != null)
			{
				MouseButtons button = e.Button;
				if (button != MouseButtons.Left)
				{
					if (button == MouseButtons.Right)
					{
						this.MenuPerson.Show(this.FTreeBox, new Point(e.X, e.Y));
					}
				}
				else
				{
					if (this.miTraceRoot.Checked)
					{
						this.Person = this.FTreeBox.Selected.Rec;
						this.GenChart(true);
						this.FTreeBox.SelectByRec(this.FPerson);
					}
				}
			}
		}

		private void ImageTree_DblClick(object sender, EventArgs e)
		{
			TPerson p = this.FTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				TGEDCOMIndividualRecord i_rec = p.Rec;
				if (this.FBase.ModifyPerson(ref i_rec))
				{
					this.UpdateChart();
				}
			}
		}

		private void miGens9Click(object sender, EventArgs e)
		{
			this.miGensInf.Checked = false;
			this.miGens1.Checked = false;
			this.miGens2.Checked = false;
			this.miGens3.Checked = false;
			this.miGens4.Checked = false;
			this.miGens5.Checked = false;
			this.miGens6.Checked = false;
			this.miGens7.Checked = false;
			this.miGens8.Checked = false;
			this.miGens9.Checked = false;
			(sender as MenuItem).Checked = true;
			if (object.Equals(sender, this.miGensInf))
			{
				this.FGensLimit = -1;
			}
			if (object.Equals(sender, this.miGens1))
			{
				this.FGensLimit = 1;
			}
			if (object.Equals(sender, this.miGens2))
			{
				this.FGensLimit = 2;
			}
			if (object.Equals(sender, this.miGens3))
			{
				this.FGensLimit = 3;
			}
			if (object.Equals(sender, this.miGens4))
			{
				this.FGensLimit = 4;
			}
			if (object.Equals(sender, this.miGens5))
			{
				this.FGensLimit = 5;
			}
			if (object.Equals(sender, this.miGens6))
			{
				this.FGensLimit = 6;
			}
			if (object.Equals(sender, this.miGens7))
			{
				this.FGensLimit = 7;
			}
			if (object.Equals(sender, this.miGens8))
			{
				this.FGensLimit = 8;
			}
			if (object.Equals(sender, this.miGens9))
			{
				this.FGensLimit = 9;
			}
			this.GenChart(true);
		}

		private void N1001Click(object sender, EventArgs e)
		{
			this.N501.Checked = false;
			this.N601.Checked = false;
			this.N701.Checked = false;
			this.N801.Checked = false;
			this.N901.Checked = false;
			this.N1001.Checked = false;
			(sender as MenuItem).Checked = true;
			if (object.Equals(sender, this.N501))
			{
				this.FScale = 5;
			}
			if (object.Equals(sender, this.N601))
			{
				this.FScale = 6;
			}
			if (object.Equals(sender, this.N701))
			{
				this.FScale = 7;
			}
			if (object.Equals(sender, this.N801))
			{
				this.FScale = 8;
			}
			if (object.Equals(sender, this.N901))
			{
				this.FScale = 9;
			}
			if (object.Equals(sender, this.N1001))
			{
				this.FScale = 10;
			}
			this.GenChart(true);
		}

		private void miEditClick(object sender, EventArgs e)
		{
			TPerson p = this.FTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				TGEDCOMIndividualRecord i_rec = p.Rec;
				if (this.FBase.ModifyPerson(ref i_rec))
				{
					this.UpdateChart();
				}
			}
		}

		private void miSpouseAddClick(object sender, EventArgs e)
		{
			TPerson p = this.FTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				TGEDCOMIndividualRecord i_rec = p.Rec;
				TGEDCOMSex sex = i_rec.Sex;
				TGEDCOMSex sx;
				if (sex != TGEDCOMSex.svMale)
				{
					if (sex != TGEDCOMSex.svFemale)
					{
						SysUtils.ShowError(GKL.LSList[210]);
						return;
					}
					sx = TGEDCOMSex.svMale;
				}
				else
				{
					sx = TGEDCOMSex.svFemale;
				}
				TGEDCOMIndividualRecord i_spouse = this.Base.SelectPerson(null, TGenEngine.TTargetMode.tmNone, sx);
				if (i_spouse != null)
				{
					TGEDCOMFamilyRecord fam = TGenEngine.CreateFamilyEx(this.FTree);
					this.Base.Engine.AddFamilySpouse(fam, i_rec);
					this.Base.Engine.AddFamilySpouse(fam, i_spouse);
					this.UpdateChart();
				}
			}
		}

		private void miSonAddClick(object sender, EventArgs e)
		{
			this.InternalChildAdd(TGEDCOMSex.svMale);
		}

		private void miDaughterAddClick(object sender, EventArgs e)
		{
			this.InternalChildAdd(TGEDCOMSex.svFemale);
		}

		private void miFamilyAddClick(object sender, EventArgs e)
		{
			TPerson p = this.FTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				TGEDCOMSex sex = p.Rec.Sex;
				if (sex < TGEDCOMSex.svMale || sex >= TGEDCOMSex.svUndetermined)
				{
					SysUtils.ShowError(GKL.LSList[210]);
				}
				else
				{
					TGEDCOMFamilyRecord fam = TGenEngine.CreateFamilyEx(this.FTree);
					this.Base.Engine.AddFamilySpouse(fam, p.Rec);
					this.UpdateChart();
				}
			}
		}

		private void miDeleteClick(object sender, EventArgs e)
		{
			TPerson p = this.FTreeBox.Selected;
			if (p != null && p.Rec != null && !object.Equals(p, this.FTreeBox.Root))
			{
				this.FBase.DeleteIndividualRecord(p.Rec, true);
				this.GenChart(true);
				this.NavRefresh();
			}
		}

		private void miRebuildKinshipsClick(object sender, EventArgs e)
		{
			this.FTreeBox.RebuildKinships();
		}

		private void miTraceRootClick(object sender, EventArgs e)
		{
			this.miTraceRoot.Checked = !this.miTraceRoot.Checked;
		}

		private void UpdateModesMenu([In] TAncestryChartBox.TChartKind aChartKind)
		{
			this.miModeBoth.Checked = false;
			this.miModeAncestors.Checked = false;
			this.miModeDescendants.Checked = false;

			switch (aChartKind) {
				case TAncestryChartBox.TChartKind.ckAncestors:
				{
					this.miModeAncestors.Checked = true;
					break;
				}
				case TAncestryChartBox.TChartKind.ckDescendants:
				{
					this.miModeDescendants.Checked = true;
					break;
				}
				case TAncestryChartBox.TChartKind.ckBoth:
				{
					this.miModeBoth.Checked = true;
					break;
				}
			}
		}

		private void SetChartKind([In] TAncestryChartBox.TChartKind Value)
		{
			this.FChartKind = Value;
			UpdateModesMenu(Value);
		}

		private void miModeDescendantsClick(object sender, EventArgs e)
		{
			TAncestryChartBox.TChartKind newMode = TAncestryChartBox.TChartKind.ckBoth;

			if (sender == this.miModeBoth) {
				newMode = TAncestryChartBox.TChartKind.ckBoth;
			} else if (sender == this.miModeAncestors) {
				newMode = TAncestryChartBox.TChartKind.ckAncestors;
			} else if (sender == this.miModeDescendants) {
				newMode = TAncestryChartBox.TChartKind.ckDescendants;
			}

			if (this.FChartKind != newMode)
			{
				this.SetChartKind(newMode);
				this.GenChart(true);
			}
		}

		private void miRebuildTreeClick(object sender, EventArgs e)
		{
			TPerson p = this.FTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				this.FPerson = p.Rec;
				this.GenChart(true);
				this.NavRefresh();
			}
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.FBackman.Free();
			}
			base.Dispose(Disposing);
		}

		public TfmChart(TfmBase aBase)
		{
			this.InitializeComponent();
			base.MdiParent = GKUI.TfmGEDKeeper.Instance;
			this.FBase = aBase;
			this.ToolBar1.ImageList = GKUI.TfmGEDKeeper.Instance.ImageList_Buttons;
			this.FTreeBox = new TAncestryChartBox();
			this.FTreeBox.Dock = DockStyle.Fill;
			this.FTreeBox.MouseDown += new MouseEventHandler(this.ImageTree_MouseDown);
			this.FTreeBox.MouseUp += new MouseEventHandler(this.ImageTree_MouseUp);
			this.FTreeBox.MouseMove += new MouseEventHandler(this.ImageTree_MouseMove);
			this.FTreeBox.DoubleClick += new EventHandler(this.ImageTree_DblClick);
			base.Controls.Add(this.FTreeBox);
			base.Controls.SetChildIndex(this.FTreeBox, 0);
			base.Controls.SetChildIndex(this.ToolBar1, 1);
			this.FBackman = new TBackManager();
			this.NavRefresh();
			this.FGensLimit = -1;
			this.FScale = 10;
			this.SetLang();
		}

		public static bool CheckData(TGEDCOMTree aTree, TGEDCOMIndividualRecord iRec, TAncestryChartBox.TChartKind aKind)
		{
			bool Result = true;
			if (aKind == TAncestryChartBox.TChartKind.ckAncestors || aKind == TAncestryChartBox.TChartKind.ckBoth)
			{
				TGenEngine.InitExtCounts(aTree, -1);
				int anc_count = TGenEngine.GetAncestorsCount(iRec);
				if (anc_count > 2048)
				{
					SysUtils.ShowMessage(string.Format(GKL.LSList[212], new object[]
					{
						anc_count.ToString()
					}));
					Result = false;
					return Result;
				}
			}
			if (aKind >= TAncestryChartBox.TChartKind.ckDescendants && aKind < (TAncestryChartBox.TChartKind)3)
			{
				TGenEngine.InitExtCounts(aTree, -1);
				int desc_count = TGenEngine.GetDescendantsCount(iRec);
				if (desc_count > 2048)
				{
					SysUtils.ShowMessage(string.Format(GKL.LSList[213], new object[]
					{
						desc_count.ToString()
					}));
					Result = false;
				}
			}
			return Result;
		}

		public void GenChart(bool aShow)
		{
			if (this.FPerson == null)
			{
				SysUtils.ShowError(GKL.LSList[209]);
			}
			else
			{
				try
				{
					this.NavAdd(this.FPerson);
					this.FTreeBox.DepthLimit = this.FGensLimit;
					this.FTreeBox.Options = GKUI.TfmGEDKeeper.Instance.Options.ChartOptions;
					this.FTreeBox.Engine = this.Base.Engine;
					this.FTreeBox.Tree = this.FTree;
					this.FTreeBox.ShieldState = this.Base.ShieldState;
					this.FTreeBox.Scale = this.FScale * 10;

					this.FTreeBox.GenChart(this.FPerson, this.FChartKind);

					TAncestryChartBox.TChartKind fChartKind = this.FChartKind;

					if (fChartKind != TAncestryChartBox.TChartKind.ckAncestors)
					{
						if (fChartKind != TAncestryChartBox.TChartKind.ckDescendants)
						{
							if (fChartKind == TAncestryChartBox.TChartKind.ckBoth)
							{
								this.Text = GKL.LSList[25];
							}
						}
						else
						{
							this.Text = GKL.LSList[24];
						}
					}
					else
					{
						this.Text = GKL.LSList[23];
					}
					this.Text = this.Text + " \"" + this.FFileName + "\"";
					if (aShow)
					{
						base.Show();
					}
				}
				catch (Exception E)
				{
					SysUtils.ShowError(E.Message);
				}
			}
		}

		public void SetLang()
		{
			this.miGensInf.Text = GKL.LSList[215];
			this.miGensInf.Checked = true;
			this.miModeBoth.Text = GKL.LSList[222];
			this.miModeAncestors.Text = GKL.LSList[223];
			this.miModeDescendants.Text = GKL.LSList[224];
			this.miTraceRoot.Text = GKL.LSList[225];
			this.miEdit.Text = GKL.LSList[226];
			this.miFamilyAdd.Text = GKL.LSList[227];
			this.miSpouseAdd.Text = GKL.LSList[228];
			this.miSonAdd.Text = GKL.LSList[229];
			this.miDaughterAdd.Text = GKL.LSList[230];
			this.miDelete.Text = GKL.LSList[231];
			this.miRebuildTree.Text = GKL.LSList[232];
			this.miRebuildKinships.Text = GKL.LSList[233];
		}
	}
}
