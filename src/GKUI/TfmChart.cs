using System;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKCore;
using GKUI.Charts;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKUI
{
	public enum ChartControlMode
	{
		ccmDefault,
		ccmDragImage,
		ccmControlsVisible
	}

	public partial class TfmChart : Form
	{
		private NavManager FNavman;
		private TfmBase FBase;
		private TTreeChartBox.TChartKind FChartKind;
		private string FFileName;
		private int FGensLimit;
		private TGEDCOMIndividualRecord FPerson;
		private float FScale;
		private TGEDCOMTree FTree;
		private TTreeChartBox FTreeBox;
		private int FX;
		private int FY;
		private ChartControlMode FMode = ChartControlMode.ccmDefault;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		public TTreeChartBox.TChartKind ChartKind
		{
			get { return this.FChartKind; }
			set { this.SetChartKind(value); }
		}

		public TfmChart(TfmBase aBase, TGEDCOMIndividualRecord StartPerson)
		{
			this.InitializeComponent();

			base.MdiParent = GKUI.TfmGEDKeeper.Instance;
			this.ToolBar1.ImageList = GKUI.TfmGEDKeeper.Instance.ImageList_Buttons;

			this.FTreeBox = new TTreeChartBox();
			this.FTreeBox.Dock = DockStyle.Fill;
			this.FTreeBox.MouseDown += new MouseEventHandler(this.ImageTree_MouseDown);
			this.FTreeBox.MouseUp += new MouseEventHandler(this.ImageTree_MouseUp);
			this.FTreeBox.MouseMove += new MouseEventHandler(this.ImageTree_MouseMove);
			this.FTreeBox.DoubleClick += new EventHandler(this.ImageTree_DblClick);
			this.FTreeBox.MouseClick += this.ImageTree_MouseClick;
			this.FTreeBox.DragOver += this.ImageTree_DragOver;

			base.Controls.Add(this.FTreeBox);
			base.Controls.SetChildIndex(this.FTreeBox, 0);
			base.Controls.SetChildIndex(this.ToolBar1, 1);

			this.FNavman = new NavManager();
			this.NavRefresh();
			this.FGensLimit = -1;
			this.FScale = 1.0f;
			this.SetLang();

			this.FBase = aBase;
			this.FTree = aBase.Tree;
			this.FFileName = Path.GetFileName(aBase.FileName);
			this.FPerson = StartPerson;
			
			this.miTraceRoot.Checked = this.FTreeBox.TraceSelected;
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.FNavman.Dispose();
			}
			base.Dispose(Disposing);
		}

		private void DoFilter()
		{
			TfmTreeFilter fmTreeFilter = new TfmTreeFilter(this.FBase);
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
				fmTreeFilter.Dispose();
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
			this.FNavman.BeginNav();
			try
			{
				this.FPerson = (this.FNavman.Next() as TGEDCOMIndividualRecord);
				this.GenChart(true);
				this.NavRefresh();
			}
			finally
			{
				this.FNavman.EndNav();
			}
		}

		private void DoPrev()
		{
			this.FNavman.BeginNav();
			try
			{
				this.FPerson = (this.FNavman.Back() as TGEDCOMIndividualRecord);
				this.GenChart(true);
				this.NavRefresh();
			}
			finally
			{
				this.FNavman.EndNav();
			}
		}

		private void NavRefresh()
		{
			this.tbPrev.Enabled = this.FNavman.CanBackward();
			this.tbNext.Enabled = this.FNavman.CanForward();
		}

		private void NavAdd(TGEDCOMIndividualRecord aRec)
		{
			if (aRec != null && !this.FNavman.Busy)
			{
				this.FNavman.Current = aRec;
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
			switch (e.KeyCode)
			{
				case Keys.F5:
					this.GenChart(true);
					this.NavRefresh();
					break;

				case Keys.F6:
					this.miRebuildTreeClick(null, null);
					break;

				case Keys.F7:
					this.FTreeBox.RebuildKinships();
					break;

				case Keys.Escape:
					base.Close();
					break;
			}
		}

		private void ToolBar1_ButtonClick(object sender, ToolBarButtonClickEventArgs e)
		{
			if (e.Button == this.tbImageSave) {
				this.DoImageSave();
			} else if (e.Button == this.tbPrev) {
				this.DoPrev();
			} else if (e.Button == this.tbNext) {
				this.DoNext();
			} else if (e.Button == this.tbFilter) {
				this.DoFilter();
			}
		}

		private void ImageTree_DragOver(object sender, DragEventArgs e)
		{
//			if (e.Data.GetDataPresent(typeof(string)))
//			{
//				e.Effect = DragDropEffects.Move;
//			}
//			else
//			{
//				e.Effect = DragDropEffects.None;
//			}
		}

		private void ImageTree_MouseDown(object sender, MouseEventArgs e)
		{
			this.FX = e.X;
			this.FY = e.Y;

			switch (this.FMode) {
				case ChartControlMode.ccmDefault:
					this.FTreeBox.SelectBy(e.X, e.Y, (e.Button == MouseButtons.Left));

					if (this.FTreeBox.Selected == null && e.Button == MouseButtons.Right)
					{
						this.FTreeBox.Cursor = Cursors.SizeAll;
						this.FMode = ChartControlMode.ccmDragImage;
					}
					break;

				case ChartControlMode.ccmDragImage:
					break;

				case ChartControlMode.ccmControlsVisible:
					this.FTreeBox.ScaleControl.MouseDown(e.X, e.Y);
					break;
			}
		}

		private void ImageTree_MouseMove(object sender, MouseEventArgs e)
		{
			switch (this.FMode)
			{
				case ChartControlMode.ccmDefault:
					if (this.FTreeBox.ScaleControl.Contains(e.X, e.Y))
					{
						this.FMode = ChartControlMode.ccmControlsVisible;
						this.FTreeBox.ScaleControl.Visible = true;
						this.FTreeBox.ScaleControl.MouseMove(e.X, e.Y, ThumbMoved);

						Point pt = new Point(e.X, e.Y);
						pt.Offset(+this.FTreeBox.Left, +this.FTreeBox.Top);
						this.toolTip1.Show(this.FTreeBox.ScaleControl.Tip, this, pt, 1500);
					} else {
//						TreeChartPerson p = this.FTreeBox.FindPersonByCoords(e.X, e.Y);
//
//						if (p != null && e.Button == MouseButtons.Left)
//						{
//							this.FTreeBox.DoDragDrop(p.Rec.XRef, DragDropEffects.Move);
//						}
					}
					break;

				case ChartControlMode.ccmDragImage:
					this.FTreeBox.LeftPos = this.FTreeBox.LeftPos - (e.X - this.FX);
					this.FTreeBox.TopPos = this.FTreeBox.TopPos - (e.Y - this.FY);
					this.FX = e.X;
					this.FY = e.Y;
					break;

				case ChartControlMode.ccmControlsVisible:
					if (!this.FTreeBox.ScaleControl.Contains(e.X, e.Y)) {
						this.FMode = ChartControlMode.ccmDefault;
						this.FTreeBox.ScaleControl.Visible = false;
						this.toolTip1.Hide(this);
					} else {
						this.FTreeBox.ScaleControl.MouseMove(e.X, e.Y, ThumbMoved);
					}
					break;
			}
		}

		private void ThumbMoved(int position)
		{
			this.FTreeBox.Scale = 0.4f + (position * 0.1f);
		}

		private void ImageTree_MouseUp(object sender, MouseEventArgs e)
		{
			switch (this.FMode) {
				case ChartControlMode.ccmDefault:
					if (this.FTreeBox.Selected != null && this.FTreeBox.Selected.Rec != null)
					{
						switch (e.Button) {
							case MouseButtons.Left:
								break;

							case MouseButtons.Right:
								this.MenuPerson.Show(this.FTreeBox, new Point(e.X, e.Y));
								break;
						}
					}
					break;

				case ChartControlMode.ccmDragImage:
					this.FTreeBox.Cursor = Cursors.Default;
					this.FMode = ChartControlMode.ccmDefault;
					break;

				case ChartControlMode.ccmControlsVisible:
					this.FTreeBox.ScaleControl.MouseUp(e.X, e.Y);
					break;
			}
		}

		private void ImageTree_MouseClick(object sender, MouseEventArgs e)
		{
		}

		private void ImageTree_DblClick(object sender, EventArgs e)
		{
			TreeChartPerson p = this.FTreeBox.Selected;
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

			if (sender == this.miGensInf) this.FGensLimit = -1;
			if (sender == this.miGens1) this.FGensLimit = 1;
			if (sender == this.miGens2) this.FGensLimit = 2;
			if (sender == this.miGens3) this.FGensLimit = 3;
			if (sender == this.miGens4) this.FGensLimit = 4;
			if (sender == this.miGens5) this.FGensLimit = 5;
			if (sender == this.miGens6) this.FGensLimit = 6;
			if (sender == this.miGens7) this.FGensLimit = 7;
			if (sender == this.miGens8) this.FGensLimit = 8;
			if (sender == this.miGens9) this.FGensLimit = 9;
			this.GenChart(true);
		}

		private void miEditClick(object sender, EventArgs e)
		{
			TreeChartPerson p = this.FTreeBox.Selected;
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
			TreeChartPerson p = this.FTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				TGEDCOMIndividualRecord i_rec = p.Rec;
				TGEDCOMSex sx;
				switch (i_rec.Sex) {
					case TGEDCOMSex.svMale:
						sx = TGEDCOMSex.svFemale;
						break;
					case TGEDCOMSex.svFemale:
						sx = TGEDCOMSex.svMale;
						break;
					default:
						TGenEngine.ShowError(LangMan.LSList[210]);
						return;
				}

				TGEDCOMIndividualRecord target = null;
				TGenEngine.TTargetMode target_mode = TGenEngine.TTargetMode.tmNone;
				if (sx == TGEDCOMSex.svFemale) {
					target = i_rec;
					target_mode = TGenEngine.TTargetMode.tmWife;
				}
				
				TGEDCOMIndividualRecord i_spouse = this.FBase.SelectPerson(target, target_mode, sx);
				if (i_spouse != null)
				{
					TGEDCOMFamilyRecord fam = TGenEngine.CreateFamilyEx(this.FTree);
					fam.aux_AddSpouse(i_rec);
					fam.aux_AddSpouse(i_spouse);
					this.UpdateChart();
				}
			}
		}

		private void InternalChildAdd(TGEDCOMSex needSex)
		{
			TreeChartPerson p = this.FTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				TGEDCOMIndividualRecord i_rec = p.Rec;
				if (i_rec.SpouseToFamilyLinks.Count == 0)
				{
					TGenEngine.ShowError(LangMan.LSList[211]);
				}
				else
				{
					if (i_rec.SpouseToFamilyLinks.Count > 1)
					{
						TGenEngine.ShowError("У данной персоны несколько семей. Выбор еще не реализован.");
					}
					else
					{
						TGEDCOMFamilyRecord fam = i_rec.SpouseToFamilyLinks[0].Family;
						TGEDCOMIndividualRecord i_child = this.FBase.SelectPerson(fam.Husband.Value as TGEDCOMIndividualRecord, TGenEngine.TTargetMode.tmParent, needSex);

						if (i_child != null && fam.aux_AddChild(i_child))
						{
							this.UpdateChart();
						}
					}
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
			TreeChartPerson p = this.FTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				TGEDCOMSex sex = p.Rec.Sex;
				if (sex < TGEDCOMSex.svMale || sex >= TGEDCOMSex.svUndetermined)
				{
					TGenEngine.ShowError(LangMan.LSList[210]);
				}
				else
				{
					TGEDCOMFamilyRecord fam = TGenEngine.CreateFamilyEx(this.FTree);
					fam.aux_AddSpouse(p.Rec);
					this.UpdateChart();
				}
			}
		}

		private void miDeleteClick(object sender, EventArgs e)
		{
			TreeChartPerson p = this.FTreeBox.Selected;
			if (p != null && p.Rec != null && !object.Equals(p, this.FTreeBox.Root))
			{
				this.FBase.DeleteIndividualRecord(p.Rec, true);
				this.GenChart(true);
				this.NavRefresh();
			}
		}

		void miRebuildKinshipsClick(object sender, EventArgs e)
		{
			this.FTreeBox.RebuildKinships();
		}

		void miTraceRootClick(object sender, EventArgs e)
		{
			this.miTraceRoot.Checked = !this.miTraceRoot.Checked;
			FTreeBox.TraceSelected = this.miTraceRoot.Checked;
		}

		void miFillColorClick(object sender, EventArgs e)
		{
			if (colorDialog1.ShowDialog() == DialogResult.OK)
			{
				this.FTreeBox.BackgroundImage = null;
				this.FTreeBox.BackColor = colorDialog1.Color;
				this.FTreeBox.Invalidate();
			}
		}

		void miFillImageClick(object sender, EventArgs e)
		{
			OpenDialog1.InitialDirectory = TGenEngine.GetAppPath() + "\\backgrounds";
			if (OpenDialog1.ShowDialog() == DialogResult.OK)
			{
				Image img = new Bitmap(OpenDialog1.FileName);
				this.FTreeBox.BackgroundImage = img;
				this.FTreeBox.BackgroundImageLayout = ImageLayout.Tile;
				this.FTreeBox.Invalidate();
			}
		}

		private void UpdateModesMenu(TTreeChartBox.TChartKind chartKind)
		{
			this.miModeBoth.Checked = false;
			this.miModeAncestors.Checked = false;
			this.miModeDescendants.Checked = false;

			switch (chartKind)
			{
				case TTreeChartBox.TChartKind.ckAncestors:
					this.miModeAncestors.Checked = true;
					break;
				case TTreeChartBox.TChartKind.ckDescendants:
					this.miModeDescendants.Checked = true;
					break;
				case TTreeChartBox.TChartKind.ckBoth:
					this.miModeBoth.Checked = true;
					break;
			}
		}

		private void SetChartKind(TTreeChartBox.TChartKind Value)
		{
			this.FChartKind = Value;
			UpdateModesMenu(Value);
		}

		private void miModeDescendantsClick(object sender, EventArgs e)
		{
			TTreeChartBox.TChartKind newMode = TTreeChartBox.TChartKind.ckBoth;

			if (sender == this.miModeBoth) {
				newMode = TTreeChartBox.TChartKind.ckBoth;
			} else if (sender == this.miModeAncestors) {
				newMode = TTreeChartBox.TChartKind.ckAncestors;
			} else if (sender == this.miModeDescendants) {
				newMode = TTreeChartBox.TChartKind.ckDescendants;
			}

			if (this.FChartKind != newMode)
			{
				this.SetChartKind(newMode);
				this.GenChart(true);
			}
		}

		private void miRebuildTreeClick(object sender, EventArgs e)
		{
			try
			{
				TreeChartPerson p = this.FTreeBox.Selected;
				if (p != null && p.Rec != null)
				{
					this.FPerson = p.Rec;
					this.GenChart(true);
					this.NavRefresh();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKChart.RebuildTree(): " + E.Message);
			}
		}

		public static bool CheckData(TGEDCOMTree tree, TGEDCOMIndividualRecord iRec, TTreeChartBox.TChartKind chartKind)
		{
			bool result = true;

			if (chartKind == TTreeChartBox.TChartKind.ckAncestors || chartKind == TTreeChartBox.TChartKind.ckBoth)
			{
				TGenEngine.InitExtCounts(tree, -1);
				int anc_count = TGenEngine.GetAncestorsCount(iRec);
				if (anc_count > 2048)
				{
					TGenEngine.ShowMessage(string.Format(LangMan.LSList[212], anc_count.ToString()));
					result = false;
					return result;
				}
			}

			if (chartKind >= TTreeChartBox.TChartKind.ckDescendants && chartKind < (TTreeChartBox.TChartKind)3)
			{
				TGenEngine.InitExtCounts(tree, -1);
				int desc_count = TGenEngine.GetDescendantsCount(iRec);
				if (desc_count > 2048)
				{
					TGenEngine.ShowMessage(string.Format(LangMan.LSList[213], desc_count.ToString()));
					result = false;
				}
			}

			return result;
		}

		public void GenChart(bool show)
		{
			try
			{
				if (this.FPerson == null)
				{
					TGenEngine.ShowError(LangMan.LSList[209]);
				}
				else
				{
					this.NavAdd(this.FPerson);
					this.FTreeBox.DepthLimit = this.FGensLimit;
					this.FTreeBox.Options = GKUI.TfmGEDKeeper.Instance.Options.ChartOptions;
					this.FTreeBox.Engine = this.FBase.Engine;
					this.FTreeBox.Tree = this.FTree;
					this.FTreeBox.ShieldState = this.FBase.ShieldState;
					this.FTreeBox.Scale = this.FScale;

					this.FTreeBox.GenChart(this.FPerson, this.FChartKind);

					switch (this.FChartKind)
					{
						case TTreeChartBox.TChartKind.ckAncestors:
							this.Text = LangMan.LSList[23];
							break;
						case TTreeChartBox.TChartKind.ckDescendants:
							this.Text = LangMan.LSList[24];
							break;
						case TTreeChartBox.TChartKind.ckBoth:
							this.Text = LangMan.LSList[25];
							break;
					}

					this.Text = this.Text + " \"" + this.FFileName + "\"";

					if (show) base.Show();

					TfmGEDKeeper.Instance.UpdateControls(false);
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKChart.GenChart(): " + E.Message);
			}
		}

		public string GetStatusString()
		{
			return string.Format(LangMan.LS(LSID.LSID_TreeIndividualsCount), FTreeBox.IndividualsCount.ToString());
		}

		public void SetLang()
		{
			this.miGensInf.Text = LangMan.LSList[215];
			this.miGensInf.Checked = true;
			this.miModeBoth.Text = LangMan.LSList[222];
			this.miModeAncestors.Text = LangMan.LSList[223];
			this.miModeDescendants.Text = LangMan.LSList[224];
			this.miTraceRoot.Text = LangMan.LSList[225];
			this.miEdit.Text = LangMan.LSList[226];
			this.miFamilyAdd.Text = LangMan.LSList[227];
			this.miSpouseAdd.Text = LangMan.LSList[228];
			this.miSonAdd.Text = LangMan.LSList[229];
			this.miDaughterAdd.Text = LangMan.LSList[230];
			this.miDelete.Text = LangMan.LSList[231];
			this.miRebuildTree.Text = LangMan.LSList[232];
			this.miRebuildKinships.Text = LangMan.LSList[233];

			this.miFillColor.Text = LangMan.LS(LSID.LSID_FillColor);
			this.miFillImage.Text = LangMan.LS(LSID.LSID_FillImage);

			this.FTreeBox.ScaleControl.Tip = LangMan.LS(LSID.LSID_Scale);
		}

	}
}
