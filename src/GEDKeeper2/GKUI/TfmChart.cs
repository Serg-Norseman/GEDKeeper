using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Charts;
using GKUI.Dialogs;

namespace GKUI
{
    /// <summary>
    /// Localization: dirty
    /// </summary>
    public partial class TfmChart : Form, ILocalization, IWorkWindow
	{
        private readonly IBase fBase;
        private readonly NavigationStack fNavman;
        private readonly GEDCOMTree fTree;
        private readonly TreeChartBox fTreeBox;

        private TreeChartBox.ChartKind fChartKind;
		private int fGensLimit;
		private GEDCOMIndividualRecord fPerson;

		public IBase Base
		{
			get { return this.fBase; }
		}

		public TreeChartBox.ChartKind ChartKind
		{
			get { return this.fChartKind; }
			set {
				this.fChartKind = value;
				UpdateModesMenu();
			}
		}

		public TfmChart(IBase aBase, GEDCOMIndividualRecord startPerson)
		{
			this.InitializeComponent();
			base.MdiParent = TfmGEDKeeper.Instance;

			this.ToolBar1.ImageList = TfmGEDKeeper.Instance.ImageList_Buttons;

			this.miModeBoth.Tag = TreeChartBox.ChartKind.ckBoth;
			this.miModeAncestors.Tag = TreeChartBox.ChartKind.ckAncestors;
			this.miModeDescendants.Tag = TreeChartBox.ChartKind.ckDescendants;

			this.fBase = aBase;
			this.fTree = aBase.Tree;
			this.fPerson = startPerson;
			
			this.fTreeBox = new TreeChartBox();
			this.fTreeBox.Base = this.fBase;
			this.fTreeBox.Tree = this.fTree;
			this.fTreeBox.Dock = DockStyle.Fill;
			//this.fTreeBox.MouseClick += this.ImageTree_MouseClick;
			this.fTreeBox.DragOver += this.ImageTree_DragOver;
			this.fTreeBox.PersonModify += this.ImageTree_PersonModify;
			this.fTreeBox.RootChanged += ImageTree_RootChanged;
			this.fTreeBox.PersonProperties += this.ImageTree_PersonProperties;
			this.fTreeBox.Options = TfmGEDKeeper.Instance.Options.ChartOptions;

			base.Controls.Add(this.fTreeBox);
			base.Controls.SetChildIndex(this.fTreeBox, 0);
			base.Controls.SetChildIndex(this.ToolBar1, 1);

			this.fNavman = new NavigationStack();
			this.NavRefresh();
			this.fGensLimit = -1;
			this.SetLang();

			this.miCertaintyIndex.Checked = this.fTreeBox.Options.CertaintyIndexVisible;
			this.fTreeBox.CertaintyIndex = this.fTreeBox.Options.CertaintyIndexVisible;
			
			this.miTraceSelected.Checked = this.fTreeBox.TraceSelected;

			this.miTraceKinships.Checked = this.fTreeBox.TraceKinships;
			this.miTraceKinships.Visible = false;
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
                if (this.fNavman != null) this.fNavman.Dispose();
                if (components != null) components.Dispose();
			}
			base.Dispose(disposing);
		}

		private void DoFilter()
		{
			using (TfmTreeFilter dlgFilter = new TfmTreeFilter(this.fBase)) {
				dlgFilter.Filter = this.fTreeBox.Filter;

				if (dlgFilter.ShowDialog() == DialogResult.OK)
				{
					this.GenChart(true);
				}
			}
		}

		private void DoImageSave()
		{
			if (this.SaveDialog1.ShowDialog() == DialogResult.OK) {
				this.fTreeBox.SaveSnapshot(this.SaveDialog1.FileName);
			}
		}

		private void NavRefresh()
		{
			this.tbPrev.Enabled = this.fNavman.CanBackward();
			this.tbNext.Enabled = this.fNavman.CanForward();
		}

		private void NavAdd(GEDCOMIndividualRecord aRec)
		{
			if (aRec != null && !this.fNavman.Busy) {
				this.fNavman.Current = aRec;
				this.NavRefresh();
			}
		}

		private void UpdateModesMenu()
		{
			this.miModeBoth.Checked = false;
			this.miModeAncestors.Checked = false;
			this.miModeDescendants.Checked = false;

			switch (this.fChartKind)
			{
				case TreeChartBox.ChartKind.ckAncestors:
					this.miModeAncestors.Checked = true;
					break;

				case TreeChartBox.ChartKind.ckDescendants:
					this.miModeDescendants.Checked = true;
					break;

				case TreeChartBox.ChartKind.ckBoth:
					this.miModeBoth.Checked = true;
					break;
			}
		}
		
		public static bool CheckData(GEDCOMTree tree, GEDCOMIndividualRecord iRec, TreeChartBox.ChartKind chartKind)
		{
			bool result = true;

			if (chartKind == TreeChartBox.ChartKind.ckAncestors || chartKind == TreeChartBox.ChartKind.ckBoth)
			{
				TreeStats.InitExtCounts(tree, -1);
				int ancCount = TreeStats.GetAncestorsCount(iRec);
				if (ancCount > 2048)
				{
					GKUtils.ShowMessage(string.Format(LangMan.LS(LSID.LSID_AncestorsNumberIsInvalid), ancCount.ToString()));
					return false;
				}
			}

			if (chartKind >= TreeChartBox.ChartKind.ckDescendants && chartKind < (TreeChartBox.ChartKind)3)
			{
				TreeStats.InitExtCounts(tree, -1);
				int descCount = TreeStats.GetDescendantsCount(iRec);
				if (descCount > 2048)
				{
					GKUtils.ShowMessage(string.Format(LangMan.LS(LSID.LSID_DescendantsNumberIsInvalid), descCount.ToString()));
					result = false;
				}
			}

			return result;
		}

		public void GenChart(bool show)
		{
			try
			{
				if (this.fPerson == null)
				{
					GKUtils.ShowError(LangMan.LS(LSID.LSID_NotSelectedPerson));
				}
				else
				{
					this.NavAdd(this.fPerson);
					this.fTreeBox.DepthLimit = this.fGensLimit;
					this.fTreeBox.ShieldState = this.fBase.ShieldState;

					this.fTreeBox.GenChart(this.fPerson, this.fChartKind, true);

					switch (this.fChartKind)
					{
						case TreeChartBox.ChartKind.ckAncestors:
							this.Text = LangMan.LS(LSID.LSID_MITreeAncestors);
							break;
						case TreeChartBox.ChartKind.ckDescendants:
							this.Text = LangMan.LS(LSID.LSID_MITreeDescendants);
							break;
						case TreeChartBox.ChartKind.ckBoth:
							this.Text = LangMan.LS(LSID.LSID_MITreeBoth);
							break;
					}

					this.Text = this.Text + " \"" + Path.GetFileName(fBase.Tree.FileName) + "\"";

					if (show) base.Show();

					TfmGEDKeeper.Instance.UpdateControls(false);
				}
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmChart.GenChart(): " + ex.Message);
			}
		}

		private void UpdateChart()
		{
			if (this.fBase != null) {
				this.fBase.RefreshLists(false);
			}

			this.fTreeBox.RefreshTree();
		}

		private GEDCOMIndividualRecord SelectSpouseFor(GEDCOMIndividualRecord iRec)
		{
		    GEDCOMSex needSex;
			switch (iRec.Sex) {
				case GEDCOMSex.svMale:
					needSex = GEDCOMSex.svFemale;
					break;
				case GEDCOMSex.svFemale:
					needSex = GEDCOMSex.svMale;
					break;
				default:
					GKUtils.ShowError(LangMan.LS(LSID.LSID_IsNotDefinedSex));
					return null;
			}

			GEDCOMIndividualRecord target = null;
			TargetMode targetMode = TargetMode.tmNone;
			if (needSex == GEDCOMSex.svFemale) {
				target = iRec;
				targetMode = TargetMode.tmWife;
			}

			GEDCOMIndividualRecord result = this.fBase.SelectPerson(target, targetMode, needSex);
			return result;
		}

		private void InternalChildAdd(GEDCOMSex needSex)
		{
			TreeChartPerson p = this.fTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				GEDCOMIndividualRecord iRec = p.Rec;

                if (iRec.SpouseToFamilyLinks.Count == 0)
				{
                	GKUtils.ShowError(LangMan.LS(LSID.LSID_IsNotFamilies));
				}
				else
				{
					if (iRec.SpouseToFamilyLinks.Count > 1)
					{
						GKUtils.ShowError("У данной персоны несколько семей. Детей следует добавлять через супругов.");
					}
					else
					{
						GEDCOMFamilyRecord fam = iRec.SpouseToFamilyLinks[0].Family;
						GEDCOMIndividualRecord iChild = this.fBase.SelectPerson(fam.Husband.Value as GEDCOMIndividualRecord, TargetMode.tmParent, needSex);

						if (iChild != null && fam.AddChild(iChild))
						{
							// данный повтор необходим, т.к. этот вызов в CreatePersonDialog срабатывает только,
							// если уже установлен отец, чего до вызова AddChild() - нет;
							// всё это необходимо для того, чтобы в справочник попали корректные отчества.
							TfmGEDKeeper.Instance.NamesTable.ImportNames(iChild);
							
							this.UpdateChart();
						}
					}
				}
			}
		}

		#region Interface handlers
		
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
					this.fTreeBox.RebuildKinships();
					break;

				case Keys.Escape:
					base.Close();
					break;
					
				case Keys.F:
					if (e.Control) {
						this.QuickFind();
					}
					break;
			}
		}

		private void ToolBar1_ButtonClick(object sender, ToolBarButtonClickEventArgs e)
		{
			if (e.Button == this.tbImageSave) {
				this.DoImageSave();
			} else if (e.Button == this.tbPrev) {
				this.NavPrev();
			} else if (e.Button == this.tbNext) {
				this.NavNext();
			} else if (e.Button == this.tbFilter) {
				this.DoFilter();
			}
		}

		private void ImageTree_DragOver(object sender, DragEventArgs e)
		{
			/*if (e.Data.GetDataPresent(typeof(string)))
			{
				e.Effect = DragDropEffects.Move;
			}
			else
			{
				e.Effect = DragDropEffects.None;
			}*/
		}

		private void ImageTree_PersonProperties(object sender, MouseEventArgs e)
		{
			this.MenuPerson.Show(this.fTreeBox, new Point(e.X, e.Y));
		}

		private void ImageTree_RootChanged(object sender, TreeChartPerson person)
		{
			if (person == null || person.Rec == null) return;

			this.fPerson = person.Rec;
		}

		private void ImageTree_PersonModify(object sender, PersonModifyEventArgs eArgs)
		{
			TreeChartPerson person = eArgs.Person;
			if (person == null) return;

			if (person.Rec != null) {
				GEDCOMIndividualRecord iRec = person.Rec;

				if (this.fBase.ModifyPerson(ref iRec)) {
					this.UpdateChart();
				}
			} else {
				// this is "stub" person, only in descendant tree
				// key properties = BaseSpouse & BaseFamily
				TreeChartPerson baseSpouse = person.BaseSpouse;
				GEDCOMFamilyRecord baseFamily = person.BaseFamily;

				if (baseSpouse != null && baseFamily != null) {
					GEDCOMIndividualRecord iSpouse = this.SelectSpouseFor(person.BaseSpouse.Rec);

					if (iSpouse != null) {
						baseFamily.AddSpouse(iSpouse);
						this.UpdateChart();
					}
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

			if (sender == this.miGensInf) this.fGensLimit = -1;
			if (sender == this.miGens1) this.fGensLimit = 1;
			if (sender == this.miGens2) this.fGensLimit = 2;
			if (sender == this.miGens3) this.fGensLimit = 3;
			if (sender == this.miGens4) this.fGensLimit = 4;
			if (sender == this.miGens5) this.fGensLimit = 5;
			if (sender == this.miGens6) this.fGensLimit = 6;
			if (sender == this.miGens7) this.fGensLimit = 7;
			if (sender == this.miGens8) this.fGensLimit = 8;
			if (sender == this.miGens9) this.fGensLimit = 9;

			this.GenChart(true);
		}

		private void miEditClick(object sender, EventArgs e)
		{
			TreeChartPerson p = this.fTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				GEDCOMIndividualRecord iRec = p.Rec;
				if (this.fBase.ModifyPerson(ref iRec)) {
					this.UpdateChart();
				}
			}
		}
		
		private void miSpouseAddClick(object sender, EventArgs e)
		{
			TreeChartPerson p = this.fTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				GEDCOMIndividualRecord iRec = p.Rec;
				GEDCOMIndividualRecord iSpouse = this.SelectSpouseFor(iRec);

				if (iSpouse != null) {
					GEDCOMFamilyRecord fam = this.fTree.CreateFamily();
					fam.AddSpouse(iRec);
					fam.AddSpouse(iSpouse);
					this.UpdateChart();
				}
			}
		}

		private void miSonAddClick(object sender, EventArgs e)
		{
			this.InternalChildAdd(GEDCOMSex.svMale);
		}

		private void miDaughterAddClick(object sender, EventArgs e)
		{
			this.InternalChildAdd(GEDCOMSex.svFemale);
		}

		private void miFamilyAddClick(object sender, EventArgs e)
		{
			TreeChartPerson p = this.fTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				GEDCOMSex sex = p.Rec.Sex;
				if (sex < GEDCOMSex.svMale || sex >= GEDCOMSex.svUndetermined)
				{
					GKUtils.ShowError(LangMan.LS(LSID.LSID_IsNotDefinedSex));
				}
				else
				{
					GEDCOMFamilyRecord fam = this.fTree.CreateFamily();
					fam.AddSpouse(p.Rec);
					this.UpdateChart();
				}
			}
		}

		private void miDeleteClick(object sender, EventArgs e)
		{
			TreeChartPerson p = this.fTreeBox.Selected;
			if (p != null && p.Rec != null && p != this.fTreeBox.Root)
			{
				this.fBase.RecordDelete(p.Rec, true);
				this.GenChart(true);
				this.NavRefresh();
			}
		}

		private void miRebuildKinshipsClick(object sender, EventArgs e)
		{
			this.fTreeBox.RebuildKinships();
		}

		private void miTraceSelected_Click(object sender, EventArgs e)
		{
			this.miTraceSelected.Checked = !this.miTraceSelected.Checked;
			this.fTreeBox.TraceSelected = this.miTraceSelected.Checked;
		}

		private void miTraceKinships_Click(object sender, EventArgs e)
		{
			this.miTraceKinships.Checked = !this.miTraceKinships.Checked;
			this.fTreeBox.TraceKinships = this.miTraceKinships.Checked;
		}

		private void miCertaintyIndex_Click(object sender, EventArgs e)
		{
			this.miCertaintyIndex.Checked = !this.miCertaintyIndex.Checked;
			
			this.fTreeBox.Options.CertaintyIndexVisible = this.miCertaintyIndex.Checked;
			this.fTreeBox.CertaintyIndex = this.miCertaintyIndex.Checked;
		}

		private void miFillColorClick(object sender, EventArgs e)
		{
			if (colorDialog1.ShowDialog() == DialogResult.OK)
			{
				this.fTreeBox.BackgroundImage = null;
				this.fTreeBox.BackColor = colorDialog1.Color;
				this.fTreeBox.Invalidate();
			}
		}

		private void miFillImageClick(object sender, EventArgs e)
		{
			OpenDialog1.InitialDirectory = GKUtils.GetAppPath() + "\\backgrounds";
			if (OpenDialog1.ShowDialog() == DialogResult.OK)
			{
				Image img = new Bitmap(OpenDialog1.FileName);
				this.fTreeBox.BackgroundImage = img;
				this.fTreeBox.BackgroundImageLayout = ImageLayout.Tile;
				this.fTreeBox.Invalidate();
			}
		}

		private void miModeItem_Click(object sender, EventArgs e)
		{
			TreeChartBox.ChartKind newMode = (TreeChartBox.ChartKind)(sender as MenuItem).Tag;

			if (this.fChartKind != newMode)
			{
				this.ChartKind = newMode;
				this.GenChart(true);
			}
		}

		private void miRebuildTreeClick(object sender, EventArgs e)
		{
			try
			{
				TreeChartPerson p = this.fTreeBox.Selected;
				if (p != null && p.Rec != null)
				{
					this.fPerson = p.Rec;
					this.GenChart(true);
					this.NavRefresh();
				}
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmChart.RebuildTree(): " + ex.Message);
			}
		}

		#endregion

		#region ILocalization implementation
		
		public void SetLang()
		{
			this.miGensInf.Text = LangMan.LS(LSID.LSID_Unlimited);
			this.miGensInf.Checked = true;
			this.miModeBoth.Text = LangMan.LS(LSID.LSID_TM_Both);
			this.miModeAncestors.Text = LangMan.LS(LSID.LSID_TM_Ancestors);
			this.miModeDescendants.Text = LangMan.LS(LSID.LSID_TM_Descendants);
			this.miEdit.Text = LangMan.LS(LSID.LSID_DoEdit);
			this.miFamilyAdd.Text = LangMan.LS(LSID.LSID_FamilyAdd);
			this.miSpouseAdd.Text = LangMan.LS(LSID.LSID_SpouseAdd);
			this.miSonAdd.Text = LangMan.LS(LSID.LSID_SonAdd);
			this.miDaughterAdd.Text = LangMan.LS(LSID.LSID_DaughterAdd);
			this.miDelete.Text = LangMan.LS(LSID.LSID_DoDelete);
			this.miRebuildTree.Text = LangMan.LS(LSID.LSID_RebuildTree);
			this.miRebuildKinships.Text = LangMan.LS(LSID.LSID_RebuildKinships);

			this.miFillColor.Text = LangMan.LS(LSID.LSID_FillColor);
			this.miFillImage.Text = LangMan.LS(LSID.LSID_FillImage);

			this.miTraceSelected.Text = LangMan.LS(LSID.LSID_TM_TraceSelected);
			this.miTraceKinships.Text = LangMan.LS(LSID.LSID_TM_TraceKinships);

			this.fTreeBox.ScaleControl.Tip = LangMan.LS(LSID.LSID_Scale);

			this.miCertaintyIndex.Text = LangMan.LS(LSID.LSID_CertaintyIndex);
		}

		#endregion
		
		#region IWorkWindow implementation

		public string GetStatusString()
		{
			return string.Format(LangMan.LS(LSID.LSID_TreeIndividualsCount), fTreeBox.IndividualsCount.ToString());
		}

		public void NavNext()
		{
			this.fNavman.BeginNav();
			try
			{
				this.fPerson = (this.fNavman.Next() as GEDCOMIndividualRecord);
				this.GenChart(true);
				this.NavRefresh();
			}
			finally
			{
				this.fNavman.EndNav();
			}
		}

		public void NavPrev()
		{
			this.fNavman.BeginNav();
			try
			{
				this.fPerson = (this.fNavman.Back() as GEDCOMIndividualRecord);
				this.GenChart(true);
				this.NavRefresh();
			}
			finally
			{
				this.fNavman.EndNav();
			}
		}

		public bool NavCanBackward()
		{
			return this.fNavman.CanBackward();
		}

		public bool NavCanForward()
		{
			return this.fNavman.CanForward();
		}

		public IList<ISearchResult> FindAll(string searchPattern)
		{
			return this.fTreeBox.FindAll(searchPattern);
		}

		public void SelectByRec(GEDCOMIndividualRecord iRec)
		{
            if (iRec == null) {
                throw new ArgumentNullException("iRec");
            }

            this.fTreeBox.SelectByRec(iRec);
		}

		public void QuickFind()
		{
			SearchPanel panel = new SearchPanel(this);
			
			Rectangle client = this.ClientRectangle;
			Point pt = this.PointToScreen(new Point(client.Left, client.Bottom - panel.Height));
			panel.Location = pt;

			panel.Show();
		}

		#endregion
	}
}
