/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Charts;
using GKUI.Dialogs;
using GKUI.Forms;

namespace GKUI
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TreeChartWin : PrintableForm, IChartWindow
    {
        private readonly IBaseWindow fBase;
        private readonly TreeChartBox fTreeBox;

        private TreeChartBox.ChartKind fChartKind;
        private int fGensLimit;
        private GEDCOMIndividualRecord fPerson;


        public IBaseWindow Base
        {
            get { return fBase; }
        }

        public TreeChartBox.ChartKind ChartKind
        {
            get { return fChartKind; }
            set {
                fChartKind = value;
                UpdateModesMenu();
            }
        }


        public TreeChartWin(IBaseWindow baseWin, GEDCOMIndividualRecord startPerson)
        {
            InitializeComponent();
            MdiParent = MainWin.Instance;

            tbImageSave.Image = GKResources.iSaveImage;
            tbModes.Image = GKResources.iTools;

            miModeBoth.Tag = TreeChartBox.ChartKind.ckBoth;
            miModeAncestors.Tag = TreeChartBox.ChartKind.ckAncestors;
            miModeDescendants.Tag = TreeChartBox.ChartKind.ckDescendants;

            fBase = baseWin;
            fPerson = startPerson;
            fGensLimit = -1;

            fTreeBox = new TreeChartBox(new TreeChartGfxRenderer());
            fTreeBox.Name = "fTreeBox";
            fTreeBox.Base = fBase;
            fTreeBox.Dock = DockStyle.Fill;
            fTreeBox.DragOver += ImageTree_DragOver;
            fTreeBox.PersonModify += ImageTree_PersonModify;
            fTreeBox.RootChanged += ImageTree_RootChanged;
            fTreeBox.PersonProperties += ImageTree_PersonProperties;
            fTreeBox.Options = MainWin.Instance.Options.ChartOptions;
            fTreeBox.NavRefresh += ImageTree_NavRefresh;

            Controls.Add(fTreeBox);
            Controls.SetChildIndex(fTreeBox, 0);
            Controls.SetChildIndex(ToolBar1, 1);

            SetLang();

            miCertaintyIndex.Checked = fTreeBox.Options.CertaintyIndexVisible;
            fTreeBox.CertaintyIndex = fTreeBox.Options.CertaintyIndexVisible;

            miTraceSelected.Checked = fTreeBox.Options.TraceSelected;
            fTreeBox.TraceSelected = fTreeBox.Options.TraceSelected;

            miTraceKinships.Checked = fTreeBox.TraceKinships;
            miTraceKinships.Visible = false;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (components != null) components.Dispose();
            }
            base.Dispose(disposing);
        }

        protected override IPrintable GetPrintable()
        {
            return fTreeBox;
        }

        public static bool CheckData(GEDCOMTree tree, GEDCOMIndividualRecord iRec, TreeChartBox.ChartKind chartKind)
        {
            bool result = true;

            if (chartKind == TreeChartBox.ChartKind.ckAncestors || chartKind == TreeChartBox.ChartKind.ckBoth)
            {
                GKUtils.InitExtCounts(tree, -1);
                int ancCount = GKUtils.GetAncestorsCount(iRec);
                if (ancCount > 2048)
                {
                    GKUtils.ShowMessage(string.Format(LangMan.LS(LSID.LSID_AncestorsNumberIsInvalid), ancCount.ToString()));
                    return false;
                }
            }

            if (chartKind >= TreeChartBox.ChartKind.ckDescendants && chartKind < (TreeChartBox.ChartKind)3)
            {
                GKUtils.InitExtCounts(tree, -1);
                int descCount = GKUtils.GetDescendantsCount(iRec);
                if (descCount > 2048)
                {
                    GKUtils.ShowMessage(string.Format(LangMan.LS(LSID.LSID_DescendantsNumberIsInvalid), descCount.ToString()));
                    result = false;
                }
            }

            return result;
        }

        private void UpdateTitle()
        {
            switch (fChartKind)
            {
                case TreeChartBox.ChartKind.ckAncestors:
                    Text = LangMan.LS(LSID.LSID_MITreeAncestors);
                    break;
                case TreeChartBox.ChartKind.ckDescendants:
                    Text = LangMan.LS(LSID.LSID_MITreeDescendants);
                    break;
                case TreeChartBox.ChartKind.ckBoth:
                    Text = LangMan.LS(LSID.LSID_MITreeBoth);
                    break;
            }

            Text = string.Format("{0} \"{1}\"", Text, Path.GetFileName(fBase.Tree.FileName));
        }

        #region Interface handlers

        private void UpdateChart()
        {
            if (fBase != null) {
                fBase.RefreshLists(false);
            }

            fTreeBox.RefreshTree();
        }

        private void UpdateModesMenu()
        {
            miModeBoth.Checked = false;
            miModeAncestors.Checked = false;
            miModeDescendants.Checked = false;

            switch (fChartKind)
            {
                case TreeChartBox.ChartKind.ckAncestors:
                    miModeAncestors.Checked = true;
                    break;

                case TreeChartBox.ChartKind.ckDescendants:
                    miModeDescendants.Checked = true;
                    break;

                case TreeChartBox.ChartKind.ckBoth:
                    miModeBoth.Checked = true;
                    break;
            }
        }

        private void TreeChartWin_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode)
            {
                case Keys.F5:
                    GenChart(true);
                    break;

                case Keys.F6:
                    miRebuildTree_Click(null, null);
                    break;

                case Keys.F7:
                    fTreeBox.RebuildKinships();
                    break;

                case Keys.Escape:
                    Close();
                    break;

                case Keys.F:
                    if (e.Control) {
                        QuickSearch();
                    }
                    break;
            }
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            fTreeBox.Select();
        }

        private void tbImageSave_Click(object sender, EventArgs e)
        {
            string fileName = UIHelper.GetSaveFile("", "", LangMan.LS(LSID.LSID_TreeImagesFilter), 2, "jpg", "");
            if (!string.IsNullOrEmpty(fileName))
            {
                fTreeBox.SaveSnapshot(fileName);
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
            MenuPerson.Show(fTreeBox, new Point(e.X, e.Y));
        }

        private void ImageTree_RootChanged(object sender, TreeChartPerson person)
        {
            if (person == null || person.Rec == null) return;

            fPerson = person.Rec;

            MainWin.Instance.UpdateControls(false);
        }

        private void ImageTree_NavRefresh(object sender, EventArgs e)
        {
            MainWin.Instance.UpdateControls(false);
        }

        private void ImageTree_PersonModify(object sender, PersonModifyEventArgs eArgs)
        {
            TreeChartPerson person = eArgs.Person;
            if (person == null) return;

            if (person.Rec != null) {
                GEDCOMIndividualRecord iRec = person.Rec;

                if (fBase.ModifyPerson(ref iRec, null, TargetMode.tmNone, GEDCOMSex.svNone)) {
                    UpdateChart();
                }
            } else {
                // this is "stub" person, only in descendant tree
                // key properties = BaseSpouse & BaseFamily
                TreeChartPerson baseSpouse = person.BaseSpouse;
                GEDCOMFamilyRecord baseFamily = person.BaseFamily;

                if (baseSpouse != null && baseFamily != null) {
                    GEDCOMIndividualRecord iSpouse = fBase.SelectSpouseFor(person.BaseSpouse.Rec);

                    if (iSpouse != null) {
                        baseFamily.AddSpouse(iSpouse);
                        UpdateChart();
                    }
                }
            }
        }

        private void miGens9_Click(object sender, EventArgs e)
        {
            miGensInf.Checked = false;
            miGens1.Checked = false;
            miGens2.Checked = false;
            miGens3.Checked = false;
            miGens4.Checked = false;
            miGens5.Checked = false;
            miGens6.Checked = false;
            miGens7.Checked = false;
            miGens8.Checked = false;
            miGens9.Checked = false;
            ((ToolStripMenuItem)sender).Checked = true;

            if (sender == miGensInf) fGensLimit = -1;
            if (sender == miGens1) fGensLimit = 1;
            if (sender == miGens2) fGensLimit = 2;
            if (sender == miGens3) fGensLimit = 3;
            if (sender == miGens4) fGensLimit = 4;
            if (sender == miGens5) fGensLimit = 5;
            if (sender == miGens6) fGensLimit = 6;
            if (sender == miGens7) fGensLimit = 7;
            if (sender == miGens8) fGensLimit = 8;
            if (sender == miGens9) fGensLimit = 9;

            GenChart(true);
        }

        private void miEdit_Click(object sender, EventArgs e)
        {
            TreeChartPerson p = fTreeBox.Selected;
            if (p == null || p.Rec == null) return;

            GEDCOMIndividualRecord iRec = p.Rec;
            if (fBase.ModifyPerson(ref iRec, null, TargetMode.tmNone, GEDCOMSex.svNone)) {
                UpdateChart();
            }
        }

        private bool ParentIsRequired(GEDCOMSex needSex)
        {
            TreeChartPerson p = fTreeBox.Selected;
            if (p == null || p.Rec == null) return false;

            bool familyExist = p.Rec.GetParentsFamily() != null;
            if (!familyExist) return true;

            GEDCOMIndividualRecord mother, father;
            p.Rec.GetParents(out father, out mother);

            bool needParent = (father == null && needSex == GEDCOMSex.svMale) ||
                (mother == null && needSex == GEDCOMSex.svFemale);
            return needParent;
        }

        private void ParentAdd(GEDCOMSex needSex)
        {
            TreeChartPerson p = fTreeBox.Selected;
            if (p == null || p.Rec == null) return;

            bool needParent = false;
            bool familyExist = p.Rec.GetParentsFamily() != null;

            if (familyExist) {
                GEDCOMIndividualRecord mother, father;
                p.Rec.GetParents(out father, out mother);
                needParent = (father == null && needSex == GEDCOMSex.svMale) ||
                    (mother == null && needSex == GEDCOMSex.svFemale);
            }

            if (!familyExist || needParent) {
                GEDCOMIndividualRecord child = p.Rec;
                GEDCOMFamilyRecord fam = (familyExist) ? p.Rec.GetParentsFamily() : fBase.Tree.CreateFamily();
                GEDCOMIndividualRecord parent = fBase.SelectPerson(null, TargetMode.tmParent, needSex);
                if (parent != null) {
                    fam.AddSpouse(parent);
                    if (!familyExist)
                        fam.AddChild(child);
                    
                    UpdateChart();
                }
            }
        }

        private void miFatherAdd_Click(object sender, EventArgs e)
        {
            ParentAdd(GEDCOMSex.svMale);
        }

        private void miMotherAdd_Click(object sender, EventArgs e)
        {
            ParentAdd(GEDCOMSex.svFemale);
        }

        private void miSpouseAdd_Click(object sender, EventArgs e)
        {
            TreeChartPerson p = fTreeBox.Selected;
            if (p == null || p.Rec == null) return;

            GEDCOMIndividualRecord iRec = p.Rec;
            GEDCOMIndividualRecord iSpouse = fBase.SelectSpouseFor(iRec);
            if (iSpouse == null) return;

            GEDCOMFamilyRecord fam = fBase.Tree.CreateFamily();
            fam.AddSpouse(iRec);
            fam.AddSpouse(iSpouse);
            UpdateChart();
        }

        private void InternalChildAdd(GEDCOMSex needSex)
        {
            TreeChartPerson p = fTreeBox.Selected;
            if (p == null || p.Rec == null) return;

            GEDCOMIndividualRecord child = fBase.AddChildForParent(p.Rec, needSex);
            if (child == null) return;

            UpdateChart();
        }

        private void miSonAdd_Click(object sender, EventArgs e)
        {
            InternalChildAdd(GEDCOMSex.svMale);
        }

        private void miDaughterAdd_Click(object sender, EventArgs e)
        {
            InternalChildAdd(GEDCOMSex.svFemale);
        }

        private void miFamilyAdd_Click(object sender, EventArgs e)
        {
            TreeChartPerson p = fTreeBox.Selected;
            if (p == null || p.Rec == null) return;

            GEDCOMFamilyRecord fam = fBase.AddFamilyForSpouse(p.Rec);
            if (fam == null) return;

            UpdateChart();
        }

        private void miDelete_Click(object sender, EventArgs e)
        {
            TreeChartPerson p = fTreeBox.Selected;
            if (p == null || p.Rec == null || p == fTreeBox.Root) return;

            fBase.RecordDelete(p.Rec, true);
            GenChart(true);
        }

        private void miRebuildKinships_Click(object sender, EventArgs e)
        {
            fTreeBox.RebuildKinships();
        }

        private void miTraceSelected_Click(object sender, EventArgs e)
        {
            miTraceSelected.Checked = !miTraceSelected.Checked;
            
            fTreeBox.Options.TraceSelected = miTraceSelected.Checked;
            fTreeBox.TraceSelected = miTraceSelected.Checked;
        }

        private void miTraceKinships_Click(object sender, EventArgs e)
        {
            miTraceKinships.Checked = !miTraceKinships.Checked;
            fTreeBox.TraceKinships = miTraceKinships.Checked;
        }

        private void miCertaintyIndex_Click(object sender, EventArgs e)
        {
            miCertaintyIndex.Checked = !miCertaintyIndex.Checked;
            
            fTreeBox.Options.CertaintyIndexVisible = miCertaintyIndex.Checked;
            fTreeBox.CertaintyIndex = miCertaintyIndex.Checked;
        }

        private void miFillColor_Click(object sender, EventArgs e)
        {
            if (colorDialog1.ShowDialog() != DialogResult.OK) return;

            fTreeBox.BackgroundImage = null;
            fTreeBox.BackColor = colorDialog1.Color;
            fTreeBox.Invalidate();
        }

        private void miFillImage_Click(object sender, EventArgs e)
        {
            string fileName = UIHelper.GetOpenFile("", GKUtils.GetBackgroundsPath(), LangMan.LS(LSID.LSID_ImagesFilter), 1, "");
            if (string.IsNullOrEmpty(fileName)) return;

            Image img = new Bitmap(fileName);
            fTreeBox.BackgroundImage = img;
            fTreeBox.BackgroundImageLayout = ImageLayout.Tile;
            fTreeBox.Invalidate();
        }

        private void miModeItem_Click(object sender, EventArgs e)
        {
            TreeChartBox.ChartKind newMode = (TreeChartBox.ChartKind)((ToolStripMenuItem)sender).Tag;
            if (fChartKind == newMode) return;

            ChartKind = newMode;
            GenChart(true);
        }

        private void miRebuildTree_Click(object sender, EventArgs e)
        {
            try
            {
                TreeChartPerson p = fTreeBox.Selected;
                if (p == null || p.Rec == null) return;

                fPerson = p.Rec;
                GenChart(true);
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("TreeChartWin.miRebuildTree_Click(): " + ex.Message);
            }
        }

        private void MenuPerson_Opening(object sender, System.ComponentModel.CancelEventArgs e)
        {
            miFatherAdd.Enabled = ParentIsRequired(GEDCOMSex.svMale);
            miMotherAdd.Enabled = ParentIsRequired(GEDCOMSex.svFemale);
        }

        #endregion

        #region IChartWindow implementation

        public void GenChart(bool show)
        {
            try
            {
                if (fPerson == null)
                {
                    GKUtils.ShowError(LangMan.LS(LSID.LSID_NotSelectedPerson));
                }
                else
                {
                    UpdateTitle();
                    if (show) base.Show();

                    fTreeBox.DepthLimit = fGensLimit;
                    fTreeBox.ShieldState = fBase.ShieldState;
                    fTreeBox.GenChart(fPerson, fChartKind, true);

                    MainWin.Instance.UpdateControls(false);
                }
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("TreeChartWin.GenChart(): " + ex.Message);
            }
        }

        #endregion

        #region ILocalization implementation
        
        public void SetLang()
        {
            tbGens.Text = LangMan.LS(LSID.LSID_Generations);

            miGensInf.Text = LangMan.LS(LSID.LSID_Unlimited);
            miGensInf.Checked = true;
            miModeBoth.Text = LangMan.LS(LSID.LSID_TM_Both);
            miModeAncestors.Text = LangMan.LS(LSID.LSID_TM_Ancestors);
            miModeDescendants.Text = LangMan.LS(LSID.LSID_TM_Descendants);
            miEdit.Text = LangMan.LS(LSID.LSID_DoEdit);
            miFatherAdd.Text = LangMan.LS(LSID.LSID_FatherAdd);
            miMotherAdd.Text = LangMan.LS(LSID.LSID_MotherAdd);
            miFamilyAdd.Text = LangMan.LS(LSID.LSID_FamilyAdd);
            miSpouseAdd.Text = LangMan.LS(LSID.LSID_SpouseAdd);
            miSonAdd.Text = LangMan.LS(LSID.LSID_SonAdd);
            miDaughterAdd.Text = LangMan.LS(LSID.LSID_DaughterAdd);
            miDelete.Text = LangMan.LS(LSID.LSID_DoDelete);
            miRebuildTree.Text = LangMan.LS(LSID.LSID_RebuildTree);
            miRebuildKinships.Text = LangMan.LS(LSID.LSID_RebuildKinships);
            miFillColor.Text = LangMan.LS(LSID.LSID_FillColor);
            miFillImage.Text = LangMan.LS(LSID.LSID_FillImage);
            miTraceSelected.Text = LangMan.LS(LSID.LSID_TM_TraceSelected);
            miTraceKinships.Text = LangMan.LS(LSID.LSID_TM_TraceKinships);
            miCertaintyIndex.Text = LangMan.LS(LSID.LSID_CertaintyIndex);

            tbImageSave.ToolTipText = LangMan.LS(LSID.LSID_ImageSaveTip);
            tbModes.ToolTipText = LangMan.LS(LSID.LSID_ModesTip);
        }

        #endregion
        
        #region IWorkWindow implementation

        public string GetStatusString()
        {
            return string.Format(LangMan.LS(LSID.LSID_TreeIndividualsCount), fTreeBox.IndividualsCount.ToString());
        }

        public void UpdateView()
        {
            GenChart(false);
        }

        public void NavNext()
        {
            fTreeBox.NavNext();
        }

        public void NavPrev()
        {
            fTreeBox.NavPrev();
        }

        public bool NavCanBackward()
        {
            return fTreeBox.NavCanBackward();
        }

        public bool NavCanForward()
        {
            return fTreeBox.NavCanForward();
        }

        public bool AllowQuickSearch()
        {
            return true;
        }

        public IList<ISearchResult> FindAll(string searchPattern)
        {
            return fTreeBox.FindAll(searchPattern);
        }

        public void SelectByRec(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            fTreeBox.SelectByRec(iRec);
        }

        public void QuickSearch()
        {
            QuickSearchDlg qsDlg = new QuickSearchDlg(this);
            
            Rectangle client = ClientRectangle;
            Point pt = PointToScreen(new Point(client.Left, client.Bottom - qsDlg.Height));
            qsDlg.Location = pt;

            qsDlg.Show();
        }

        public bool AllowFilter()
        {
            return true;
        }

        public void SetFilter()
        {
            using (TreeFilterDlg dlgFilter = new TreeFilterDlg(fBase)) {
                dlgFilter.Filter = fTreeBox.Filter;

                if (dlgFilter.ShowDialog() == DialogResult.OK)
                {
                    GenChart(true);
                }
            }
        }

        #endregion
    }
}
