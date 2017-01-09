/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

//define DEBUG_PRINT

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Printing;
using System.IO;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Charts;
using GKUI.Controls;
using GKUI.Dialogs;

namespace GKUI
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TreeChartWin : Form, IChartWindow
    {
        private readonly IBaseWindow fBase;
        private readonly TreeChartBox fTreeBox;
        private PrintDocument fPrintDoc;

        private TreeChartBox.ChartKind fChartKind;
        private int fGensLimit;
        private GEDCOMIndividualRecord fPerson;

        public IBaseWindow Base
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

        public TreeChartWin(IBaseWindow aBase, GEDCOMIndividualRecord startPerson)
        {
            this.InitializeComponent();
            base.MdiParent = MainWin.Instance;

            this.tbImageSave.Image = GKResources.iSaveImage;
            this.tbModes.Image = GKResources.iTools;

            this.miModeBoth.Tag = TreeChartBox.ChartKind.ckBoth;
            this.miModeAncestors.Tag = TreeChartBox.ChartKind.ckAncestors;
            this.miModeDescendants.Tag = TreeChartBox.ChartKind.ckDescendants;

            this.fBase = aBase;
            this.fPerson = startPerson;

            this.fTreeBox = new TreeChartBox();
            this.fTreeBox.Base = this.fBase;
            this.fTreeBox.Dock = DockStyle.Fill;
            this.fTreeBox.DragOver += this.ImageTree_DragOver;
            this.fTreeBox.PersonModify += this.ImageTree_PersonModify;
            this.fTreeBox.RootChanged += ImageTree_RootChanged;
            this.fTreeBox.PersonProperties += this.ImageTree_PersonProperties;
            this.fTreeBox.Options = MainWin.Instance.Options.ChartOptions;
            this.fTreeBox.NavRefresh += this.ImageTree_NavRefresh;

            base.Controls.Add(this.fTreeBox);
            base.Controls.SetChildIndex(this.fTreeBox, 0);
            base.Controls.SetChildIndex(this.ToolBar1, 1);

            this.fGensLimit = -1;
            this.SetLang();

            this.miCertaintyIndex.Checked = this.fTreeBox.Options.CertaintyIndexVisible;
            this.fTreeBox.CertaintyIndex = this.fTreeBox.Options.CertaintyIndexVisible;

            this.miTraceSelected.Checked = this.fTreeBox.Options.TraceSelected;
            this.fTreeBox.TraceSelected = this.fTreeBox.Options.TraceSelected;

            this.miTraceKinships.Checked = this.fTreeBox.TraceKinships;
            this.miTraceKinships.Visible = false;
            
            this.InitPrintDoc();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (components != null) components.Dispose();
            }
            base.Dispose(disposing);
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

            this.Text = string.Format("{0} \"{1}\"", this.Text, Path.GetFileName(fBase.Tree.FileName));
        }

        #region Interface handlers

        private void UpdateChart()
        {
            if (this.fBase != null) {
                this.fBase.RefreshLists(false);
            }

            this.fTreeBox.RefreshTree();
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

        private void TreeChartWin_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode)
            {
                case Keys.F5:
                    this.GenChart(true);
                    break;

                case Keys.F6:
                    this.miRebuildTree_Click(null, null);
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

        private void tbImageSave_Click(object sender, EventArgs e)
        {
            string fileName = UIHelper.GetSaveFile("", "", LangMan.LS(LSID.LSID_TreeImagesFilter), 2, "jpg", "");
            if (!string.IsNullOrEmpty(fileName))
            {
                this.fTreeBox.SaveSnapshot(fileName);
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

                if (this.fBase.ModifyPerson(ref iRec, null, TargetMode.tmNone, GEDCOMSex.svNone)) {
                    this.UpdateChart();
                }
            } else {
                // this is "stub" person, only in descendant tree
                // key properties = BaseSpouse & BaseFamily
                TreeChartPerson baseSpouse = person.BaseSpouse;
                GEDCOMFamilyRecord baseFamily = person.BaseFamily;

                if (baseSpouse != null && baseFamily != null) {
                    GEDCOMIndividualRecord iSpouse = this.fBase.SelectSpouseFor(person.BaseSpouse.Rec);

                    if (iSpouse != null) {
                        baseFamily.AddSpouse(iSpouse);
                        this.UpdateChart();
                    }
                }
            }
        }

        private void miGens9_Click(object sender, EventArgs e)
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
            ((ToolStripMenuItem)sender).Checked = true;

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

        private void miEdit_Click(object sender, EventArgs e)
        {
            TreeChartPerson p = this.fTreeBox.Selected;
            if (p != null && p.Rec != null)
            {
                GEDCOMIndividualRecord iRec = p.Rec;
                if (this.fBase.ModifyPerson(ref iRec, null, TargetMode.tmNone, GEDCOMSex.svNone)) {
                    this.UpdateChart();
                }
            }
        }

        private bool ParentIsRequired(GEDCOMSex needSex)
        {
            TreeChartPerson p = this.fTreeBox.Selected;

            if (p != null && p.Rec != null)
            {
                bool needParent = false;
                bool familyExist = p.Rec.GetParentsFamily() != null;

                if (familyExist) {
                    GEDCOMIndividualRecord mother, father;
                    p.Rec.GetParents(out father, out mother);

                    needParent = (father == null && needSex == GEDCOMSex.svMale) ||
                        (mother == null && needSex == GEDCOMSex.svFemale);

                    return needParent;
                }

                return true;
            }

            return false;
        }

        private void ParentAdd(GEDCOMSex needSex)
        {
            TreeChartPerson p = this.fTreeBox.Selected;

            if (p != null && p.Rec != null)
            {
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
                    GEDCOMFamilyRecord fam = (familyExist) ? p.Rec.GetParentsFamily() : this.fBase.Tree.CreateFamily();
                    GEDCOMIndividualRecord parent = this.fBase.SelectPerson(null, TargetMode.tmParent, needSex);
                    if (parent != null) {
                        fam.AddSpouse(parent);
                        if (!familyExist)
                            fam.AddChild(child);
                        
                        this.UpdateChart();
                    }
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
            TreeChartPerson p = this.fTreeBox.Selected;
            if (p != null && p.Rec != null)
            {
                GEDCOMIndividualRecord iRec = p.Rec;
                GEDCOMIndividualRecord iSpouse = this.fBase.SelectSpouseFor(iRec);

                if (iSpouse != null) {
                    GEDCOMFamilyRecord fam = this.fBase.Tree.CreateFamily();
                    fam.AddSpouse(iRec);
                    fam.AddSpouse(iSpouse);
                    this.UpdateChart();
                }
            }
        }

        private void InternalChildAdd(GEDCOMSex needSex)
        {
            TreeChartPerson p = this.fTreeBox.Selected;
            if (p != null && p.Rec != null) {
                GEDCOMIndividualRecord child = this.fBase.AddChildForParent(p.Rec, needSex);

                if (child != null) {
                    this.UpdateChart();
                }
            }
        }

        private void miSonAdd_Click(object sender, EventArgs e)
        {
            this.InternalChildAdd(GEDCOMSex.svMale);
        }

        private void miDaughterAdd_Click(object sender, EventArgs e)
        {
            this.InternalChildAdd(GEDCOMSex.svFemale);
        }

        private void miFamilyAdd_Click(object sender, EventArgs e)
        {
            TreeChartPerson p = this.fTreeBox.Selected;
            if (p != null && p.Rec != null)
            {
                GEDCOMFamilyRecord fam = this.fBase.AddFamilyForSpouse(p.Rec);

                if (fam != null) {
                    this.UpdateChart();
                }
            }
        }

        private void miDelete_Click(object sender, EventArgs e)
        {
            TreeChartPerson p = this.fTreeBox.Selected;
            if (p != null && p.Rec != null && p != this.fTreeBox.Root)
            {
                this.fBase.RecordDelete(p.Rec, true);
                this.GenChart(true);
            }
        }

        private void miRebuildKinships_Click(object sender, EventArgs e)
        {
            this.fTreeBox.RebuildKinships();
        }

        private void miTraceSelected_Click(object sender, EventArgs e)
        {
            this.miTraceSelected.Checked = !this.miTraceSelected.Checked;
            
            this.fTreeBox.Options.TraceSelected = this.miTraceSelected.Checked;
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

        private void miFillColor_Click(object sender, EventArgs e)
        {
            if (colorDialog1.ShowDialog() == DialogResult.OK)
            {
                this.fTreeBox.BackgroundImage = null;
                this.fTreeBox.BackColor = colorDialog1.Color;
                this.fTreeBox.Invalidate();
            }
        }

        private void miFillImage_Click(object sender, EventArgs e)
        {
            string fileName = UIHelper.GetOpenFile("", GKUtils.GetBackgroundsPath(), LangMan.LS(LSID.LSID_ImagesFilter), 1, "");
            if (!string.IsNullOrEmpty(fileName))
            {
                Image img = new Bitmap(fileName);
                this.fTreeBox.BackgroundImage = img;
                this.fTreeBox.BackgroundImageLayout = ImageLayout.Tile;
                this.fTreeBox.Invalidate();
            }
        }

        private void miModeItem_Click(object sender, EventArgs e)
        {
            TreeChartBox.ChartKind newMode = (TreeChartBox.ChartKind)((ToolStripMenuItem)sender).Tag;
            if (this.fChartKind == newMode) return;

            this.ChartKind = newMode;
            this.GenChart(true);
        }

        private void miRebuildTree_Click(object sender, EventArgs e)
        {
            try
            {
                TreeChartPerson p = this.fTreeBox.Selected;
                if (p != null && p.Rec != null)
                {
                    this.fPerson = p.Rec;
                    this.GenChart(true);
                }
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TreeChartWin.miRebuildTree_Click(): " + ex.Message);
            }
        }

        private void MenuPerson_Opening(object sender, System.ComponentModel.CancelEventArgs e)
        {
            miFatherAdd.Enabled = ParentIsRequired(GEDCOMSex.svMale);
            miMotherAdd.Enabled = ParentIsRequired(GEDCOMSex.svFemale);
        }

        #endregion

        #region Print support

        private void InitPrintDoc()
        {
            this.fPrintDoc = new PrintDocument();
            this.fPrintDoc.QueryPageSettings += printDocument1_QueryPageSettings;
            this.fPrintDoc.BeginPrint += printDocument1_BeginPrint;
            this.fPrintDoc.PrintPage += printDocument1_PrintPage;
        }

        private void InitCurDoc()
        {
            this.fPrintDoc.DocumentName = this.Text;
            this.fPrintDoc.DefaultPageSettings.Landscape = this.fTreeBox.IsLandscape();
            this.fPrintDoc.DefaultPageSettings.Margins = new Margins(25, 25, 25, 25);
        }

        private void printDocument1_BeginPrint(object sender, PrintEventArgs e)
        {
        }

        private void printDocument1_QueryPageSettings(object sender, QueryPageSettingsEventArgs e)
        {
            e.PageSettings.Landscape = this.fTreeBox.IsLandscape();
            e.PageSettings.Margins = new Margins(25, 25, 25, 25);
        }

        private void printDocument1_PrintPage(object sender, PrintPageEventArgs e)
        {
            Graphics gfx = e.Graphics;

            //PrinterBounds objBounds = new PrinterBounds(e);
            //Rectangle realMarginBounds = objBounds.Bounds;  // Get the REAL Margin Bounds !

            Rectangle marginBounds = e.MarginBounds;
            Rectangle pageBounds = e.PageBounds;

            #if DEBUG_PRINT
            gfx.DrawRectangle(Pens.Gray, marginBounds);
            #endif

            Image img = this.fTreeBox.GetPrintableImage();

            int imgW = img.Width;
            int imgH = img.Height;
            float factor = SysUtils.ZoomToFit(imgW, imgH, marginBounds.Width, marginBounds.Height);
            imgW = (int)(imgW * factor);
            imgH = (int)(imgH * factor);
            int x = (pageBounds.Width - imgW) / 2;
            int y = (pageBounds.Height - imgH) / 2;

            gfx.DrawImage(img, x, y, imgW, imgH);

            e.HasMorePages = false;
        }

        #endregion

        #region IChartWindow implementation

        public bool AllowPrint()
        {
            return true;
        }

        public void DoPrint()
        {
            this.InitCurDoc();

            using (PrintDialog printDlg = new PrintDialog()) {
                printDlg.Document = this.fPrintDoc;

                if (printDlg.ShowDialog() == DialogResult.OK) {
                    this.fPrintDoc.PrinterSettings = printDlg.PrinterSettings;
                    this.fPrintDoc.Print();
                }
            }
        }

        public void DoPrintPreview()
        {
            this.InitCurDoc();

            using (PrintPreviewDialog previewDlg = new PrintPreviewDialog()) {
                previewDlg.WindowState = FormWindowState.Maximized;
                previewDlg.Document = this.fPrintDoc;
                previewDlg.ShowDialog();
            }
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
                    this.UpdateTitle();
                    if (show) base.Show();

                    this.fTreeBox.DepthLimit = this.fGensLimit;
                    this.fTreeBox.ShieldState = this.fBase.ShieldState;
                    this.fTreeBox.GenChart(this.fPerson, this.fChartKind, true);

                    MainWin.Instance.UpdateControls(false);
                }
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TreeChartWin.GenChart(): " + ex.Message);
            }
        }

        #endregion

        #region ILocalization implementation
        
        public void SetLang()
        {
            this.tbGens.Text = LangMan.LS(LSID.LSID_Generations);

            this.miGensInf.Text = LangMan.LS(LSID.LSID_Unlimited);
            this.miGensInf.Checked = true;
            this.miModeBoth.Text = LangMan.LS(LSID.LSID_TM_Both);
            this.miModeAncestors.Text = LangMan.LS(LSID.LSID_TM_Ancestors);
            this.miModeDescendants.Text = LangMan.LS(LSID.LSID_TM_Descendants);
            this.miEdit.Text = LangMan.LS(LSID.LSID_DoEdit);
            this.miFatherAdd.Text = LangMan.LS(LSID.LSID_FatherAdd);
            this.miMotherAdd.Text = LangMan.LS(LSID.LSID_MotherAdd);
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
            this.miCertaintyIndex.Text = LangMan.LS(LSID.LSID_CertaintyIndex);

            this.fTreeBox.ScaleControl.Tip = LangMan.LS(LSID.LSID_Scale);

            this.tbImageSave.ToolTipText = LangMan.LS(LSID.LSID_ImageSaveTip);
            this.tbModes.ToolTipText = LangMan.LS(LSID.LSID_ModesTip);
        }

        #endregion
        
        #region IWorkWindow implementation

        public string GetStatusString()
        {
            return string.Format(LangMan.LS(LSID.LSID_TreeIndividualsCount), fTreeBox.IndividualsCount.ToString());
        }

        public void UpdateView()
        {
            this.GenChart(false);
        }

        public void NavNext()
        {
            this.fTreeBox.NavNext();
        }

        public void NavPrev()
        {
            this.fTreeBox.NavPrev();
        }

        public bool NavCanBackward()
        {
            return this.fTreeBox.NavCanBackward();
        }

        public bool NavCanForward()
        {
            return this.fTreeBox.NavCanForward();
        }

        public bool AllowQuickFind()
        {
            return true;
        }

        public IList<ISearchResult> FindAll(string searchPattern)
        {
            return this.fTreeBox.FindAll(searchPattern);
        }

        public void SelectByRec(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

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

        public bool AllowFilter()
        {
            return true;
        }

        public void SetFilter()
        {
            using (TreeFilterDlg dlgFilter = new TreeFilterDlg(this.fBase)) {
                dlgFilter.Filter = this.fTreeBox.Filter;

                if (dlgFilter.ShowDialog() == DialogResult.OK)
                {
                    this.GenChart(true);
                }
            }
        }

        #endregion
    }
}
