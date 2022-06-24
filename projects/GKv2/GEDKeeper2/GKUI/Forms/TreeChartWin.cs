﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;
using BSLib.Design.Graphics;
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKCore.Options;
using GKUI.Components;
using GKUI.Platform;

namespace GKUI.Forms
{
    public partial class TreeChartWin : PrintableForm, ITreeChartWin
    {
        private readonly TreeChartWinController fController;

        private readonly IBaseWindow fBase;
        private readonly TreeChartBox fTreeBox;

        private GDMIndividualRecord fPerson;

        private ToolStripMenuItem miGensInfCommon;
        private ToolStripMenuItem miGensInfAncestors;
        private ToolStripMenuItem miGensInfDescendants;


        public IBaseWindow Base
        {
            get { return fBase; }
        }

        #region View Interface

        ITreeChart ITreeChartWin.TreeBox
        {
            get { return fTreeBox; }
        }

        #endregion

        public TreeChartWin(IBaseWindow baseWin, GDMIndividualRecord startPerson)
        {
            InitializeComponent();

            tbFilter.Image = UIHelper.LoadResourceImage("Resources.btn_filter.gif");
            tbPrev.Image = UIHelper.LoadResourceImage("Resources.btn_left.gif");
            tbNext.Image = UIHelper.LoadResourceImage("Resources.btn_right.gif");
            tbImageSave.Image = UIHelper.LoadResourceImage("Resources.btn_save_image.gif");
            tbDocPreview.Image = UIHelper.LoadResourceImage("Resources.btn_preview.gif");
            tbDocPrint.Image = UIHelper.LoadResourceImage("Resources.btn_print.gif");
            tbOptions.Image = UIHelper.LoadResourceImage("Resources.btn_tools.gif");

            UIHelper.FixToolStrip(ToolBar1);

            miModeBoth.Tag = TreeChartKind.ckBoth;
            miModeAncestors.Tag = TreeChartKind.ckAncestors;
            miModeDescendants.Tag = TreeChartKind.ckDescendants;

            fBase = baseWin;
            fPerson = startPerson;

            fTreeBox = new TreeChartBox(new WFGfxRenderer());
            fTreeBox.Name = "fTreeBox";
            fTreeBox.Base = fBase;
            fTreeBox.PersonModify += ImageTree_PersonModify;
            fTreeBox.RootChanged += ImageTree_RootChanged;
            fTreeBox.InfoRequest += ImageTree_InfoRequest;
            fTreeBox.PersonProperties += ImageTree_PersonProperties;
            fTreeBox.Options = GlobalOptions.Instance.TreeChartOptions;
            fTreeBox.NavRefresh += ImageTree_NavRefresh;
            fTreeBox.ZoomChanged += ImageTree_NavRefresh;
            fTreeBox.Dock = DockStyle.Fill;
            Controls.Add(fTreeBox);
            Controls.SetChildIndex(fTreeBox, 0);
            Controls.SetChildIndex(ToolBar1, 1);

            PopulateContextMenus();

            miGensInfCommon.Checked = true;
            miGensInfAncestors.Checked = true;
            miGensInfDescendants.Checked = true;

            miCertaintyIndex.Checked = fTreeBox.Options.CertaintyIndexVisible;
            fTreeBox.CertaintyIndex = fTreeBox.Options.CertaintyIndexVisible;

            miTraceSelected.Checked = fTreeBox.Options.TraceSelected;
            fTreeBox.TraceSelected = fTreeBox.Options.TraceSelected;

            miTraceKinships.Checked = fTreeBox.TraceKinships;
            miTraceKinships.Visible = false;

            fController = new TreeChartWinController(this);
            fController.Init(baseWin);

            SetupDepth();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (components != null) components.Dispose();
            }
            base.Dispose(disposing);
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            fTreeBox.Select();
            UpdateControls();
        }

        protected override IPrintable GetPrintable()
        {
            return fTreeBox;
        }

        #region Interface handlers

        private void ToolBar1_ButtonClick(object sender, EventArgs e)
        {
            if (sender == tbFilter) {
                SetFilter();
            } else if (sender == tbPrev) {
                NavPrev();
            } else if (sender == tbNext) {
                NavNext();
            }
        }

        private void UpdateModesMenu()
        {
            miModeBoth.Checked = false;
            miModeAncestors.Checked = false;
            miModeDescendants.Checked = false;

            switch (fTreeBox.Model.Kind) {
                case TreeChartKind.ckAncestors:
                    miModeAncestors.Checked = true;
                    break;

                case TreeChartKind.ckDescendants:
                    miModeDescendants.Checked = true;
                    break;

                case TreeChartKind.ckBoth:
                    miModeBoth.Checked = true;
                    break;
            }
        }

        private void TreeChartWin_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode) {
                case Keys.F5:
                    GenChart();
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

                case Keys.S:
                    if (e.Control) {
                        fBase.SaveFileEx(false);
                    }
                    break;
            }
        }

        private void tbImageSave_Click(object sender, EventArgs e)
        {
            fController.SaveSnapshot();
        }

        private void ImageTree_PersonProperties(object sender, MouseEventArgs e)
        {
            MenuPerson.Show(fTreeBox, new Point(e.X, e.Y));
        }

        private void ImageTree_RootChanged(object sender, TreeChartPerson person)
        {
            if (person != null && person.Rec != null) {
                fPerson = person.Rec;
                UpdateControls();
            }
        }

        private void ImageTree_NavRefresh(object sender, EventArgs e)
        {
            UpdateControls();
        }

        private void ImageTree_PersonModify(object sender, PersonModifyEventArgs eArgs)
        {
            fController.ModifyPerson(eArgs.Person);
        }

        private void ImageTree_InfoRequest(object sender, TreeChartPerson person)
        {
            if (person != null && person.Rec != null) {
                fController.RequestInfo(person);
            }
        }

        private void PopulateContextMenus()
        {
            miGensInfCommon = UIHelper.AddToolStripItem(MenuGensCommon, "Inf", -1, miGensX_Click);
            UIHelper.AddToolStripItem(MenuGensCommon, "1", 1, miGensX_Click);
            UIHelper.AddToolStripItem(MenuGensCommon, "2", 2, miGensX_Click);
            UIHelper.AddToolStripItem(MenuGensCommon, "3", 3, miGensX_Click);
            UIHelper.AddToolStripItem(MenuGensCommon, "4", 4, miGensX_Click);
            UIHelper.AddToolStripItem(MenuGensCommon, "5", 5, miGensX_Click);
            UIHelper.AddToolStripItem(MenuGensCommon, "6", 6, miGensX_Click);
            UIHelper.AddToolStripItem(MenuGensCommon, "7", 7, miGensX_Click);
            UIHelper.AddToolStripItem(MenuGensCommon, "8", 8, miGensX_Click);
            UIHelper.AddToolStripItem(MenuGensCommon, "9", 9, miGensX_Click);

            miGensInfAncestors = UIHelper.AddToolStripItem(MenuGensAncestors, "Inf", -1, miGensXAncestors_Click);
            UIHelper.AddToolStripItem(MenuGensAncestors, "1", 1, miGensXAncestors_Click);
            UIHelper.AddToolStripItem(MenuGensAncestors, "2", 2, miGensXAncestors_Click);
            UIHelper.AddToolStripItem(MenuGensAncestors, "3", 3, miGensXAncestors_Click);
            UIHelper.AddToolStripItem(MenuGensAncestors, "4", 4, miGensXAncestors_Click);
            UIHelper.AddToolStripItem(MenuGensAncestors, "5", 5, miGensXAncestors_Click);
            UIHelper.AddToolStripItem(MenuGensAncestors, "6", 6, miGensXAncestors_Click);
            UIHelper.AddToolStripItem(MenuGensAncestors, "7", 7, miGensXAncestors_Click);
            UIHelper.AddToolStripItem(MenuGensAncestors, "8", 8, miGensXAncestors_Click);
            UIHelper.AddToolStripItem(MenuGensAncestors, "9", 9, miGensXAncestors_Click);

            miGensInfDescendants = UIHelper.AddToolStripItem(MenuGensDescendants, "Inf", -1, miGensXDescendants_Click);
            UIHelper.AddToolStripItem(MenuGensDescendants, "1", 1, miGensXDescendants_Click);
            UIHelper.AddToolStripItem(MenuGensDescendants, "2", 2, miGensXDescendants_Click);
            UIHelper.AddToolStripItem(MenuGensDescendants, "3", 3, miGensXDescendants_Click);
            UIHelper.AddToolStripItem(MenuGensDescendants, "4", 4, miGensXDescendants_Click);
            UIHelper.AddToolStripItem(MenuGensDescendants, "5", 5, miGensXDescendants_Click);
            UIHelper.AddToolStripItem(MenuGensDescendants, "6", 6, miGensXDescendants_Click);
            UIHelper.AddToolStripItem(MenuGensDescendants, "7", 7, miGensXDescendants_Click);
            UIHelper.AddToolStripItem(MenuGensDescendants, "8", 8, miGensXDescendants_Click);
            UIHelper.AddToolStripItem(MenuGensDescendants, "9", 9, miGensXDescendants_Click);

            for (var bs = GfxBorderStyle.None; bs <= GfxBorderStyle.CrossCorners; bs++) {
                UIHelper.AddToolStripItem(MenuBorders, bs.ToString(), (int)bs, miBorderX_Click);
            }
        }

        private void miGensX_Click(object sender, EventArgs e)
        {
            int depth = UIHelper.GetMenuItemTag<int>(MenuGensCommon, sender);
            fTreeBox.DepthLimitAncestors = depth;
            fTreeBox.DepthLimitDescendants = depth;
            GenChart();
        }

        private void miGensXAncestors_Click(object sender, EventArgs e)
        {
            int depth = UIHelper.GetMenuItemTag<int>(MenuGensAncestors, sender);
            fTreeBox.DepthLimitAncestors = depth;
            GenChart();
        }

        private void miGensXDescendants_Click(object sender, EventArgs e)
        {
            int depth = UIHelper.GetMenuItemTag<int>(MenuGensDescendants, sender);
            fTreeBox.DepthLimitDescendants = depth;
            GenChart();
        }

        private void SetupDepth()
        {
            var treeOptions = GlobalOptions.Instance.TreeChartOptions;

            fController.SetupDepth();

            if (!treeOptions.SeparateDepth) {
                UIHelper.SetMenuItemTag(MenuGensCommon, treeOptions.DepthLimit);
            } else {
                UIHelper.SetMenuItemTag(MenuGensAncestors, treeOptions.DepthLimitAncestors);
                UIHelper.SetMenuItemTag(MenuGensDescendants, treeOptions.DepthLimitDescendants);
            }

            UIHelper.SetMenuItemTag(MenuBorders, (int)GlobalOptions.Instance.TreeChartOptions.BorderStyle);
        }

        private void miBorderX_Click(object sender, EventArgs e)
        {
            int borderStyle = UIHelper.GetMenuItemTag<int>(MenuBorders, sender);
            GlobalOptions.Instance.TreeChartOptions.BorderStyle = (GfxBorderStyle)borderStyle;
            fTreeBox.Invalidate();
        }

        private void miEdit_Click(object sender, EventArgs e)
        {
            fController.Edit();
        }

        private void miFatherAdd_Click(object sender, EventArgs e)
        {
            fController.AddFather();
        }

        private void miMotherAdd_Click(object sender, EventArgs e)
        {
            fController.AddMother();
        }

        private void miSpouseAdd_Click(object sender, EventArgs e)
        {
            fController.AddSpouse();
        }

        private void miSonAdd_Click(object sender, EventArgs e)
        {
            fController.AddSon();
        }

        private void miDaughterAdd_Click(object sender, EventArgs e)
        {
            fController.AddDaughter();
        }

        private void miFamilyAdd_Click(object sender, EventArgs e)
        {
            fController.AddFamily();
        }

        private void miDelete_Click(object sender, EventArgs e)
        {
            fController.Delete();
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
            using (var colorDialog1 = new ColorDialog()) {
                if (colorDialog1.ShowDialog() != DialogResult.OK) return;

                fTreeBox.BackgroundImage = null;
                fTreeBox.BackColor = colorDialog1.Color;
                fTreeBox.Invalidate();
            }
        }

        private void miFillImage_Click(object sender, EventArgs e)
        {
            string fileName = AppHost.StdDialogs.GetOpenFile("", GKUtils.GetBackgroundsPath(), LangMan.LS(LSID.LSID_ImagesFilter), 1, "");
            if (string.IsNullOrEmpty(fileName)) return;

            Image img = new Bitmap(fileName);
            fTreeBox.BackgroundImage = img;
            fTreeBox.BackgroundImageLayout = ImageLayout.Tile;
            fTreeBox.Invalidate();
        }

        private void miModeItem_Click(object sender, EventArgs e)
        {
            TreeChartKind newMode = (TreeChartKind)((ToolStripMenuItem)sender).Tag;
            if (fTreeBox.Model.Kind == newMode) return;

            GenChart(newMode);
        }

        private void miRebuildTree_Click(object sender, EventArgs e)
        {
            try {
                TreeChartPerson p = fTreeBox.Selected;
                if (p == null || p.Rec == null) return;

                fPerson = p.Rec;
                GenChart();
            } catch (Exception ex) {
                Logger.WriteError("TreeChartWin.miRebuildTree_Click()", ex);
            }
        }

        private void miSelectColor_Click(object sender, EventArgs e)
        {
            fController.SelectColor();
        }

        private void MenuPerson_Opening(object sender, System.ComponentModel.CancelEventArgs e)
        {
            miFatherAdd.Enabled = fController.ParentIsRequired(GDMSex.svMale);
            miMotherAdd.Enabled = fController.ParentIsRequired(GDMSex.svFemale);
            miGoToRecord.Enabled = fController.SelectedPersonIsReal();

            TreeChartPerson p = fTreeBox.Selected;
            miGoToPrimaryBranch.Enabled = (p != null && p.Rec != null && p.IsDup);
        }

        private void tbDocPreview_Click(object sender, EventArgs e)
        {
            DoPrintPreview();
        }

        private void tbDocPrint_Click(object sender, EventArgs e)
        {
            DoPrint();
        }

        private void tbOptions_Click(object sender, EventArgs e)
        {
            AppHost.Instance.ShowOptions(OptionsPage.opTreeChart);
        }

        private void miGoToRecord_Click(object sender, EventArgs e)
        {
            fController.GoToRecord();
        }

        private void miGoToPrimaryBranch_Click(object sender, EventArgs e)
        {
            fController.GoToPrimaryBranch();
        }

        #endregion

        #region IChartWindow implementation

        public void GenChart()
        {
            GenChart(fTreeBox.Model.Kind);
        }

        public void GenChart(TreeChartKind chartKind)
        {
            try {
                if (fPerson == null) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_NotSelectedPerson));
                } else {
                    fTreeBox.GenChart(fPerson, chartKind, true);
                    UpdateControls();
                }
            } catch (Exception ex) {
                Logger.WriteError("TreeChartWin.GenChart()", ex);
            }
        }

        #endregion

        #region ILocalizable implementation

        public override void SetLocale()
        {
            fController.SetLocale();
        }

        #endregion

        #region IWorkWindow implementation

        public void UpdateControls()
        {
            try {
                fController.UpdateTitle();
                UpdateModesMenu();

                StatusLines[0] = string.Format(LangMan.LS(LSID.LSID_TreeIndividualsCount), fTreeBox.IndividualsCount);
                var imageSize = fTreeBox.GetImageSize();
                StatusLines[1] = string.Format(LangMan.LS(LSID.LSID_ImageSize), imageSize.Width, imageSize.Height);
                StatusLines[2] = string.Format("{0}: {1:f0} %", LangMan.LS(LSID.LSID_Scale), fTreeBox.Scale * 100);

                tbPrev.Enabled = NavCanBackward();
                tbNext.Enabled = NavCanForward();

                AppHost.Instance.UpdateControls(false, true);
            } catch (Exception ex) {
                Logger.WriteError("TreeChartWin.UpdateControls()", ex);
            }
        }

        public void UpdateSettings()
        {
            GenChart();
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
            return fTreeBox.Model.FindAll(searchPattern);
        }

        public void SelectByRec(GDMRecord record)
        {
            GDMIndividualRecord iRec = record as GDMIndividualRecord;
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            fTreeBox.SelectByRec(iRec);
        }

        public void QuickSearch()
        {
            QuickSearchDlg qsDlg = new QuickSearchDlg(this);

            Rectangle client = ClientRectangle;
            qsDlg.Location = PointToScreen(new Point(client.Left, client.Bottom - qsDlg.Height));

            qsDlg.Show();
        }

        public bool AllowFilter()
        {
            return true;
        }

        public void SetFilter()
        {
            fController.SetFilter();
        }

        #endregion
    }
}
