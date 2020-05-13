﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using BSLib.Design.Graphics;
using Eto.Drawing;
using Eto.Forms;
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKCore.Options;
using GKUI.Components;
using GKUI.Providers;

namespace GKUI.Forms
{
    public partial class TreeChartWin : PrintableForm, ITreeChartWin
    {
        private readonly TreeChartWinController fController;

        private readonly IBaseWindow fBase;
        private readonly TreeChartBox fTreeBox;

        private GDMIndividualRecord fPerson;


        public IBaseWindow Base
        {
            get { return fBase; }
        }

        #region View Interface

        ITreeChartBox ITreeChartWin.TreeBox
        {
            get { return fTreeBox; }
        }

        #endregion

        public TreeChartWin(IBaseWindow baseWin, GDMIndividualRecord startPerson)
        {
            InitializeComponent();

            tbModes.Image = UIHelper.LoadResourceImage("Resources.btn_tools.gif");
            tbFilter.Image = UIHelper.LoadResourceImage("Resources.btn_filter.gif");
            tbPrev.Image = UIHelper.LoadResourceImage("Resources.btn_left.gif");
            tbNext.Image = UIHelper.LoadResourceImage("Resources.btn_right.gif");
            tbImageSave.Image = UIHelper.LoadResourceImage("Resources.btn_save_image.gif");

            tbDocPreview.Image = UIHelper.LoadResourceImage("Resources.btn_preview.gif");
            tbDocPrint.Image = UIHelper.LoadResourceImage("Resources.btn_print.gif");
            tbOptions.Image = UIHelper.LoadResourceImage("Resources.btn_tools.gif");

            miModeBoth.Tag = TreeChartKind.ckBoth;
            miModeAncestors.Tag = TreeChartKind.ckAncestors;
            miModeDescendants.Tag = TreeChartKind.ckDescendants;

            fBase = baseWin;
            fPerson = startPerson;

            fTreeBox = new TreeChartBox(new EtoGfxRenderer());
            fTreeBox.Base = fBase;
            //fTreeBox.DragOver += ImageTree_DragOver;
            fTreeBox.PersonModify += ImageTree_PersonModify;
            fTreeBox.RootChanged += ImageTree_RootChanged;
            fTreeBox.InfoRequest += ImageTree_InfoRequest;
            fTreeBox.PersonProperties += ImageTree_PersonProperties;
            fTreeBox.Options = GlobalOptions.Instance.TreeChartOptions;
            fTreeBox.NavRefresh += ImageTree_NavRefresh;
            fTreeBox.ZoomChanged += ImageTree_NavRefresh;
            Content = fTreeBox;

            SetLang();

            miCertaintyIndex.Checked = fTreeBox.Options.CertaintyIndexVisible;
            fTreeBox.CertaintyIndex = fTreeBox.Options.CertaintyIndexVisible;

            miTraceSelected.Checked = fTreeBox.Options.TraceSelected;
            fTreeBox.TraceSelected = fTreeBox.Options.TraceSelected;

            miTraceKinships.Checked = fTreeBox.TraceKinships;
            miTraceKinships.Enabled = false;

            fController = new TreeChartWinController(this);
            fController.Init(baseWin);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
            }
            base.Dispose(disposing);
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            fTreeBox.Focus();
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
            switch (e.Key) {
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

        // FIXME: Eto restriction
        /*private void ImageTree_DragOver(object sender, DragEventArgs e)
        {
            if (e.Data.GetDataPresent(typeof(string))) {
				e.Effect = DragDropEffects.Move;
			} else {
				e.Effect = DragDropEffects.None;
			}
        }*/

        private void ImageTree_PersonProperties(object sender, EventArgs e)
        {
            MenuPerson.Show(fTreeBox /*, new Point(e.X, e.Y)*/); // FIXME: GKv3 DevRestr
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
            ((RadioMenuItem)sender).Checked = true;

            int depth = -1;
            if (sender == miGensInf) depth = -1;
            if (sender == miGens1) depth = 1;
            if (sender == miGens2) depth = 2;
            if (sender == miGens3) depth = 3;
            if (sender == miGens4) depth = 4;
            if (sender == miGens5) depth = 5;
            if (sender == miGens6) depth = 6;
            if (sender == miGens7) depth = 7;
            if (sender == miGens8) depth = 8;
            if (sender == miGens9) depth = 9;
            fTreeBox.DepthLimit = depth;

            GenChart();
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
            fTreeBox.Options.TraceSelected = miTraceSelected.Checked;
            fTreeBox.TraceSelected = miTraceSelected.Checked;
        }

        private void miTraceKinships_Click(object sender, EventArgs e)
        {
            fTreeBox.TraceKinships = miTraceKinships.Checked;
        }

        private void miCertaintyIndex_Click(object sender, EventArgs e)
        {
            fTreeBox.Options.CertaintyIndexVisible = miCertaintyIndex.Checked;
            fTreeBox.CertaintyIndex = miCertaintyIndex.Checked;
        }

        private void miFillColor_Click(object sender, EventArgs e)
        {
            using (var colorDialog1 = new ColorDialog()) {
                if (colorDialog1.ShowDialog(this) != DialogResult.Ok) return;

                //fTreeBox.BackgroundImage = null;
                fTreeBox.BackgroundColor = colorDialog1.Color;
                fTreeBox.Invalidate();
            }
        }

        private void miFillImage_Click(object sender, EventArgs e)
        {
            /*string fileName = AppHost.StdDialogs.GetOpenFile("", GKUtils.GetBackgroundsPath(), LangMan.LS(LSID.LSID_ImagesFilter), 1, "");
            if (string.IsNullOrEmpty(fileName)) return;

            Image img = new Bitmap(fileName);
            fTreeBox.BackgroundImage = img;
            fTreeBox.BackgroundImageLayout = ImageLayout.Tile;
            fTreeBox.Invalidate();*/
        }

        private void miModeItem_Click(object sender, EventArgs e)
        {
            TreeChartKind newMode = (TreeChartKind)((RadioMenuItem)sender).Tag;
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
                Logger.LogWrite("TreeChartWin.miRebuildTree_Click(): " + ex.Message);
            }
        }

        private void miSelectColor_Click(object sender, EventArgs e)
        {
            fController.SelectColor();
        }

        private void MenuPerson_Opening(object sender, EventArgs e)
        {
            miFatherAdd.Enabled = fController.ParentIsRequired(GDMSex.svMale);
            miMotherAdd.Enabled = fController.ParentIsRequired(GDMSex.svFemale);
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
                Logger.LogWrite("TreeChartWin.GenChart(): " + ex.Message);
            }
        }

        #endregion

        #region ILocalization implementation

        public override void SetLang()
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
            miSelectColor.Text = LangMan.LS(LSID.LSID_SelectColor);

            SetToolTip(tbModes, LangMan.LS(LSID.LSID_ModesTip));
            SetToolTip(tbImageSave, LangMan.LS(LSID.LSID_ImageSaveTip));
            SetToolTip(tbDocPrint, LangMan.LS(LSID.LSID_DocPrint));
            SetToolTip(tbDocPreview, LangMan.LS(LSID.LSID_DocPreview));
            SetToolTip(tbPrev, LangMan.LS(LSID.LSID_PrevRec));
            SetToolTip(tbNext, LangMan.LS(LSID.LSID_NextRec));
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

                tbPrev.Enabled = NavCanBackward();
                tbNext.Enabled = NavCanForward();

                AppHost.Instance.UpdateControls(false, true);
            } catch (Exception ex) {
                Logger.LogWrite("TreeChartWin.UpdateControls(): " + ex.Message);
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

            Rectangle client = Bounds;
            qsDlg.Location = new Point(client.Left, client.Bottom - qsDlg.Height);

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
