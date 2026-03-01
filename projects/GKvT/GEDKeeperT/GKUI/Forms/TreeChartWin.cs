/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Options;
using GKCore.Search;
using GKUI.Components;
using GKUI.Platform;
using Terminal.Gui;

namespace GKUI.Forms
{
    public partial class TreeChartWin : PrintableForm, ITreeChartWin
    {
        #region Design components

        private readonly TreeChartBox fTreeBox;
        private ToolStripMenuItem miGensInfCommon;
        private ToolStripMenuItem miGensInfAncestors;
        private ToolStripMenuItem miGensInfDescendants;

        #endregion

        private readonly TreeChartWinController fController;

        private GDMIndividualRecord fPerson;


        public IWindow OwnerWindow
        {
            get { return fController.Base; }
        }

        #region View Interface

        ITreeChart ITreeChartWin.TreeBox
        {
            get { return fTreeBox; }
        }

        #endregion

        public TreeChartWin(IBaseWindow baseWin)
        {
            InitializeComponent();

            //miModeBoth.Tag = TreeChartKind.ckBoth;
            //miModeAncestors.Tag = TreeChartKind.ckAncestors;
            //miModeDescendants.Tag = TreeChartKind.ckDescendants;

            fTreeBox = new TreeChartBox(new TGGfxRenderer());
            fTreeBox.Base = baseWin;
            fTreeBox.PersonModify += ImageTree_PersonModify;
            fTreeBox.RootChanged += ImageTree_RootChanged;
            fTreeBox.InfoRequest += ImageTree_InfoRequest;
            fTreeBox.PersonProperties += ImageTree_PersonProperties;
            fTreeBox.Options = GlobalOptions.Instance.TreeChartOptions;
            fTreeBox.NavRefresh += ImageTree_NavRefresh;
            fTreeBox.ZoomChanged += ImageTree_NavRefresh;
            fTreeBox.X = 0;
            fTreeBox.Y = 2; // Leave one row for the toplevel menu
            fTreeBox.Width = Dim.Fill();
            fTreeBox.Height = Dim.Fill();
            Add(fTreeBox);

            PopulateContextMenus();

            miGensInfCommon.Checked = true;
            miGensInfAncestors.Checked = true;
            miGensInfDescendants.Checked = true;

            miCertaintyIndex.Checked = fTreeBox.Options.CertaintyIndexVisible;
            fTreeBox.CertaintyIndex = fTreeBox.Options.CertaintyIndexVisible;

            miXRefVisible.Checked = fTreeBox.Options.XRefVisible;
            fTreeBox.XRefVisible = fTreeBox.Options.XRefVisible;

            miTrackSelectedLines.Checked = fTreeBox.Options.TrackSelectedLines;

            miTrackMatchedSources.Checked = fTreeBox.Options.TrackMatchedSources;

            miParentAges.Checked = fTreeBox.Options.ParentAges;

            miTraceSelected.Checked = fTreeBox.Options.TraceSelected;
            fTreeBox.TraceSelected = fTreeBox.Options.TraceSelected;

            miTraceKinships.Checked = fTreeBox.TraceKinships;
            //miTraceKinships.Visible = false;

            miHideDescSpouses.Checked = fTreeBox.Options.HideDescSpouses;

            fController = new TreeChartWinController(this);
            fController.Init(baseWin);

            SetupDepth();
        }

        /*protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            fTreeBox.Select();
            UpdateControls();
        }

        protected override void OnClosed(EventArgs e)
        {
            AppHost.Instance.CloseDependentWindows(this);
            fController.OnClosed();
            base.OnClosed(e);
        }*/

        #region Interface handlers

        private void tbFilter_Click()
        {
            SetFilter();
        }

        private void tbPrev_Click()
        {
            NavPrev();
        }

        private void tbNext_Click()
        {
            NavNext();
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

        private void TreeChartWin_KeyDown(KeyEventEventArgs e)
        {
            switch (e.KeyEvent.Key) {
                case Key.F5:
                    GenChart();
                    break;

                case Key.F6:
                    miRebuildTree_Click();
                    break;

                case Key.F7:
                    fTreeBox.RebuildKinships();
                    break;

                case Key.Esc:
                    Close();
                    break;

                case Key.F:
                    if (e.KeyEvent.IsCtrl) {
                        QuickSearch();
                    }
                    break;

                case Key.S:
                    if (e.KeyEvent.IsCtrl) {
                        fController.Base.SaveFileEx(false);
                    }
                    break;

                case Key.P:
                    if (e.KeyEvent.IsAlt) {
                        fController.CopySnapshot();
                    }
                    break;
            }
        }

        private void tbImageSave_Click()
        {
            fController.SaveSnapshot();
        }

        private void ImageTree_PersonProperties(object sender, MouseEventArgs e)
        {
            MenuPerson.Position = new Point(e.MouseEvent.X, e.MouseEvent.Y);
            MenuPerson.Show();
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
            miGensInfCommon = UIHelper.AddToolStripItem(tbGensCommon, "Inf", -1, miGensX_Click);
            miGensInfAncestors = UIHelper.AddToolStripItem(tbGensAncestors, "Inf", -1, miGensXAncestors_Click);
            miGensInfDescendants = UIHelper.AddToolStripItem(tbGensDescendants, "Inf", -1, miGensXDescendants_Click);

            for (int i = 1; i <= 9; i++) {
                UIHelper.AddToolStripItem(tbGensCommon, i.ToString(), i, miGensX_Click);
                UIHelper.AddToolStripItem(tbGensAncestors, i.ToString(), i, miGensXAncestors_Click);
                UIHelper.AddToolStripItem(tbGensDescendants, i.ToString(), i, miGensXDescendants_Click);
            }
        }

        private void miGensX_Click()
        {
            /*int depth = UIHelper.GetMenuItemTag<int>(tbGensCommon, sender);
            fTreeBox.DepthLimitAncestors = depth;
            fTreeBox.DepthLimitDescendants = depth;
            GenChart();*/
        }

        private void miGensXAncestors_Click()
        {
            /*int depth = UIHelper.GetMenuItemTag<int>(tbGensAncestors, sender);
            fTreeBox.DepthLimitAncestors = depth;
            GenChart();*/
        }

        private void miGensXDescendants_Click()
        {
            /*int depth = UIHelper.GetMenuItemTag<int>(tbGensDescendants, sender);
            fTreeBox.DepthLimitDescendants = depth;
            GenChart();*/
        }

        private void SetupDepth()
        {
            var treeOptions = GlobalOptions.Instance.TreeChartOptions;

            fController.SetupDepth();

            if (!treeOptions.SeparateDepth) {
                UIHelper.SetMenuItemTag(tbGensCommon, treeOptions.DepthLimit);
            } else {
                UIHelper.SetMenuItemTag(tbGensAncestors, treeOptions.DepthLimitAncestors);
                UIHelper.SetMenuItemTag(tbGensDescendants, treeOptions.DepthLimitDescendants);
            }
        }

        private void miEdit_Click()
        {
            fController.Edit();
        }

        private void miFatherAdd_Click()
        {
            fController.AddFather();
        }

        private void miMotherAdd_Click()
        {
            fController.AddMother();
        }

        private void miSpouseAdd_Click()
        {
            fController.AddSpouse();
        }

        private void miSonAdd_Click()
        {
            fController.AddSon();
        }

        private void miDaughterAdd_Click()
        {
            fController.AddDaughter();
        }

        private void miFamilyAdd_Click()
        {
            fController.AddFamily();
        }

        private void miDelete_Click()
        {
            fController.Delete();
        }

        private void miRebuildKinships_Click()
        {
            fTreeBox.RebuildKinships();
        }

        private void miMapAncestors_Click()
        {
            fController.ShowMapAncestors();
        }

        private void miMapDescendants_Click()
        {
            fController.ShowMapDescendants();
        }

        private void miMapAll_Click()
        {
            fController.ShowMapAll();
        }

        private void miTraceSelected_Click()
        {
            miTraceSelected.Checked = !miTraceSelected.Checked;

            fTreeBox.Options.TraceSelected = miTraceSelected.Checked;
            fTreeBox.TraceSelected = miTraceSelected.Checked;
        }

        private void miTraceKinships_Click()
        {
            miTraceKinships.Checked = !miTraceKinships.Checked;
            fTreeBox.TraceKinships = miTraceKinships.Checked;
        }

        private void miHideDescSpouses_Click()
        {
            miHideDescSpouses.Checked = !miHideDescSpouses.Checked;
            fTreeBox.Options.HideDescSpouses = miHideDescSpouses.Checked;
            GenChart();
        }

        private void miCertaintyIndex_Click()
        {
            miCertaintyIndex.Checked = !miCertaintyIndex.Checked;

            fTreeBox.Options.CertaintyIndexVisible = miCertaintyIndex.Checked;
            fTreeBox.CertaintyIndex = miCertaintyIndex.Checked;
        }

        private void miXRefVisible_Click()
        {
            miXRefVisible.Checked = !miXRefVisible.Checked;

            fTreeBox.Options.XRefVisible = miXRefVisible.Checked;
            fTreeBox.XRefVisible = miXRefVisible.Checked;
        }

        private void miTrackSelectedLines_Click()
        {
            miTrackSelectedLines.Checked = !miTrackSelectedLines.Checked;
            fTreeBox.Options.TrackSelectedLines = miTrackSelectedLines.Checked;
            fTreeBox.Invalidate();
        }

        private void miTrackMatchedSources_Click()
        {
            miTrackMatchedSources.Checked = !miTrackMatchedSources.Checked;
            fTreeBox.Options.TrackMatchedSources = miTrackMatchedSources.Checked;
            fTreeBox.Invalidate();
        }

        private void miParentAges_Click()
        {
            miParentAges.Checked = !miParentAges.Checked;
            fTreeBox.Options.ParentAges = miParentAges.Checked;
            if (fTreeBox.Options.ParentAges) {
                GenChart();
            } else {
                fTreeBox.Invalidate();
            }
        }

        private async void miFillColor_Click()
        {
            await fController.SelectBackgroundColor();
        }

        private void miModeItem_Click()
        {
            /*TreeChartKind newMode = (TreeChartKind)((ToolStripMenuItem)sender).Tag;
            if (fTreeBox.Model.Kind == newMode) return;

            GenChart(fPerson, newMode);*/
        }

        private void miRebuildTree_Click()
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

        private void miSelectColor_Click()
        {
            fController.SelectColor();
        }

        private void MenuPerson_Opening(object sender, System.ComponentModel.CancelEventArgs e)
        {
            /*miFatherAdd.Enabled = fController.ParentIsRequired(GDMSex.svMale);
            miMotherAdd.Enabled = fController.ParentIsRequired(GDMSex.svFemale);

            bool isRealPerson = fController.SelectedPersonIsReal();
            miGoToRecord.Enabled = isRealPerson;
            miOpenInNewWindow.Enabled = isRealPerson;

            TreeChartPerson p = fTreeBox.Selected;
            miGoToPrimaryBranch.Enabled = (p != null && p.Rec != null && p.IsDup);*/
        }

        private void tbOptions_Click()
        {
            AppHost.Instance.ShowOptions(this, OptionsPage.opTreeChart);
        }

        private void miGoToRecord_Click()
        {
            fController.GoToRecord();
        }

        private void miGoToPrimaryBranch_Click()
        {
            fController.GoToPrimaryBranch();
        }

        private void miOpenInNewWindow_Click()
        {
            fController.OpenInNewWindow();
        }

        private void miMergeDuplicates_Click()
        {
            fController.MergeDuplicates();
        }

        #endregion

        #region IChartWindow implementation

        public void GenChart()
        {
            if (fPerson != null) {
                GenChart(fPerson, fTreeBox.Model.Kind);
            }
        }

        public void GenChart(GDMIndividualRecord startPerson, TreeChartKind chartKind)
        {
            try {
                if (startPerson == null) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.NotSelectedPerson));
                } else {
                    fPerson = startPerson;
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

                StatusLines[0] = string.Format(LangMan.LS(LSID.TreeIndividualsCount), fTreeBox.IndividualsCount);
                var imageSize = fTreeBox.GetImageSize();
                StatusLines[1] = string.Format(LangMan.LS(LSID.ImageSize), imageSize.Width, imageSize.Height);
                StatusLines[2] = string.Format("{0}: {1:f0} %", LangMan.LS(LSID.Scale), fTreeBox.Scale * 100);

                //tbPrev.Enabled = NavCanBackward();
                //tbNext.Enabled = NavCanForward();

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
                throw new ArgumentNullException(nameof(iRec));

            fTreeBox.SelectByRec(iRec);
        }

        public void QuickSearch()
        {
            /*QuickSearchDlg qsDlg = new QuickSearchDlg(this);
            qsDlg.Show();*/
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
