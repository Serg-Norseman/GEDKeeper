/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore;
using GKCore.Charts;
using GKCore.Controllers;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Options;
using GKUI.Components;
using GKUI.Platform;

namespace GKUI.Forms
{
    public partial class TreeChartWin : PrintableForm, ITreeChartWin
    {
        private readonly TreeChartWinController fController;

        private readonly TreeChartBox fTreeBox;

        private GDMIndividualRecord fPerson;

        private List<GKComboItem<GfxBorderStyle>> fBorderItems;
        private List<GKComboItem<int>> fGenItems;
        private List<GKComboItem<TreeChartKind>> fModes;


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
            InitializeComponentEx();

            fTreeBox = new TreeChartBox(new XFGfxRenderer());
            fTreeBox.Base = baseWin;
            fTreeBox.PersonModify += ImageTree_PersonModify;
            fTreeBox.RootChanged += ImageTree_RootChanged;
            fTreeBox.InfoRequest += ImageTree_InfoRequest;
            fTreeBox.PersonProperties += ImageTree_PersonProperties;
            fTreeBox.Options = GlobalOptions.Instance.TreeChartOptions;
            fTreeBox.NavRefresh += ImageTree_NavRefresh;
            fTreeBox.ZoomChanged += ImageTree_NavRefresh;
            Content = fTreeBox;

            PopulateContextMenus();

            /*
            miCertaintyIndex.Checked = fTreeBox.Options.CertaintyIndexVisible;
            fTreeBox.CertaintyIndex = fTreeBox.Options.CertaintyIndexVisible;

            miXRefVisible.Checked = fTreeBox.Options.XRefVisible;
            fTreeBox.XRefVisible = fTreeBox.Options.XRefVisible;

            miTrackSelectedLines.Checked = fTreeBox.Options.TrackSelectedLines;

            miTrackMatchedSources.Checked = fTreeBox.Options.TrackMatchedSources;

            miTraceSelected.Checked = fTreeBox.Options.TraceSelected;
            fTreeBox.TraceSelected = fTreeBox.Options.TraceSelected;

            miTraceKinships.Checked = fTreeBox.TraceKinships;
            miTraceKinships.Enabled = false;

            miHideDescSpouses.Checked = fTreeBox.Options.HideDescSpouses;*/

            fController = new TreeChartWinController(this);
            fController.Init(baseWin);

            SetupDepth();
        }

        private void InitializeComponentEx()
        {
            /*
            miHideDescSpouses = new CheckMenuItem();
            miHideDescSpouses.Click += miHideDescSpouses_Click;

            miTraceSelected = new CheckMenuItem();
            miTraceSelected.Click += miTraceSelected_Click;

            miTraceKinships = new CheckMenuItem();
            miTraceKinships.Click += miTraceKinships_Click;

            miCertaintyIndex = new CheckMenuItem();
            miCertaintyIndex.Click += miCertaintyIndex_Click;

            miXRefVisible = new CheckMenuItem();
            miXRefVisible.Click += miXRefVisible_Click;

            miFillColor = new ButtonMenuItem();
            miFillColor.Click += miFillColor_Click;

            miFillImage = new ButtonMenuItem();
            miFillImage.Click += miFillImage_Click;

            MenuModes.Items.AddRange(new MenuItem[] {
                                         miHideDescSpouses,
                                         miTraceSelected,
                                         miTraceKinships,
                                         miCertaintyIndex,
                                         miXRefVisible,
                                         miFillColor,
                                         miFillImage});
            */
        }

        protected override void OnAppearing()
        {
            base.OnAppearing();

            fTreeBox.Focus();
            UpdateControls();
        }

        protected override void OnDisappearing()
        {
            AppHost.Instance.CloseDependentWindows(this);
            fController.OnClosed();
            base.OnDisappearing();
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
            // not used
        }

        private void tbImageSave_Click(object sender, EventArgs e)
        {
            fController.SaveSnapshot();
        }

        private void ImageTree_PersonProperties(object sender, EventArgs e)
        {
            //MenuPerson.Show(fTreeBox, ((MouseEventArgs)e).Location);
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
            fGenItems = new List<GKComboItem<int>>();
            fGenItems.Add(new GKComboItem<int>(LangMan.LS(LSID.Unlimited), -1));
            for (int i = 1; i <= 9; i++) {
                fGenItems.Add(new GKComboItem<int>(i.ToString(), i));
            }

            fBorderItems = new List<GKComboItem<GfxBorderStyle>>();
            for (var bs = GfxBorderStyle.None; bs <= GfxBorderStyle.Last; bs++) {
                fBorderItems.Add(new GKComboItem<GfxBorderStyle>(LangMan.LS(BorderPainter.StyleNames[(int)bs]), bs));
            }

            fModes = new List<GKComboItem<TreeChartKind>>();
            fModes.Add(new GKComboItem<TreeChartKind>(LangMan.LS(LSID.TM_Both), TreeChartKind.ckBoth));
            fModes.Add(new GKComboItem<TreeChartKind>(LangMan.LS(LSID.TM_Ancestors), TreeChartKind.ckAncestors));
            fModes.Add(new GKComboItem<TreeChartKind>(LangMan.LS(LSID.TM_Descendants), TreeChartKind.ckDescendants));
        }

        private async void miGensX_Click(object sender, EventArgs e)
        {
            var depthItem = await UIHelper.SelectItem(this, fGenItems);
            if (depthItem != null) {
                fTreeBox.DepthLimitAncestors = depthItem.Tag;
                fTreeBox.DepthLimitDescendants = depthItem.Tag;
                GenChart();
            }
        }

        private async void miGensXAncestors_Click(object sender, EventArgs e)
        {
            var depthItem = await UIHelper.SelectItem(this, fGenItems);
            if (depthItem != null) {
                fTreeBox.DepthLimitAncestors = depthItem.Tag;
                GenChart();
            }
        }

        private async void miGensXDescendants_Click(object sender, EventArgs e)
        {
            var depthItem = await UIHelper.SelectItem(this, fGenItems);
            if (depthItem != null) {
                fTreeBox.DepthLimitDescendants = depthItem.Tag;
                GenChart();
            }
        }

        private void SetupDepth()
        {
            var treeOptions = GlobalOptions.Instance.TreeChartOptions;

            fController.SetupDepth();
        }

        private async void miBorderX_Click(object sender, EventArgs e)
        {
            var borderItem = await UIHelper.SelectItem(this, fBorderItems);
            if (borderItem != null) {
                GlobalOptions.Instance.TreeChartOptions.BorderStyle = borderItem.Tag;
                fTreeBox.Invalidate();
            }
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
            /*fTreeBox.Options.TraceSelected = miTraceSelected.Checked;
            fTreeBox.TraceSelected = miTraceSelected.Checked;*/
        }

        private void miTraceKinships_Click(object sender, EventArgs e)
        {
            /*fTreeBox.TraceKinships = miTraceKinships.Checked;*/
        }

        private void miHideDescSpouses_Click(object sender, EventArgs e)
        {
            /*fTreeBox.Options.HideDescSpouses = miHideDescSpouses.Checked;
            GenChart();*/
        }

        private void miCertaintyIndex_Click(object sender, EventArgs e)
        {
            /*fTreeBox.Options.CertaintyIndexVisible = miCertaintyIndex.Checked;
            fTreeBox.CertaintyIndex = miCertaintyIndex.Checked;*/
        }

        private void miXRefVisible_Click(object sender, EventArgs e)
        {
            /*fTreeBox.Options.XRefVisible = miXRefVisible.Checked;
            fTreeBox.XRefVisible = miXRefVisible.Checked;*/
        }

        private void miFillColor_Click(object sender, EventArgs e)
        {
            /*using (var colorDialog1 = new ColorDialog()) {
                if (colorDialog1.ShowDialog(this) != DialogResult.Ok) return;

                fTreeBox.BackgroundImage = null;
                fTreeBox.BackgroundColor = colorDialog1.Color;
                fTreeBox.Invalidate();
            }*/
        }

        private void miFillImage_Click(object sender, EventArgs e)
        {
            /*string fileName = AppHost.StdDialogs.GetOpenFile("", GKUtils.GetBackgroundsPath(), LangMan.LS(LSID.ImagesFilter), 1, "");
            if (string.IsNullOrEmpty(fileName)) return;

            Image img = new Bitmap(fileName);
            fTreeBox.BackgroundImage = img;
            //fTreeBox.BackgroundImageLayout = ImageLayout.Tile;
            fTreeBox.Invalidate();*/
        }

        private async void miModeItem_Click(object sender, EventArgs e)
        {
            var modeItem = await UIHelper.SelectItem(this, fModes);
            if (modeItem != null && fTreeBox.Model.Kind != modeItem.Tag) {
                GenChart(fPerson, modeItem.Tag);
            }
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

        private void MenuPerson_Opening(object sender, EventArgs e)
        {
            /*miFatherAdd.Enabled = fController.ParentIsRequired(GDMSex.svMale);
            miMotherAdd.Enabled = fController.ParentIsRequired(GDMSex.svFemale);

            bool isRealPerson = fController.SelectedPersonIsReal();
            miGoToRecord.Enabled = isRealPerson;
            miOpenInNewWindow.Enabled = isRealPerson;

            TreeChartPerson p = fTreeBox.Selected;
            miGoToPrimaryBranch.Enabled = (p != null && p.Rec != null && p.IsDup);*/
        }

        private void tbOptions_Click(object sender, EventArgs e)
        {
            AppHost.Instance.ShowOptions(this, OptionsPage.opTreeChart);
        }

        private void miGoToRecord_Click(object sender, EventArgs e)
        {
            fController.GoToRecord();
        }

        private void miGoToPrimaryBranch_Click(object sender, EventArgs e)
        {
            fController.GoToPrimaryBranch();
        }

        private void miOpenInNewWindow_Click(object sender, EventArgs e)
        {
            fController.OpenInNewWindow();
        }

        private void miMergeDuplicates_Click(object sender, EventArgs e)
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

                tbPrev.IsEnabled = NavCanBackward();
                tbNext.IsEnabled = NavCanForward();

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
