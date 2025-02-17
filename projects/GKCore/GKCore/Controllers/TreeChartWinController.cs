/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;
using System.IO;
using GDModel;
using GKCore.Charts;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Options;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class TreeChartWinController : FormController<ITreeChartWin>
    {

        public TreeChartWinController(ITreeChartWin view) : base(view)
        {
        }

        public override void UpdateView()
        {
        }

        public void OnClosed()
        {
            var mruFile = AppHost.Instance.GetMRUFile(fBase);
            var root = fView.TreeBox.Model.Root;
            if (mruFile != null && root != null && root.Rec != null) {
                mruFile.LastTreeRecord = root.Rec.XRef;
            }
        }

        public void UpdateTitle()
        {
            string kindName = "";
            switch (fView.TreeBox.Model.Kind) {
                case TreeChartKind.ckAncestors:
                    kindName = LangMan.LS(LSID.MITreeAncestors);
                    break;
                case TreeChartKind.ckDescendants:
                    kindName = LangMan.LS(LSID.MITreeDescendants);
                    break;
                case TreeChartKind.ckBoth:
                    kindName = LangMan.LS(LSID.MITreeBoth);
                    break;
            }

            fView.Title = string.Format("{0} \"{1}\"", kindName, Path.GetFileName(fBase.Context.FileName));
        }

        public void UpdateChart()
        {
            if (fBase != null) {
                fBase.RefreshLists(false);
            }

            fView.TreeBox.RefreshTree();
        }

        public void AddFamily()
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p == null || p.Rec == null) return;

            GDMFamilyRecord fam = fBase.Context.AddFamilyForSpouse(p.Rec);
            if (fam == null) return;

            UpdateChart();
        }

        private async void InternalChildAdd(GDMSex needSex)
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p == null || p.Rec == null) return;

            GDMIndividualRecord child = await fBase.Context.AddChildForParent(fView, p.Rec, needSex);
            if (child == null) return;

            UpdateChart();
        }

        public void AddSon()
        {
            InternalChildAdd(GDMSex.svMale);
        }

        public void AddDaughter()
        {
            InternalChildAdd(GDMSex.svFemale);
        }

        public async void AddSpouse()
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p == null || p.Rec == null) return;

            GDMIndividualRecord iRec = p.Rec;
            GDMIndividualRecord iSpouse = await fBase.Context.SelectSpouseFor(fView, iRec);
            if (iSpouse == null) return;

            GDMFamilyRecord fam = fBase.Context.Tree.CreateFamily();
            fam.AddSpouse(iRec);
            fam.AddSpouse(iSpouse);
            UpdateChart();
        }

        private async void ParentAdd(GDMSex needSex)
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p == null || p.Rec == null) return;

            bool needParent;

            GDMFamilyRecord fam = fBase.Context.Tree.GetParentsFamily(p.Rec);
            bool familyExist = fam != null;

            if (familyExist) {
                GDMIndividualRecord father, mother;
                fBase.Context.Tree.GetSpouses(fam, out father, out mother);

                needParent = (father == null && needSex == GDMSex.svMale) ||
                             (mother == null && needSex == GDMSex.svFemale);
            } else {
                needParent = true;
            }

            if (needParent) {
                GDMIndividualRecord parent = await fBase.Context.SelectPerson(fView, p.Rec, TargetMode.tmChild, needSex);
                if (parent != null) {
                    if (!familyExist) {
                        fam = fBase.Context.Tree.CreateFamily();
                        fam.AddChild(p.Rec);
                    }

                    if (fam.HasMember(parent)) {
                        AppHost.StdDialogs.ShowAlert(LangMan.LS(LSID.InvalidLink));
                        return;
                    }

                    fam.AddSpouse(parent);
                    
                    UpdateChart();
                }
            }
        }

        public void AddFather()
        {
            ParentAdd(GDMSex.svMale);
        }

        public void AddMother()
        {
            ParentAdd(GDMSex.svFemale);
        }

        public async void Edit()
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p == null || p.Rec == null) return;

            var indiRes = await BaseController.ModifyIndividual(fView, fBase, p.Rec, null, TargetMode.tmNone, GDMSex.svUnknown);
            if (indiRes.Result) {
                UpdateChart();
            }
        }

        public async void Delete()
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p == null || p.Rec == null || p == fView.TreeBox.Model.Root) return;

            await BaseController.DeleteRecord(fBase, p.Rec, true);
            UpdateChart();
        }

        public async void ModifyPerson(TreeChartPerson person)
        {
            if (person == null) return;

            bool modified = false;

            if (person.Rec != null) {
                var indiRes = await BaseController.ModifyIndividual(fView, fBase, person.Rec, null, TargetMode.tmNone, GDMSex.svUnknown);
                modified = indiRes.Result;
            } else {
                // this is "stub" person, only in descendant tree
                // key properties = BaseSpouse & BaseFamily
                TreeChartPerson baseSpouse = person.BaseSpouse;
                GDMFamilyRecord baseFamily = person.BaseFamily;

                if (baseSpouse != null && baseFamily != null) {
                    GDMIndividualRecord iSpouse = await fBase.Context.SelectSpouseFor(fView, person.BaseSpouse.Rec);

                    if (iSpouse != null) {
                        modified = baseFamily.AddSpouse(iSpouse);
                    }
                }
            }

            if (modified) {
                UpdateChart();
            }
        }

        public void RequestInfo(TreeChartPerson person)
        {
            if (person != null) {
                BaseController.ViewRecordInfo(fView, fBase, person.Rec);
            }
        }

        public bool ParentIsRequired(GDMSex needSex)
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p == null || p.Rec == null) return false;

            bool familyExist = fBase.Context.Tree.GetParentsFamily(p.Rec) != null;
            if (!familyExist) return true;

            GDMIndividualRecord father, mother;
            fBase.Context.Tree.GetParents(p.Rec, out father, out mother);

            bool needParent = (father == null && needSex == GDMSex.svMale) ||
                (mother == null && needSex == GDMSex.svFemale);
            return needParent;
        }

        public void GoToRecord()
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p == null || p.Rec == null) return;

            fBase.SelectRecordByXRef(p.Rec.XRef, false);
            fBase.Activate();
        }

        public void OpenInNewWindow()
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p == null || p.Rec == null) return;

            BaseController.ShowTreeChart(fBase, p.Rec, TreeChartKind.ckBoth);
        }

        public void MergeDuplicates()
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p == null || p.Rec == null) return;

            BaseController.ShowRecMerge(fView, fBase, p.Rec, null);
        }

        public bool SelectedPersonIsReal()
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            return (p != null && p.Rec != null);
        }

        public async void SaveSnapshot()
        {
            string filters = GKUtils.GetImageFilter(true);
            filters += "|" + LangMan.LS(LSID.PDFFilter);

            string fileName = await AppHost.StdDialogs.GetSaveFile("", GlobalOptions.Instance.ImageExportLastDir, filters, 2, "jpg", "");
            if (!string.IsNullOrEmpty(fileName)) {
                GlobalOptions.Instance.ImageExportLastDir = Path.GetDirectoryName(fileName);

                fView.TreeBox.SaveSnapshot(fileName);
            }
        }

        public async void SetFilter()
        {
            using (var dlgFilter = AppHost.Container.Resolve<ITreeFilterDlg>(fBase)) {
                dlgFilter.Filter = fView.TreeBox.Model.Filter;

                if (await AppHost.Instance.ShowModalAsync(dlgFilter, fView)) {
                    fView.GenChart();
                }
            }
        }

        public async void SelectColor()
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p == null || p.Rec == null) return;

            p.UserColor = await AppHost.StdDialogs.SelectColor(p.UserColor);
            fView.TreeBox.Invalidate();
        }

        public void GoToPrimaryBranch()
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p == null || p.Rec == null || !p.IsDup) return;

            var pers = fView.TreeBox.Model.FindPersonByRec(p.Rec, true);
            if (pers != null) {
                fView.TreeBox.SelectBy(pers, true);
            }
        }

        public void SetupDepth()
        {
            var treeOptions = GlobalOptions.Instance.TreeChartOptions;

            GetControl<IButtonToolItem>("tbGensCommon").Visible = !treeOptions.SeparateDepth;
            GetControl<IButtonToolItem>("tbGensAncestors").Visible = treeOptions.SeparateDepth;
            GetControl<IButtonToolItem>("tbGensDescendants").Visible = treeOptions.SeparateDepth;
        }

        public void ShowMapAncestors()
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p != null) {
                var selectedIndividuals = fView.TreeBox.Model.GetAncestors(p);
                BaseController.ShowMap_IndiList(fBase, selectedIndividuals);
            }
        }

        public void ShowMapDescendants()
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p != null) {
                var selectedIndividuals = fView.TreeBox.Model.GetDescendants(p);
                BaseController.ShowMap_IndiList(fBase, selectedIndividuals);
            }
        }

        public void ShowMapAll()
        {
            var selectedIndividuals = new List<GDMIndividualRecord>();
            var model = fView.TreeBox.Model;
            for (int i = 0; i < model.Persons.Count; i++) {
                GDMIndividualRecord indiRec = model.Persons[i].Rec;
                if (indiRec != null) selectedIndividuals.Add(indiRec);
            }
            BaseController.ShowMap_IndiList(fBase, selectedIndividuals);
        }

        public override void SetLocale()
        {
            GetControl<IButtonToolItem>("tbGensCommon").Text = LangMan.LS(LSID.Generations);
            GetControl<IButtonToolItem>("tbGensAncestors").Text = LangMan.LS(LSID.Generations) + ": " + LangMan.LS(LSID.Ancestors);
            GetControl<IButtonToolItem>("tbGensDescendants").Text = LangMan.LS(LSID.Generations) + ": " + LangMan.LS(LSID.Descendants);
            GetControl<IButtonToolItem>("tbBorders").Text = LangMan.LS(LSID.Borders);
            GetControl<IButtonToolItem>("tbModes").Text = LangMan.LS(LSID.ModesTip);

            if (AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<IButtonToolItem>("miEdit").Text = LangMan.LS(LSID.DoEdit);
                GetControl<IButtonToolItem>("miFatherAdd").Text = LangMan.LS(LSID.FatherAdd);
                GetControl<IButtonToolItem>("miMotherAdd").Text = LangMan.LS(LSID.MotherAdd);
                GetControl<IButtonToolItem>("miFamilyAdd").Text = LangMan.LS(LSID.FamilyAdd);
                GetControl<IButtonToolItem>("miSpouseAdd").Text = LangMan.LS(LSID.SpouseAdd);
                GetControl<IButtonToolItem>("miSonAdd").Text = LangMan.LS(LSID.SonAdd);
                GetControl<IButtonToolItem>("miDaughterAdd").Text = LangMan.LS(LSID.DaughterAdd);
                GetControl<IButtonToolItem>("miDelete").Text = LangMan.LS(LSID.DoDelete);
                GetControl<IButtonToolItem>("miGoToRecord").Text = LangMan.LS(LSID.GoToPersonRecord);
                GetControl<IButtonToolItem>("miGoToPrimaryBranch").Text = LangMan.LS(LSID.GoToPrimaryBranch);
                GetControl<IButtonToolItem>("miOpenInNewWindow").Text = LangMan.LS(LSID.OpenInNewWindow);
                GetControl<IButtonToolItem>("miMergeDuplicates").Text = LangMan.LS(LSID.MergeDuplicates);
                GetControl<IButtonToolItem>("miRebuildTree").Text = LangMan.LS(LSID.RebuildTree);
                GetControl<IButtonToolItem>("miRebuildKinships").Text = LangMan.LS(LSID.RebuildKinships);
                GetControl<IButtonToolItem>("miSelectColor").Text = LangMan.LS(LSID.SelectColor);
                return;
            }

            GetControl<IMenuItem>("miGensInfCommon").Text = LangMan.LS(LSID.Unlimited);
            GetControl<IMenuItem>("miGensInfAncestors").Text = LangMan.LS(LSID.Unlimited);
            GetControl<IMenuItem>("miGensInfDescendants").Text = LangMan.LS(LSID.Unlimited);
            GetControl<IMenuItem>("miModeBoth").Text = LangMan.LS(LSID.TM_Both);
            GetControl<IMenuItem>("miModeAncestors").Text = LangMan.LS(LSID.TM_Ancestors);
            GetControl<IMenuItem>("miModeDescendants").Text = LangMan.LS(LSID.TM_Descendants);

            GetControl<IMenuItem>("miEdit").Text = LangMan.LS(LSID.DoEdit);
            GetControl<IMenuItem>("miFatherAdd").Text = LangMan.LS(LSID.FatherAdd);
            GetControl<IMenuItem>("miMotherAdd").Text = LangMan.LS(LSID.MotherAdd);
            GetControl<IMenuItem>("miFamilyAdd").Text = LangMan.LS(LSID.FamilyAdd);
            GetControl<IMenuItem>("miSpouseAdd").Text = LangMan.LS(LSID.SpouseAdd);
            GetControl<IMenuItem>("miSonAdd").Text = LangMan.LS(LSID.SonAdd);
            GetControl<IMenuItem>("miDaughterAdd").Text = LangMan.LS(LSID.DaughterAdd);
            GetControl<IMenuItem>("miDelete").Text = LangMan.LS(LSID.DoDelete);
            GetControl<IMenuItem>("miGoToRecord").Text = LangMan.LS(LSID.GoToPersonRecord);
            GetControl<IMenuItem>("miGoToPrimaryBranch").Text = LangMan.LS(LSID.GoToPrimaryBranch);
            GetControl<IMenuItem>("miOpenInNewWindow").Text = LangMan.LS(LSID.OpenInNewWindow);
            GetControl<IMenuItem>("miMergeDuplicates").Text = LangMan.LS(LSID.MergeDuplicates);
            GetControl<IMenuItem>("miRebuildTree").Text = LangMan.LS(LSID.RebuildTree);
            GetControl<IMenuItem>("miRebuildKinships").Text = LangMan.LS(LSID.RebuildKinships);
            GetControl<IMenuItem>("miSelectColor").Text = LangMan.LS(LSID.SelectColor);

            GetControl<IMenuItem>("miMaps").Text = LangMan.LS(LSID.MIMap);
            GetControl<IMenuItem>("miMapAncestors").Text = LangMan.LS(LSID.Ancestors);
            GetControl<IMenuItem>("miMapDescendants").Text = LangMan.LS(LSID.Descendants);
            GetControl<IMenuItem>("miMapAll").Text = LangMan.LS(LSID.TM_Both);

            GetControl<IMenuItem>("miFillColor").Text = LangMan.LS(LSID.FillColor);
            GetControl<IMenuItem>("miFillImage").Text = LangMan.LS(LSID.FillImage);
            GetControl<IMenuItem>("miTraceSelected").Text = LangMan.LS(LSID.TM_TraceSelected);
            GetControl<IMenuItem>("miTraceKinships").Text = LangMan.LS(LSID.TM_TraceKinships);
            GetControl<IMenuItem>("miCertaintyIndex").Text = LangMan.LS(LSID.CertaintyIndex);
            GetControl<IMenuItem>("miXRefVisible").Text = LangMan.LS(LSID.XRefVisible);
            GetControl<IMenuItem>("miTrackSelectedLines").Text = LangMan.LS(LSID.TrackSelectedLines);
            GetControl<IMenuItem>("miTrackMatchedSources").Text = LangMan.LS(LSID.TrackMatchedSources);
            GetControl<IMenuItem>("miHideDescSpouses").Text = LangMan.LS(LSID.HideDescSpouses);
            GetControl<IMenuItem>("miParentAges").Text = LangMan.LS(LSID.ParentsAge);

            SetToolTip("tbModes", LangMan.LS(LSID.ModesTip));
            SetToolTip("tbImageSave", LangMan.LS(LSID.ImageSaveTip));
            SetToolTip("tbDocPrint", LangMan.LS(LSID.DocPrint));
            SetToolTip("tbDocPreview", LangMan.LS(LSID.DocPreview));
            SetToolTip("tbPrev", LangMan.LS(LSID.PrevRec));
            SetToolTip("tbNext", LangMan.LS(LSID.NextRec));
            SetToolTip("tbFilter", LangMan.LS(LSID.MIFilter));
            SetToolTip("tbOptions", LangMan.LS(LSID.MIOptions));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IToolItem>("tbImageSave").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_ImageSave, true);
            GetControl<IToolItem>("tbDocPrint").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_DocPrint, true);
            GetControl<IToolItem>("tbDocPreview").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_DocPreview, true);
            GetControl<IToolItem>("tbPrev").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Prev, true);
            GetControl<IToolItem>("tbNext").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Next, true);
            GetControl<IToolItem>("tbOptions").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Settings, true);
            GetControl<IToolItem>("tbFilter").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Filter, true);
        }
    }
}
