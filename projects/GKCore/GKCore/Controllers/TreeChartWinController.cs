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

using System.IO;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.Charts;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Options;
using GKCore.Types;

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

        public void UpdateTitle()
        {
            string kindName = "";
            switch (fView.TreeBox.Model.Kind) {
                case TreeChartKind.ckAncestors:
                    kindName = LangMan.LS(LSID.LSID_MITreeAncestors);
                    break;
                case TreeChartKind.ckDescendants:
                    kindName = LangMan.LS(LSID.LSID_MITreeDescendants);
                    break;
                case TreeChartKind.ckBoth:
                    kindName = LangMan.LS(LSID.LSID_MITreeBoth);
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

        private void InternalChildAdd(GDMSex needSex)
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p == null || p.Rec == null) return;

            GDMIndividualRecord child = fBase.Context.AddChildForParent(p.Rec, needSex);
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

        public void AddSpouse()
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p == null || p.Rec == null) return;

            GDMIndividualRecord iRec = p.Rec;
            GDMIndividualRecord iSpouse = fBase.Context.SelectSpouseFor(iRec);
            if (iSpouse == null) return;

            GDMFamilyRecord fam = fBase.Context.Tree.CreateFamily();
            fam.AddSpouse(iRec);
            fam.AddSpouse(iSpouse);
            UpdateChart();
        }

        private void ParentAdd(GDMSex needSex)
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
                GDMIndividualRecord parent = fBase.Context.SelectPerson(p.Rec, TargetMode.tmChild, needSex);
                if (parent != null) {
                    if (!familyExist) {
                        fam = fBase.Context.Tree.CreateFamily();
                        fam.AddChild(p.Rec);
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

        public void Edit()
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p == null || p.Rec == null) return;

            GDMIndividualRecord iRec = p.Rec;
            if (BaseController.ModifyIndividual(fBase, ref iRec, null, TargetMode.tmNone, GDMSex.svUnknown)) {
                UpdateChart();
            }
        }

        public void Delete()
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p == null || p.Rec == null || p == fView.TreeBox.Model.Root) return;

            BaseController.DeleteRecord(fBase, p.Rec, true);
            UpdateChart();
        }

        public void ModifyPerson(TreeChartPerson person)
        {
            if (person == null) return;

            bool modified = false;

            if (person.Rec != null) {
                GDMIndividualRecord iRec = person.Rec;
                modified = BaseController.ModifyIndividual(fBase, ref iRec, null, TargetMode.tmNone, GDMSex.svUnknown);
            } else {
                // this is "stub" person, only in descendant tree
                // key properties = BaseSpouse & BaseFamily
                TreeChartPerson baseSpouse = person.BaseSpouse;
                GDMFamilyRecord baseFamily = person.BaseFamily;

                if (baseSpouse != null && baseFamily != null) {
                    GDMIndividualRecord iSpouse = fBase.Context.SelectSpouseFor(person.BaseSpouse.Rec);

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
                BaseController.ViewRecordInfo(fBase, person.Rec);
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

        public bool SelectedPersonIsReal()
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            return (p != null && p.Rec != null);
        }

        public void SaveSnapshot()
        {
            string filters = GKUtils.GetImageFilter(true);
            string fileName = AppHost.StdDialogs.GetSaveFile("", "", filters, 2, "jpg", "");
            if (!string.IsNullOrEmpty(fileName)) {
                fView.TreeBox.SaveSnapshot(fileName);
            }
        }

        public void SetFilter()
        {
            using (var dlgFilter = AppHost.Container.Resolve<ITreeFilterDlg>(fBase)) {
                dlgFilter.Filter = fView.TreeBox.Model.Filter;

                if (dlgFilter.ShowModalX(fView)) {
                    fView.GenChart();
                }
            }
        }

        public void SelectColor()
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p == null || p.Rec == null) return;

            p.UserColor = AppHost.StdDialogs.SelectColor(p.UserColor);
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

        public override void SetLocale()
        {
            GetControl<IButtonToolItem>("tbGensCommon").Text = LangMan.LS(LSID.LSID_Generations);
            GetControl<IButtonToolItem>("tbGensAncestors").Text = LangMan.LS(LSID.LSID_Generations) + ": " + LangMan.LS(LSID.LSID_Ancestors);
            GetControl<IButtonToolItem>("tbGensDescendants").Text = LangMan.LS(LSID.LSID_Generations) + ": " + LangMan.LS(LSID.LSID_Descendants);
            GetControl<IButtonToolItem>("tbModes").Text = LangMan.LS(LSID.LSID_ModesTip);
            GetControl<IButtonToolItem>("tbBorders").Text = LangMan.LS(LSID.LSID_Borders);

            GetControl<IMenuItem>("miGensInfCommon").Text = LangMan.LS(LSID.LSID_Unlimited);
            GetControl<IMenuItem>("miGensInfAncestors").Text = LangMan.LS(LSID.LSID_Unlimited);
            GetControl<IMenuItem>("miGensInfDescendants").Text = LangMan.LS(LSID.LSID_Unlimited);
            GetControl<IMenuItem>("miModeBoth").Text = LangMan.LS(LSID.LSID_TM_Both);
            GetControl<IMenuItem>("miModeAncestors").Text = LangMan.LS(LSID.LSID_TM_Ancestors);
            GetControl<IMenuItem>("miModeDescendants").Text = LangMan.LS(LSID.LSID_TM_Descendants);
            GetControl<IMenuItem>("miEdit").Text = LangMan.LS(LSID.LSID_DoEdit);
            GetControl<IMenuItem>("miFatherAdd").Text = LangMan.LS(LSID.LSID_FatherAdd);
            GetControl<IMenuItem>("miMotherAdd").Text = LangMan.LS(LSID.LSID_MotherAdd);
            GetControl<IMenuItem>("miFamilyAdd").Text = LangMan.LS(LSID.LSID_FamilyAdd);
            GetControl<IMenuItem>("miSpouseAdd").Text = LangMan.LS(LSID.LSID_SpouseAdd);
            GetControl<IMenuItem>("miSonAdd").Text = LangMan.LS(LSID.LSID_SonAdd);
            GetControl<IMenuItem>("miDaughterAdd").Text = LangMan.LS(LSID.LSID_DaughterAdd);
            GetControl<IMenuItem>("miDelete").Text = LangMan.LS(LSID.LSID_DoDelete);
            GetControl<IMenuItem>("miRebuildTree").Text = LangMan.LS(LSID.LSID_RebuildTree);
            GetControl<IMenuItem>("miRebuildKinships").Text = LangMan.LS(LSID.LSID_RebuildKinships);
            GetControl<IMenuItem>("miFillColor").Text = LangMan.LS(LSID.LSID_FillColor);
            GetControl<IMenuItem>("miFillImage").Text = LangMan.LS(LSID.LSID_FillImage);
            GetControl<IMenuItem>("miTraceSelected").Text = LangMan.LS(LSID.LSID_TM_TraceSelected);
            GetControl<IMenuItem>("miTraceKinships").Text = LangMan.LS(LSID.LSID_TM_TraceKinships);
            GetControl<IMenuItem>("miCertaintyIndex").Text = LangMan.LS(LSID.LSID_CertaintyIndex);
            GetControl<IMenuItem>("miSelectColor").Text = LangMan.LS(LSID.LSID_SelectColor);
            GetControl<IMenuItem>("miGoToRecord").Text = LangMan.LS(LSID.LSID_GoToPersonRecord);
            GetControl<IMenuItem>("miGoToPrimaryBranch").Text = LangMan.LS(LSID.LSID_GoToPrimaryBranch);

            SetToolTip("tbModes", LangMan.LS(LSID.LSID_ModesTip));
            SetToolTip("tbImageSave", LangMan.LS(LSID.LSID_ImageSaveTip));
            SetToolTip("tbDocPrint", LangMan.LS(LSID.LSID_DocPrint));
            SetToolTip("tbDocPreview", LangMan.LS(LSID.LSID_DocPreview));
            SetToolTip("tbPrev", LangMan.LS(LSID.LSID_PrevRec));
            SetToolTip("tbNext", LangMan.LS(LSID.LSID_NextRec));
            SetToolTip("tbFilter", LangMan.LS(LSID.LSID_MIFilter));
            SetToolTip("tbOptions", LangMan.LS(LSID.LSID_MIOptions));
        }
    }
}
