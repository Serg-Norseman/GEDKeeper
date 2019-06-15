/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore.Charts;
using GKCore.MVP;
using GKCore.MVP.Views;
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
            switch (fView.ChartKind) {
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

            fView.Caption = string.Format("{0} \"{1}\"", kindName, Path.GetFileName(fBase.Context.FileName));
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

            bool needParent = false;
            bool familyExist = p.Rec.GetParentsFamily() != null;

            if (familyExist) {
                GDMIndividualRecord mother, father;
                GDMFamilyRecord fam = p.Rec.GetParentsFamily();
                if (fam == null) {
                    father = null;
                    mother = null;
                } else {
                    father = fam.Husband.Individual;
                    mother = fam.Wife.Individual;
                }

                needParent = (father == null && needSex == GDMSex.svMale) ||
                    (mother == null && needSex == GDMSex.svFemale);
            }

            if (!familyExist || needParent) {
                GDMIndividualRecord child = p.Rec;
                GDMFamilyRecord fam = (familyExist) ? p.Rec.GetParentsFamily() : fBase.Context.Tree.CreateFamily();
                GDMIndividualRecord parent = fBase.Context.SelectPerson(null, TargetMode.tmParent, needSex);
                if (parent != null) {
                    fam.AddSpouse(parent);
                    if (!familyExist)
                        fam.AddChild(child);
                    
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

        public bool ParentIsRequired(GDMSex needSex)
        {
            TreeChartPerson p = fView.TreeBox.Selected;
            if (p == null || p.Rec == null) return false;

            bool familyExist = p.Rec.GetParentsFamily() != null;
            if (!familyExist) return true;

            GDMIndividualRecord mother, father;
            GDMFamilyRecord fam = p.Rec.GetParentsFamily();
            if (fam == null) {
                father = null;
                mother = null;
            } else {
                father = fam.Husband.Individual;
                mother = fam.Wife.Individual;
            }

            bool needParent = (father == null && needSex == GDMSex.svMale) ||
                (mother == null && needSex == GDMSex.svFemale);
            return needParent;
        }

        // TODO: update localization
        public void SaveSnapshot()
        {
            string filters = LangMan.LS(LSID.LSID_TreeImagesFilter) + "|SVG files (*.svg)|*.svg";
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
    }
}
