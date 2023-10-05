/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class RecordSelectDlgController : DialogController<IRecordSelectDialog>
    {
        private GDMRecordType fRecType;
        private readonly Target fTarget;


        public GDMRecordType RecType
        {
            get { return fRecType; }
            set {
                fRecType = value;
                UpdateFilters();
            }
        }

        public Target Target
        {
            get { return fTarget; }
        }


        public RecordSelectDlgController(IRecordSelectDialog view) : base(view)
        {
            fTarget = new Target();
        }

        private void UpdateFilters()
        {
            var filters = GlobalOptions.Instance.GetRSFilters(fRecType);
            filters.Sort();

            fView.FilterBox.Clear();
            fView.FilterBox.Add("*");
            fView.FilterBox.AddStrings(filters);
        }

        public void SetTarget(TargetMode mode, GDMIndividualRecord target, GDMSex needSex, string defFilter = "*")
        {
            fTarget.TargetMode = mode;
            fTarget.TargetIndividual = target;
            fTarget.NeedSex = needSex;

            if (GlobalOptions.Instance.UseSurnamesInPersonSelectionFilter && mode != TargetMode.tmNone && target != null) {
                string[] fams = fBase.Context.GetIndividualSurnames(target);
                if (fams != null && fams.Length > 0 && !string.IsNullOrEmpty(fams[0])) {
                    defFilter = fams[0];
                }
            }

            fView.FilterBox.Text = defFilter;

            UpdateView();
        }

        // select child for parent
        private bool ChildSelectorHandler(GDMRecord record)
        {
            var probableChild = (GDMIndividualRecord)record;
            bool result = (probableChild != null && probableChild.ChildToFamilyLinks.Count == 0);

            if (GlobalOptions.Instance.UseBirthDatesInPersonSelectionFilter) {
                var parent = fTarget.TargetIndividual;
                result = result && (parent.GetUDN(GEDCOMTagType.BIRT).CompareTo(probableChild.GetUDN(GEDCOMTagType.BIRT)) < 0);
            }

            return result;
        }

        // select parent for child
        private bool ParentSelectorHandler(GDMRecord record)
        {
            var probableParent = (GDMIndividualRecord)record;
            var child = fTarget.TargetIndividual;
            return (probableParent.GetUDN(GEDCOMTagType.BIRT).CompareTo(child.GetUDN(GEDCOMTagType.BIRT)) < 0);
        }

        private bool SpouseSelectorHandler(GDMRecord record)
        {
            var famRec = (GDMFamilyRecord)record;
            return (famRec != null && famRec.HasSpouse(fTarget.TargetIndividual));
        }

        public void ChangeFilter()
        {
            string flt = fView.FilterBox.Text;
            GKUtils.SaveFilter(flt, GlobalOptions.Instance.GetRSFilters(fRecType));
            UpdateFilters();
        }

        public override void UpdateView()
        {
            string flt = fView.FilterBox.Text;
            if (string.IsNullOrEmpty(flt)) {
                flt = "*";
            } else if (flt != "*") {
                flt = "*" + flt + "*";
            }

            IListView recordsList = fView.RecordsList;
            recordsList.ListMan.Filter.Clear();
            recordsList.ListMan.QuickFilter.Value = flt;

            fView.FilterCtl.Params = recordsList.ListMan.QuickFilter;

            switch (fRecType) {
                case GDMRecordType.rtIndividual: {
                        IndividualListFilter iFilter = (IndividualListFilter)recordsList.ListMan.Filter;
                        iFilter.Sex = fTarget.NeedSex;
                        switch (fTarget.TargetMode) {
                            case TargetMode.tmParent:
                                recordsList.ListMan.ExternalFilter = ChildSelectorHandler;
                                break;
                            case TargetMode.tmChild:
                                if (GlobalOptions.Instance.UseBirthDatesInPersonSelectionFilter) {
                                    recordsList.ListMan.ExternalFilter = ParentSelectorHandler;
                                }
                                break;
                            case TargetMode.tmSpouse:
                                break;
                            case TargetMode.tmFamilyChild:
                                break;
                            case TargetMode.tmFamilySpouse:
                                break;
                        }
                        recordsList.SortModelColumn((int)IndividualListModel.ColumnType.ctName);
                        break;
                    }

                case GDMRecordType.rtFamily:
                    if (fTarget.TargetMode == TargetMode.tmFamilySpouse) {
                        recordsList.ListMan.ExternalFilter = SpouseSelectorHandler;
                    }
                    recordsList.SortModelColumn((int)FamilyListModel.ColumnType.ctFamilyStr);
                    break;
            }

            recordsList.UpdateContents();
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.WinRecordSelect);

            GetControl<IButton>("btnCreate").Text = LangMan.LS(LSID.DlgAppend);
            GetControl<IButton>("btnSelect").Text = LangMan.LS(LSID.DlgSelect);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);

            SetToolTip("txtFastFilter", LangMan.LS(LSID.PressEnterToSaveFilter));
        }
    }
}
