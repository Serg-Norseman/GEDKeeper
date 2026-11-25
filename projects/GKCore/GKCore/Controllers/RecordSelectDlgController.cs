/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Options;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class RecordSelectDlgController : DialogController<IRecordSelectDialog>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnSelect;
        private IButton _btnCreate;
        private IButton _btnCancel;
        private IComboBox _txtFastFilter;
        private IFilterControl _fltCtl;
#pragma warning restore CS0649, IDE0044
        #endregion

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

        public void ShowDetails()
        {
            GDMRecord rec = fView.RecordsList.GetSelectedData() as GDMRecord;
            if (rec == null) return;

            BaseController.ViewRecordInfo(fView, fBase, rec);
        }

        private void UpdateFilters()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                var filters = GlobalOptions.Instance.GetRSFilters(fRecType);
                filters.Sort();

                _txtFastFilter.Clear();
                _txtFastFilter.Add("*");
                _txtFastFilter.AddStrings(filters);
            }
        }

        private string GetFilter()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                return _txtFastFilter.Text;
            } else {
                return fView.FilterText.Text;
            }
        }

        private void SetFilter(string value)
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                _txtFastFilter.Text = value;
            } else {
                fView.FilterText.Text = value;
            }
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

            SetFilter(defFilter);

            UpdateView();
        }

        // select child for parent
        private bool ChildSelectorHandler(GDMRecord record)
        {
            // protection
            if (record == fTarget.TargetIndividual) return false;

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
            // protection
            if (record == fTarget.TargetIndividual) return false;

            if (GlobalOptions.Instance.UseBirthDatesInPersonSelectionFilter) {
                var probableParent = (GDMIndividualRecord)record;
                var child = fTarget.TargetIndividual;
                return (probableParent.GetUDN(GEDCOMTagType.BIRT).CompareTo(child.GetUDN(GEDCOMTagType.BIRT)) < 0);
            }
            return true;
        }

        private bool SpouseSelectorHandler(GDMRecord record)
        {
            var famRec = (GDMFamilyRecord)record;
            return (famRec != null && famRec.HasSpouse(fTarget.TargetIndividual));
        }

        public void ChangeFilter()
        {
            string flt = GetFilter();
            GKUtils.SaveFilter(flt, GlobalOptions.Instance.GetRSFilters(fRecType));
            UpdateFilters();
        }

        public override void UpdateView()
        {
            string flt = GetFilter();
            if (string.IsNullOrEmpty(flt)) {
                flt = "*";
            } else if (flt != "*") {
                flt = "*" + flt + "*";
            }

            IListView recordsList = fView.RecordsList;
            recordsList.ListMan.Filter.Clear();

            var quickFilter = ((IRecordsListModel)recordsList.ListMan).QuickFilter;
            quickFilter.Value = flt;
            if (_fltCtl != null) _fltCtl.Params = quickFilter;

            switch (fRecType) {
                case GDMRecordType.rtIndividual: {
                        IndividualListFilter iFilter = (IndividualListFilter)recordsList.ListMan.Filter;
                        iFilter.Sex = fTarget.NeedSex;
                        switch (fTarget.TargetMode) {
                            case TargetMode.tmParent:
                                recordsList.ListMan.ExternalFilter = ChildSelectorHandler;
                                break;
                            case TargetMode.tmChild:
                                recordsList.ListMan.ExternalFilter = ParentSelectorHandler;
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

            UpdateButtons();
        }

        private void UpdateButtons()
        {
            _btnSelect.Enabled = fView.RecordsList.ListMan.FilteredCount > 0;
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.WinRecordSelect));

            _btnCreate.Text = LangMan.LS(LSID.DlgAppend);
            _btnSelect.Text = LangMan.LS(LSID.DlgSelect);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);

            SetToolTip("txtFastFilter", LangMan.LS(LSID.PressEnterToSaveFilter));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnCreate.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_ItemAdd);
            _btnSelect.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
