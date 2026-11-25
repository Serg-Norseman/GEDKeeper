/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Filters;
using GKCore.Lists;
using GKCore.Locales;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class CommonFilterDlgController<T> : DialogController<T>
        where T : ICommonFilterDlg
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IButton _btnReset;
        private ITabPage _pageFieldsFilter;
        private IFilterGridView _filterView;
#pragma warning restore CS0649, IDE0044
        #endregion

        private readonly IRecordsListModel fListMan;

        public CommonFilterDlgController(T view, IRecordsListModel listMan) : base(view)
        {
            fListMan = listMan;
        }

        public override bool Accept()
        {
            try {
                fListMan.Filter.Clear();

                int num = _filterView.Count;
                for (int r = 0; r < num; r++) {
                    ColumnConditionExpression fcond = _filterView[r];
                    if (fcond != null) {
                        fListMan.AddCondition((byte)fcond.ColumnIndex, fcond.Operator, fcond.Value.ToString());
                    }
                }

                return true;
            } catch (Exception ex) {
                Logger.WriteError("CommonFilterDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            _filterView.Clear();
            int num = fListMan.Filter.Conditions.Count;
            for (int i = 0; i < num; i++) {
                ColumnConditionExpression fcond = fListMan.Filter.Conditions[i];
                _filterView.AddCondition(fcond);
            }
        }

        public void Reset()
        {
            fListMan.Filter.Clear();
            UpdateView();
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.MIFilter));

            GKData.CondSigns[6] = LangMan.LS(LSID.CondContains);
            GKData.CondSigns[7] = LangMan.LS(LSID.CondNotContains);
            GKData.CondSigns[8] = LangMan.LS(LSID.CondContainsMask);
            GKData.CondSigns[9] = LangMan.LS(LSID.CondNotContainsMask);

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _btnReset.Text = LangMan.LS(LSID.DlgReset);
            _pageFieldsFilter.Text = LangMan.LS(LSID.FieldsFilter);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public class CommonFilterDlgController : CommonFilterDlgController<ICommonFilterDlg>
    {
        public CommonFilterDlgController(ICommonFilterDlg view, IRecordsListModel listMan) : base(view, listMan)
        {
        }
    }
}
