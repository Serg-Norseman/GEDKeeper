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
    public sealed class CommonFilterDlgController : DialogController<ICommonFilterDlg>
    {
        private readonly IRecordsListModel fListMan;

        public CommonFilterDlgController(ICommonFilterDlg view, IRecordsListModel listMan) : base(view)
        {
            fListMan = listMan;
        }

        public override bool Accept()
        {
            try {
                fListMan.Filter.Clear();

                int num = fView.FilterGrid.Count;
                for (int r = 0; r < num; r++) {
                    ColumnConditionExpression fcond = fView.FilterGrid[r];
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
            fView.FilterGrid.Clear();
            int num = fListMan.Filter.Conditions.Count;
            for (int i = 0; i < num; i++) {
                ColumnConditionExpression fcond = fListMan.Filter.Conditions[i];
                fView.FilterGrid.AddCondition(fcond);
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

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<IButton>("btnReset").Text = LangMan.LS(LSID.DlgReset);
            GetControl<ITabPage>("pageFieldsFilter").Text = LangMan.LS(LSID.FieldsFilter);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
