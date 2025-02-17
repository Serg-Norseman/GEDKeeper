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

using System;
using GKCore.Design.Controls;
using GKCore.Interfaces;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Types;
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
                    FilterCondition fcond = fView.FilterGrid[r];
                    if (fcond != null) {
                        fListMan.AddCondition((byte)fcond.ColumnIndex, fcond.Condition, fcond.Value.ToString());
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
                FilterCondition fcond = fListMan.Filter.Conditions[i];
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
            fView.Title = LangMan.LS(LSID.MIFilter);

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
