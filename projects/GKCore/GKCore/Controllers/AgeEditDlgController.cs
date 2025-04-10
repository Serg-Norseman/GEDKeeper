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
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class AgeEditDlgController : DialogController<IAgeEditDlg>
    {
        private GDMAge fAge;


        public GDMAge Age
        {
            get { return fAge; }
            set {
                if (fAge != value) {
                    fAge = value;
                    UpdateView();
                }
            }
        }


        public AgeEditDlgController(IAgeEditDlg view) : base(view)
        {
            for (int i = 0; i < GEDCOMConsts.AgeRelatives.Length; i++) {
                fView.RelativeCombo.AddItem(GEDCOMConsts.AgeRelatives[i], i - 1);
            }

            fView.RelativeCombo.Activate();
        }

        public override bool Accept()
        {
            try {
                fAge.Relative = fView.RelativeCombo.GetSelectedTag<int>();
                fAge.StringValue = fView.ValueText.Text;

                CommitChanges();

                return true;
            } catch (Exception ex) {
                Logger.WriteError("AgeEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.RelativeCombo.SetSelectedTag(fAge.Relative);
            fView.ValueText.Text = fAge.StringValue;
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.Age);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);

            //GetControl<ILabel>("lblRelative").Text = LangMan.LS(LSID.AgeRelative);
            GetControl<ILabel>("lblValue").Text = LangMan.LS(LSID.Value);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
