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
    public sealed class SourceCallNumberDlgController : DialogController<ISourceCallNumberEditDlg>
    {
        private GDMSourceCallNumber fCallNumber;

        public GDMSourceCallNumber CallNumber
        {
            get { return fCallNumber; }
            set {
                if (fCallNumber != value) {
                    fCallNumber = value;
                    UpdateView();
                }
            }
        }


        public SourceCallNumberDlgController(ISourceCallNumberEditDlg view) : base(view)
        {
            for (GDMMediaType mt = GDMMediaType.mtUnknown; mt <= GDMMediaType.mtLast; mt++) {
                fView.MediaTypeCombo.Add(LangMan.LS(GKData.MediaTypes[(int)mt]));
            }
            fView.CallNumberText.Activate();
        }

        public override bool Accept()
        {
            try {
                fCallNumber.StringValue = fView.CallNumberText.Text;
                fCallNumber.MediaType = (GDMMediaType)fView.MediaTypeCombo.SelectedIndex;
                return true;
            } catch (Exception ex) {
                Logger.WriteError("SourceCallNumberDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.CallNumberText.Text = fCallNumber.StringValue;
            fView.MediaTypeCombo.SelectedIndex = (int)fCallNumber.MediaType;
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.CallNumber);
            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ILabel>("lblCallNumber").Text = LangMan.LS(LSID.CallNumber);
            GetControl<ILabel>("lblMediaType").Text = LangMan.LS(LSID.Type);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
