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
using GKCore.Names;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class NameEditDlgController : DialogController<INameEditDlg>
    {
        private NameEntry fIName;

        public NameEntry NameEntry
        {
            get { return fIName; }
            set {
                if (fIName != value) {
                    fIName = value;
                    UpdateView();
                }
            }
        }


        public NameEditDlgController(INameEditDlg view) : base(view)
        {
            for (GDMSex sx = GDMSex.svUnknown; sx <= GDMSex.svLast; sx++) {
                fView.SexCombo.Add(GKUtils.SexStr(sx));
            }
        }

        public override bool Accept()
        {
            try {
                fIName.Name = fView.Name.Text;
                fIName.Sex = (GDMSex)fView.SexCombo.SelectedIndex;
                fIName.F_Patronymic = fView.FPatr.Text;
                fIName.M_Patronymic = fView.MPatr.Text;

                return true;
            } catch (Exception ex) {
                Logger.WriteError("NameEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            if (fIName == null) {
                fView.Name.Text = "";
                fView.SexCombo.SelectedIndex = 0;
                fView.FPatr.Text = "";
                fView.MPatr.Text = "";
            } else {
                fView.Name.Text = fIName.Name;
                fView.SexCombo.SelectedIndex = (sbyte)fIName.Sex;
                fView.FPatr.Text = fIName.F_Patronymic;
                fView.MPatr.Text = fIName.M_Patronymic;
            }
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.GivenName);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ILabel>("lblName").Text = LangMan.LS(LSID.GivenName);
            GetControl<ILabel>("lblSex").Text = LangMan.LS(LSID.Sex);
            GetControl<IGroupBox>("grpPatronymics").Text = LangMan.LS(LSID.Patronymic);
            GetControl<ILabel>("lblFemale").Text = LangMan.LS(LSID.PatFemale);
            GetControl<ILabel>("lblMale").Text = LangMan.LS(LSID.PatMale);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
