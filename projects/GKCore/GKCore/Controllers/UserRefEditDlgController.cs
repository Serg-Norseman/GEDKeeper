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
    public sealed class UserRefEditDlgController : DialogController<IUserRefEditDlg>
    {
        private GDMUserReference fUserReference;

        public GDMUserReference UserReference
        {
            get { return fUserReference; }
            set {
                if (fUserReference != value) {
                    fUserReference = value;
                    UpdateView();
                }
            }
        }


        public UserRefEditDlgController(IUserRefEditDlg view) : base(view)
        {
            fView.Ref.Add("");
            for (SpecialUserRef ur = SpecialUserRef.urRI_StGeorgeCross; ur <= SpecialUserRef.urLast; ur++) {
                string sur = LangMan.LS(GKData.SpecialUserRefs[(int)ur].Title);
                fView.Ref.Add(sur);
            }
            fView.Ref.ReadOnly = false;

            fView.RefType.Add("");
            fView.RefType.Add(LangMan.LS(GKData.URTreeNoteType));
            fView.RefType.ReadOnly = false;
        }

        public override bool Accept()
        {
            try {
                fUserReference.StringValue = fView.Ref.Text;
                fUserReference.ReferenceType = fView.RefType.Text;

                return true;
            } catch (Exception ex) {
                Logger.WriteError("UserRefEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.Ref.Text = fUserReference.StringValue;
            fView.RefType.Text = fUserReference.ReferenceType;
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.WinUserRefEdit);
            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ILabel>("lblReference").Text = LangMan.LS(LSID.Reference);
            GetControl<ILabel>("lblRefType").Text = LangMan.LS(LSID.Type);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
