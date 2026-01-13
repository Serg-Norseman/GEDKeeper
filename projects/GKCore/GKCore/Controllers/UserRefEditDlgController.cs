/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore.Charts;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;
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

                if (!Validate(fUserReference)) return false;

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
            fView.SetTitle(LangMan.LS(LSID.WinUserRefEdit));
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
