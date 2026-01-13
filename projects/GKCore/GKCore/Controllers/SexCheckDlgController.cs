/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
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
    public sealed class SexCheckDlgController : DialogController<ISexCheckDlg>
    {
        public GDMSex Sex
        {
            get {
                if (GetControl<IRadioButton>("rbMale").Checked) {
                    return GDMSex.svMale;
                }
                if (GetControl<IRadioButton>("rbFemale").Checked) {
                    return GDMSex.svFemale;
                }
                return GDMSex.svUnknown;
            }
            set {
                switch (value) {
                    case GDMSex.svUnknown:
                    case GDMSex.svIntersex:
                        GetControl<IRadioButton>("rbNone").Checked = true;
                        break;

                    case GDMSex.svMale:
                        GetControl<IRadioButton>("rbMale").Checked = true;
                        break;

                    case GDMSex.svFemale:
                        GetControl<IRadioButton>("rbFemale").Checked = true;
                        break;
                }
            }
        }


        public SexCheckDlgController(ISexCheckDlg view) : base(view)
        {
        }

        public override void UpdateView()
        {
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.WinCheckSex));

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);

            GetControl<IGroupBox>("grpSex").Text = LangMan.LS(LSID.Sex);
            GetControl<IRadioButton>("rbNone").Text = " ? ";
            GetControl<IRadioButton>("rbMale").Text = LangMan.LS(LSID.SexM);
            GetControl<IRadioButton>("rbFemale").Text = LangMan.LS(LSID.SexF);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
