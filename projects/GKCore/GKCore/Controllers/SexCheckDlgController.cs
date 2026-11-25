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
        #region Design components
#pragma warning disable CS0649, IDE0044
        private ITextBox _txtName;
        private IGroupBox _grpSex;
        private IRadioButton _rbNone;
        private IRadioButton _rbMale;
        private IRadioButton _rbFemale;
        private IButton _btnAccept;
        private IButton _btnCancel;
#pragma warning restore CS0649, IDE0044
        #endregion

        public GDMSex Sex
        {
            get {
                if (_rbMale.Checked) {
                    return GDMSex.svMale;
                }
                if (_rbFemale.Checked) {
                    return GDMSex.svFemale;
                }
                return GDMSex.svUnknown;
            }
            set {
                switch (value) {
                    case GDMSex.svUnknown:
                    case GDMSex.svIntersex:
                        _rbNone.Checked = true;
                        break;

                    case GDMSex.svMale:
                        _rbMale.Checked = true;
                        break;

                    case GDMSex.svFemale:
                        _rbFemale.Checked = true;
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

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);

            _grpSex.Text = LangMan.LS(LSID.Sex);
            _rbNone.Text = " ? ";
            _rbMale.Text = LangMan.LS(LSID.SexM);
            _rbFemale.Text = LangMan.LS(LSID.SexF);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
