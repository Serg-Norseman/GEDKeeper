/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Names;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class NameEditDlgController : DialogController<INameEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ILabel _lblName;
        private ITextBox _txtName;
        private ILabel _lblSex;
        private IComboBox _cmbSex;
        private IGroupBox _grpPatronymics;
        private ILabel _lblFemale;
        private ITextBox _txtFPatr;
        private ILabel _lblMale;
        private ITextBox _txtMPatr;
#pragma warning restore CS0649, IDE0044
        #endregion

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
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _cmbSex.ReadOnly = true;
            for (GDMSex sx = GDMSex.svUnknown; sx <= GDMSex.svLast; sx++) {
                _cmbSex.Add(GKUtils.SexStr(sx));
            }
        }

        public override bool Accept()
        {
            try {
                fIName.Name = _txtName.Text;
                fIName.Sex = (GDMSex)_cmbSex.SelectedIndex;
                fIName.F_Patronymic = _txtFPatr.Text;
                fIName.M_Patronymic = _txtMPatr.Text;

                return true;
            } catch (Exception ex) {
                Logger.WriteError("NameEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            if (fIName == null) {
                _txtName.Text = "";
                _cmbSex.SelectedIndex = 0;
                _txtFPatr.Text = "";
                _txtMPatr.Text = "";
            } else {
                _txtName.Text = fIName.Name;
                _cmbSex.SelectedIndex = (sbyte)fIName.Sex;
                _txtFPatr.Text = fIName.F_Patronymic;
                _txtMPatr.Text = fIName.M_Patronymic;
            }
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.GivenName));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _lblName.Text = LangMan.LS(LSID.GivenName);
            _lblSex.Text = LangMan.LS(LSID.Sex);
            _grpPatronymics.Text = LangMan.LS(LSID.Patronymic);
            _lblFemale.Text = LangMan.LS(LSID.PatFemale);
            _lblMale.Text = LangMan.LS(LSID.PatMale);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
