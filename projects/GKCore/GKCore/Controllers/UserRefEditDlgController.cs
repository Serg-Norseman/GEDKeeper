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
    public sealed class UserRefEditDlgController : DialogController<IUserRefEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ILabel _lblReference;
        private IComboBox _cmbRef;
        private ILabel _lblRefType;
        private IComboBox _cmbRefType;
#pragma warning restore CS0649, IDE0044
        #endregion

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
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _cmbRef.Add("");
            for (SpecialUserRef ur = SpecialUserRef.urRI_StGeorgeCross; ur <= SpecialUserRef.urLast; ur++) {
                string sur = LangMan.LS(GKData.SpecialUserRefs[(int)ur].Title);
                _cmbRef.Add(sur);
            }
            _cmbRef.ReadOnly = false;

            _cmbRefType.Add("");
            _cmbRefType.Add(LangMan.LS(GKData.URTreeNoteType));
            _cmbRefType.ReadOnly = false;
        }

        public override bool Accept()
        {
            try {
                fUserReference.StringValue = _cmbRef.Text;
                fUserReference.ReferenceType = _cmbRefType.Text;

                if (!Validate(fUserReference)) return false;

                return true;
            } catch (Exception ex) {
                Logger.WriteError("UserRefEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            _cmbRef.Text = fUserReference.StringValue;
            _cmbRefType.Text = fUserReference.ReferenceType;
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.UserRef));
            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _lblReference.Text = LangMan.LS(LSID.Reference);
            _lblRefType.Text = LangMan.LS(LSID.Type);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
