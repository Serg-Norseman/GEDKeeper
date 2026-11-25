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
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class SourceCallNumberDlgController : DialogController<ISourceCallNumberEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ILabel _lblCallNumber;
        private ITextBox _txtNumber;
        private ILabel _lblMediaType;
        private IComboBox _cmbMediaType;
#pragma warning restore CS0649, IDE0044
        #endregion

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
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _cmbMediaType.ReadOnly = true;
            for (GDMMediaType mt = GDMMediaType.mtUnknown; mt <= GDMMediaType.mtLast; mt++) {
                _cmbMediaType.Add(LangMan.LS(GKData.MediaTypes[(int)mt]));
            }
            _txtNumber.Activate();
        }

        public override bool Accept()
        {
            try {
                fCallNumber.StringValue = _txtNumber.Text;
                fCallNumber.MediaType = (GDMMediaType)_cmbMediaType.SelectedIndex;

                if (!Validate(fCallNumber)) return false;

                return true;
            } catch (Exception ex) {
                Logger.WriteError("SourceCallNumberDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            _txtNumber.Text = fCallNumber.StringValue;
            _cmbMediaType.SelectedIndex = (int)fCallNumber.MediaType;
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.CallNumber));
            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _lblCallNumber.Text = LangMan.LS(LSID.CallNumber);
            _lblMediaType.Text = LangMan.LS(LSID.Type);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
