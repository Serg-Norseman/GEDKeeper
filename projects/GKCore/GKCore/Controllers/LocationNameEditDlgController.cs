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
    public sealed class LocationNameEditDlgController : DialogController<ILocationNameEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ILabel _lblTitle;
        private ITextBox _txtTitle;
        private ILabel _lblShortTitle;
        private ITextBox _txtShortTitle;
        private ILabel _lblDate;
        private IDateControl _dateCtl;
#pragma warning restore CS0649, IDE0044
        #endregion

        private GDMLocationName fLocationName;

        public GDMLocationName LocationName
        {
            get { return fLocationName; }
            set {
                if (fLocationName != value) {
                    fLocationName = value;
                    UpdateView();
                }
            }
        }


        public LocationNameEditDlgController(ILocationNameEditDlg view) : base(view)
        {
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _txtTitle.Activate();
        }

        public override bool Accept()
        {
            try {
                fLocationName.StringValue = _txtTitle.Text;
                fLocationName.Abbreviation = _txtShortTitle.Text;

                try {
                    GDMCustomDate dt = _dateCtl.Date;
                    if (dt == null) throw new ArgumentNullException("dt");

                    fLocationName.Date.ParseString(dt.StringValue);
                } catch (Exception) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.DateInvalid));
                    throw;
                }

                fLocalUndoman.Commit();

                return true;
            } catch (Exception ex) {
                Logger.WriteError("LocationNameEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            _dateCtl.FixedDateType = GDMDateType.PeriodBetween;

            _txtTitle.Text = fLocationName.StringValue;
            _txtShortTitle.Text = fLocationName.Abbreviation;
            _dateCtl.Date = fLocationName.Date.Value;
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.Location));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _lblTitle.Text = LangMan.LS(LSID.Title);
            _lblShortTitle.Text = LangMan.LS(LSID.ShortTitle);
            _lblDate.Text = LangMan.LS(LSID.Date);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
