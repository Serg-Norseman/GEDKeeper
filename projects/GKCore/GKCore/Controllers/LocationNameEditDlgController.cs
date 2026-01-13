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
            fView.NameText.Activate();
        }

        public override bool Accept()
        {
            try {
                fLocationName.StringValue = fView.NameText.Text;
                fLocationName.Abbreviation = fView.AbbrText.Text;

                try {
                    GDMCustomDate dt = fView.DateCtl.Date;
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
            fView.DateCtl.FixedDateType = GDMDateType.PeriodBetween;

            fView.NameText.Text = fLocationName.StringValue;
            fView.AbbrText.Text = fLocationName.Abbreviation;
            fView.DateCtl.Date = fLocationName.Date.Value;
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.Location));

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ILabel>("lblTitle").Text = LangMan.LS(LSID.Title);
            GetControl<ILabel>("lblShortTitle").Text = LangMan.LS(LSID.ShortTitle);
            GetControl<ILabel>("lblDate").Text = LangMan.LS(LSID.Date);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
