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
    public sealed class LocationLinkEditDlgController : DialogController<ILocationLinkEditDlg>
    {
        private GDMLocationLink fLocationLink;
        private GDMLocationRecord fTempLocation;

        public GDMLocationLink LocationLink
        {
            get { return fLocationLink; }
            set {
                if (fLocationLink != value) {
                    fLocationLink = value;
                    fTempLocation = fBase.Context.Tree.GetPtrValue<GDMLocationRecord>(fLocationLink);
                    UpdateView();
                }
            }
        }


        public LocationLinkEditDlgController(ILocationLinkEditDlg view) : base(view)
        {
            fView.TopLevelText.Activate();

            fView.DateCtl.DateChanged += new EventHandler(OnDateChanged);
        }

        public override bool Accept()
        {
            try {
                fBase.Context.Tree.SetPtrValue(fLocationLink, fTempLocation);

                try {
                    GDMCustomDate dt = fView.DateCtl.Date;
                    if (dt == null) throw new ArgumentNullException("dt");

                    fLocationLink.Date.ParseString(dt.StringValue);
                } catch (Exception) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.DateInvalid));
                    throw;
                }

                fLocalUndoman.Commit();

                return true;
            } catch (Exception ex) {
                Logger.WriteError("LocationLinkEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.DateCtl.FixedDateType = GDMDateType.PeriodBetween;
            fView.DateCtl.Date = fLocationLink.Date.Value;
            UpdateLocationName();
        }

        public async void SetLocation()
        {
            fTempLocation = await BaseController.SelectRecord(fView, fBase, GDMRecordType.rtLocation, null) as GDMLocationRecord;
            UpdateLocationName();
        }

        public void UpdateLocationName()
        {
            fView.TopLevelText.Text = (fTempLocation == null) ? "" : fTempLocation.GetNameByDate(fView.DateCtl.Date, true);
        }

        private void OnDateChanged(object sender, EventArgs e)
        {
            UpdateLocationName();
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.Location));

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ILabel>("lblLocation").Text = LangMan.LS(LSID.Location);
            GetControl<ILabel>("lblDate").Text = LangMan.LS(LSID.Date);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            GetControl<IButton>("btnLocationAdd").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach);
        }
    }
}
