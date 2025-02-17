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
                } catch (Exception ex) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.DateInvalid));
                    throw ex;
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
            fTempLocation = await fBase.Context.SelectRecord(fView, GDMRecordType.rtLocation, null) as GDMLocationRecord;
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
            fView.Title = LangMan.LS(LSID.Location);

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
        }
    }
}
