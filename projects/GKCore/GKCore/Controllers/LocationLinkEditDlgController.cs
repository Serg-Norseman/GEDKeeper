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
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IButton _btnLocationAdd;
        private ILabel _lblLocation;
        private ITextBox _txtTopLevel;
        private ILabel _lblDate;
        private IDateControl _dateCtl;
#pragma warning restore CS0649, IDE0044
        #endregion

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
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _txtTopLevel.Activate();

            _dateCtl.DateChanged += new EventHandler(OnDateChanged);
        }

        public override bool Accept()
        {
            try {
                fBase.Context.Tree.SetPtrValue(fLocationLink, fTempLocation);

                try {
                    GDMCustomDate dt = _dateCtl.Date;
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
            _dateCtl.FixedDateType = GDMDateType.PeriodBetween;
            _dateCtl.Date = fLocationLink.Date.Value;
            UpdateLocationName();
        }

        public async void SetLocation()
        {
            fTempLocation = await BaseController.SelectRecord(fView, fBase, GDMRecordType.rtLocation, null) as GDMLocationRecord;
            UpdateLocationName();
        }

        public void UpdateLocationName()
        {
            _txtTopLevel.Text = (fTempLocation == null) ? "" : fTempLocation.GetNameByDate(_dateCtl.Date, true);
        }

        private void OnDateChanged(object sender, EventArgs e)
        {
            UpdateLocationName();
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.Location));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _lblLocation.Text = LangMan.LS(LSID.Location);
            _lblDate.Text = LangMan.LS(LSID.Date);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            _btnLocationAdd.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach);
        }
    }
}
