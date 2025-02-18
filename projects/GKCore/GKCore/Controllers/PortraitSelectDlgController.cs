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
using BSLib;
using GDModel;
using GKCore.Design.Graphics;
using GKCore.Design.Controls;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class PortraitSelectDlgController : DialogController<IPortraitSelectDlg>
    {
        private GDMMultimediaLink fMultimediaLink;

        public GDMMultimediaLink MultimediaLink
        {
            get { return fMultimediaLink; }
            set {
                if (fMultimediaLink != value) {
                    fMultimediaLink = value;
                    UpdateView();
                }
            }
        }


        public PortraitSelectDlgController(IPortraitSelectDlg view) : base(view)
        {
        }

        public override bool Accept()
        {
            try {
                BaseController.SetMultimediaLinkRegion(fMultimediaLink, fView.ImageCtl.SelectionRegion, true);

                var uid = fMultimediaLink.GetUID(fBase.Context.Tree);
                PortraitsCache.Instance.RemoveObsolete(uid);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("PortraitSelectDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            GDMMultimediaRecord mmRec = fBase.Context.Tree.GetPtrValue<GDMMultimediaRecord>(fMultimediaLink);
            if (fMultimediaLink == null || mmRec == null) return;

            IImage img = fBase.Context.LoadMediaImage(mmRec, -1, -1, ExtRect.Empty, false);
            if (img == null) return;

            fView.ImageCtl.OpenImage(null, img);

            if (fMultimediaLink.IsPrimaryCutout) {
                fView.ImageCtl.SelectionRegion = fMultimediaLink.CutoutPosition.Value;
            }
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.PortraitSelect);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            fView.ImageCtl.ApplyTheme();
        }
    }
}
