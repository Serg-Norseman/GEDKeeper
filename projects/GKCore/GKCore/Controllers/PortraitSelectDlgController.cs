/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Media;
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

            IImage img = fBase.Context.LoadMediaImage(mmRec, 0, -1, -1, ExtRect.Empty, false, false);
            if (img == null) return;

            fView.ImageCtl.OpenImage(null, img);

            if (fMultimediaLink.IsPrimaryCutout) {
                fView.ImageCtl.SelectionRegion = fMultimediaLink.CutoutPosition.Value;
            }
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.PortraitSelect));

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
