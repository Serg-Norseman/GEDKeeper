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
            for (GDMMediaType mt = GDMMediaType.mtUnknown; mt <= GDMMediaType.mtLast; mt++) {
                fView.MediaTypeCombo.Add(LangMan.LS(GKData.MediaTypes[(int)mt]));
            }
            fView.CallNumberText.Activate();
        }

        public override bool Accept()
        {
            try {
                fCallNumber.StringValue = fView.CallNumberText.Text;
                fCallNumber.MediaType = (GDMMediaType)fView.MediaTypeCombo.SelectedIndex;

                if (!Validate(fCallNumber)) return false;

                return true;
            } catch (Exception ex) {
                Logger.WriteError("SourceCallNumberDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.CallNumberText.Text = fCallNumber.StringValue;
            fView.MediaTypeCombo.SelectedIndex = (int)fCallNumber.MediaType;
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.CallNumber));
            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ILabel>("lblCallNumber").Text = LangMan.LS(LSID.CallNumber);
            GetControl<ILabel>("lblMediaType").Text = LangMan.LS(LSID.Type);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
