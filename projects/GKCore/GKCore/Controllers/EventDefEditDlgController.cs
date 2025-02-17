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
using GDModel.Providers.GEDCOM;
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
    public sealed class EventDefEditDlgController : DialogController<IEventDefEditDlg>
    {
        private EventDef fEventDef;

        public EventDef EventDef
        {
            get { return fEventDef; }
            set {
                if (fEventDef != value) {
                    fEventDef = value;
                    UpdateView();
                }
            }
        }


        public EventDefEditDlgController(IEventDefEditDlg view) : base(view)
        {
        }

        public override bool Accept()
        {
            try {
                if (!fEventDef.Protected) {
                    fEventDef.DisplayName = fView.NameText.Text;
                    fEventDef.Tag = fView.TagCombo.Text;
                    fEventDef.Type = fView.TypeText.Text;
                    fEventDef.Description = fView.DescText.Text;
                }
                fEventDef.Enabled = fView.EnabledCheck.Checked;

                bool isCustom = (fEventDef.Tag == GEDCOMTagName.EVEN) || (fEventDef.Tag == GEDCOMTagName.FACT);
                if (isCustom && string.IsNullOrEmpty(fEventDef.Type)) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.TagTypeWarning));
                    return false;
                }

                return true;
            } catch (Exception ex) {
                Logger.WriteError("EventDefEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.NameText.Text = fEventDef.DisplayName;
            fView.TypeText.Text = fEventDef.Type;
            fView.EnabledCheck.Checked = fEventDef.Enabled;
            fView.DescText.Text = fEventDef.Description;

            string tag = fEventDef.Tag;
            bool isCustom = string.IsNullOrEmpty(tag) || (tag == GEDCOMTagName.EVEN) || (tag == GEDCOMTagName.FACT);

            if (isCustom) {
                // new event/fact tag
                fView.TagCombo.AddRange(new string[] { GEDCOMTagName.EVEN, GEDCOMTagName.FACT });
                fView.TagCombo.Text = string.IsNullOrEmpty(tag) ? GEDCOMTagName.FACT : tag;
            } else {
                // exists event/fact tag
                fView.TagCombo.AddRange(new string[] { fEventDef.Tag });
                fView.TagCombo.Text = fEventDef.Tag;
            }

            fView.NameText.Enabled = !fEventDef.Protected;
            fView.TagCombo.Enabled = !fEventDef.Protected;
            fView.TypeText.Enabled = !fEventDef.Protected;
            fView.DescText.Enabled = !fEventDef.Protected;
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.EventDefinition);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ILabel>("lblName").Text = LangMan.LS(LSID.GeneralName);
            GetControl<ILabel>("lblTag").Text = LangMan.LS(LSID.Tag);
            GetControl<ILabel>("lblType").Text = LangMan.LS(LSID.Type);
            GetControl<ICheckBox>("chkEnabled").Text = LangMan.LS(LSID.Enabled);
            GetControl<ILabel>("lblDesc").Text = LangMan.LS(LSID.Description);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
