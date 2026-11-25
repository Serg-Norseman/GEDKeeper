/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Events;
using GKCore.Locales;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class EventDefEditDlgController : DialogController<IEventDefEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ILabel _lblName;
        private ITextBox _txtName;
        private ILabel _lblTag;
        private IComboBox _cmbTag;
        private ILabel _lblType;
        private ITextBox _txtType;
        private ICheckBox _chkEnabled;
        private ILabel _lblDesc;
        private ITextBox _txtDesc;
#pragma warning restore CS0649, IDE0044
        #endregion

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

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _cmbTag.ReadOnly = true;
        }

        public override bool Accept()
        {
            try {
                if (!fEventDef.Protected) {
                    fEventDef.DisplayName = _txtName.Text;
                    fEventDef.Tag = _cmbTag.Text;
                    fEventDef.Type = _txtType.Text;
                    fEventDef.Description = _txtDesc.Text;
                }
                fEventDef.Enabled = _chkEnabled.Checked;

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
            _txtName.Text = fEventDef.DisplayName;
            _txtType.Text = fEventDef.Type;
            _chkEnabled.Checked = fEventDef.Enabled;
            _txtDesc.Text = fEventDef.Description;

            string tag = fEventDef.Tag;
            bool isCustom = string.IsNullOrEmpty(tag) || (tag == GEDCOMTagName.EVEN) || (tag == GEDCOMTagName.FACT);

            if (isCustom) {
                // new event/fact tag
                _cmbTag.AddRange(new string[] { GEDCOMTagName.EVEN, GEDCOMTagName.FACT });
                _cmbTag.Text = string.IsNullOrEmpty(tag) ? GEDCOMTagName.FACT : tag;
            } else {
                // exists event/fact tag
                _cmbTag.AddRange(new string[] { fEventDef.Tag });
                _cmbTag.Text = fEventDef.Tag;
            }

            _txtName.Enabled = !fEventDef.Protected;
            _cmbTag.Enabled = !fEventDef.Protected;
            _txtType.Enabled = !fEventDef.Protected;
            _txtDesc.Enabled = !fEventDef.Protected;
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.EventDefinition));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _lblName.Text = LangMan.LS(LSID.GeneralName);
            _lblTag.Text = LangMan.LS(LSID.Tag);
            _lblType.Text = LangMan.LS(LSID.Type);
            _chkEnabled.Text = LangMan.LS(LSID.Enabled);
            _lblDesc.Text = LangMan.LS(LSID.Description);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
