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
using GKCore.Options;
using GKUI.Themes;

namespace GKCore.Controllers
{
    public sealed class AssociationEditDlgController : DialogController<IAssociationEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IButton _btnPersonAdd;
        private ILabel _lblRelation;
        private ILabel _lblPerson;
        private IComboBox _cmbRelation;
        private ITextBox _txtPerson;
#pragma warning restore CS0649, IDE0044
        #endregion

        private GDMAssociation fAssociation;
        private GDMIndividualRecord fTempPerson;

        public GDMAssociation Association
        {
            get { return fAssociation; }
            set {
                if (fAssociation != value) {
                    fAssociation = value;
                    fTempPerson = fBase.Context.Tree.GetPtrValue(fAssociation);
                    UpdateView();
                }
            }
        }


        public AssociationEditDlgController(IAssociationEditDlg view) : base(view)
        {
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _cmbRelation.AddStrings(GlobalOptions.Instance.Relations);
            _cmbRelation.ReadOnly = false;
        }

        public override bool Accept()
        {
            try {
                string rel = _cmbRelation.Text.Trim();
                if (!string.IsNullOrEmpty(rel) && GlobalOptions.Instance.Relations.IndexOf(rel) < 0) {
                    GlobalOptions.Instance.Relations.Add(rel);
                }

                fAssociation.Relation = rel;
                fBase.Context.Tree.SetPtrValue(fAssociation, fTempPerson);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("AssociationEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            _cmbRelation.Text = fAssociation.Relation;
            _txtPerson.Text = (fTempPerson == null) ? "" : GKUtils.GetNameString(fTempPerson, false);
        }

        public async void SetPerson()
        {
            fTempPerson = await BaseController.SelectPerson(fView, fBase, null, TargetMode.tmNone, GDMSex.svUnknown);
            _txtPerson.Text = (fTempPerson == null) ? "" : GKUtils.GetNameString(fTempPerson, false);
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.Association));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _lblRelation.Text = LangMan.LS(LSID.Relation);
            _lblPerson.Text = LangMan.LS(LSID.Person);

            SetToolTip("btnPersonAdd", LangMan.LS(LSID.PersonAttachTip));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
            _btnPersonAdd.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
        }
    }
}
