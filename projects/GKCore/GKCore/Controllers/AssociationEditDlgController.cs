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
using GKCore.Options;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class AssociationEditDlgController : DialogController<IAssociationEditDlg>
    {
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
            fView.Relation.AddStrings(GlobalOptions.Instance.Relations);
            fView.Relation.ReadOnly = false;
        }

        public override bool Accept()
        {
            try {
                string rel = fView.Relation.Text.Trim();
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
            fView.Relation.Text = fAssociation.Relation;
            fView.Person.Text = (fTempPerson == null) ? "" : GKUtils.GetNameString(fTempPerson, false);
        }

        public async void SetPerson()
        {
            fTempPerson = await fBase.Context.SelectPerson(fView, null, TargetMode.tmNone, GDMSex.svUnknown);
            fView.Person.Text = (fTempPerson == null) ? "" : GKUtils.GetNameString(fTempPerson, false);
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.Association);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ILabel>("lblRelation").Text = LangMan.LS(LSID.Relation);
            GetControl<ILabel>("lblPerson").Text = LangMan.LS(LSID.Person);

            SetToolTip("btnPersonAdd", LangMan.LS(LSID.PersonAttachTip));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            GetControl<IButton>("btnPersonAdd").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
        }
    }
}
