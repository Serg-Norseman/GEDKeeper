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
    public sealed class ParentsEditDlgController : DialogController<IParentsEditDlg>
    {
        private GDMChildToFamilyLink fChildLink;
        private GDMIndividualRecord fIndividualRecord;

        public GDMChildToFamilyLink ChildLink
        {
            get { return fChildLink; }
            set {
                if (fChildLink != value) {
                    fChildLink = value;
                    UpdateView();
                }
            }
        }

        public GDMIndividualRecord IndividualRecord
        {
            get { return fIndividualRecord; }
            set { fIndividualRecord = value; }
        }


        public ParentsEditDlgController(IParentsEditDlg view) : base(view)
        {
            for (GDMPedigreeLinkageType plt = GDMPedigreeLinkageType.plNone; plt <= GDMPedigreeLinkageType.plFoster; plt++) {
                fView.LinkageTypeCombo.Add(LangMan.LS(GKData.ParentTypes[(int)plt]));
            }
        }

        public override bool Accept()
        {
            try {
                fChildLink.PedigreeLinkageType = (GDMPedigreeLinkageType)fView.LinkageTypeCombo.SelectedIndex;

                fLocalUndoman.Commit();

                fBase.NotifyRecord(fIndividualRecord, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("ParentsEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            try {
                fView.ChildName.Text = GKUtils.GetNameString(fIndividualRecord, false);
                fView.LinkageTypeCombo.SelectedIndex = (sbyte)fChildLink.PedigreeLinkageType;

                UpdateControls();
            } catch (Exception ex) {
                Logger.WriteError("ParentsEditDlgController.UpdateView()", ex);
            }
        }

        public void UpdateControls()
        {
            if (fChildLink != null) {
                GDMFamilyRecord family = fBase.Context.Tree.GetPtrValue(fChildLink);
                SetParentsAvl(true);

                GDMIndividualRecord father, mother;
                fBase.Context.Tree.GetSpouses(family, out father, out mother);

                if (father != null) {
                    SetFatherAvl(true);
                    fView.Father.Text = GKUtils.GetNameString(father, false);
                } else {
                    SetFatherAvl(false);
                    fView.Father.Text = "";
                }

                if (mother != null) {
                    SetMotherAvl(true);
                    fView.Mother.Text = GKUtils.GetNameString(mother, false);
                } else {
                    SetMotherAvl(false);
                    fView.Mother.Text = "";
                }
            } else {
                SetParentsAvl(false);
                SetFatherAvl(false);
                SetMotherAvl(false);

                fView.Father.Text = "";
                fView.Mother.Text = "";
            }
        }

        public async void EditParents()
        {
            GDMFamilyRecord family = await fBase.Context.GetChildFamily(fIndividualRecord, false, null);
            if (family != null) {
                var famRes = await BaseController.ModifyFamily(fView, fBase, family, TargetMode.tmNone, null);
                if (famRes.Result) {
                    UpdateControls();
                }
            }
        }

        public async void AddFather()
        {
            if (await BaseController.AddIndividualFather(fView, fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        public async void DeleteFather()
        {
            if (await BaseController.DeleteIndividualFather(fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        public async void AddMother()
        {
            if (await BaseController.AddIndividualMother(fView, fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        public async void DeleteMother()
        {
            if (await BaseController.DeleteIndividualMother(fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        private void SetParentsAvl(bool avail)
        {
            GetControl<IButton>("btnParentsEdit").Enabled = avail;
        }

        private void SetFatherAvl(bool avail)
        {
            GetControl<IButton>("btnFatherAdd").Enabled = !avail;
            GetControl<IButton>("btnFatherDelete").Enabled = avail;
        }

        private void SetMotherAvl(bool avail)
        {
            GetControl<IButton>("btnMotherAdd").Enabled = !avail;
            GetControl<IButton>("btnMotherDelete").Enabled = avail;
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.WinPersonEdit);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ILabel>("lblChildName").Text = LangMan.LS(LSID.GeneralName);
            GetControl<ILabel>("lblParents").Text = LangMan.LS(LSID.Parents);
            GetControl<ILabel>("lblLinkageType").Text = LangMan.LS(LSID.LinkageType);

            SetToolTip("btnParentsEdit", LangMan.LS(LSID.ParentsEditTip));
            SetToolTip("btnFatherAdd", LangMan.LS(LSID.FatherAddTip));
            SetToolTip("btnFatherDelete", LangMan.LS(LSID.FatherDeleteTip));
            SetToolTip("btnMotherAdd", LangMan.LS(LSID.MotherAddTip));
            SetToolTip("btnMotherDelete", LangMan.LS(LSID.MotherDeleteTip));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            GetControl<IButton>("btnParentsEdit").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_ItemEdit, true);

            GetControl<IButton>("btnFatherAdd").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
            GetControl<IButton>("btnFatherDelete").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Detach, true);

            GetControl<IButton>("btnMotherAdd").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
            GetControl<IButton>("btnMotherDelete").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Detach, true);
        }
    }
}
