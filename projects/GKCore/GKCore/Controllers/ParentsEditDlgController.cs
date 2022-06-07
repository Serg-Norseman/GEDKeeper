/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

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
                fView.ChildName.Text = GKUtils.GetNameString(fIndividualRecord, true, false);
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
                    fView.Father.Text = GKUtils.GetNameString(father, true, false);
                } else {
                    SetFatherAvl(false);
                    fView.Father.Text = "";
                }

                if (mother != null) {
                    SetMotherAvl(true);
                    fView.Mother.Text = GKUtils.GetNameString(mother, true, false);
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

        public void EditParents()
        {
            GDMFamilyRecord family = fBase.Context.GetChildFamily(fIndividualRecord, false, null);
            if (family != null && BaseController.ModifyFamily(fBase, ref family, TargetMode.tmNone, null)) {
                UpdateControls();
            }
        }

        public void AddFather()
        {
            if (BaseController.AddIndividualFather(fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        public void DeleteFather()
        {
            if (BaseController.DeleteIndividualFather(fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        public void AddMother()
        {
            if (BaseController.AddIndividualMother(fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        public void DeleteMother()
        {
            if (BaseController.DeleteIndividualMother(fBase, fLocalUndoman, fIndividualRecord)) {
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
            fView.Title = LangMan.LS(LSID.LSID_WinPersonEdit);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.LSID_DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.LSID_DlgCancel);
            GetControl<ILabel>("lblChildName").Text = LangMan.LS(LSID.LSID_Name);
            GetControl<ILabel>("lblParents").Text = LangMan.LS(LSID.LSID_Parents);
            GetControl<ILabel>("lblLinkageType").Text = LangMan.LS(LSID.LSID_LinkageType);

            SetToolTip("btnParentsEdit", LangMan.LS(LSID.LSID_ParentsEditTip));
            SetToolTip("btnFatherAdd", LangMan.LS(LSID.LSID_FatherAddTip));
            SetToolTip("btnFatherDelete", LangMan.LS(LSID.LSID_FatherDeleteTip));
            SetToolTip("btnMotherAdd", LangMan.LS(LSID.LSID_MotherAddTip));
            SetToolTip("btnMotherDelete", LangMan.LS(LSID.LSID_MotherDeleteTip));
        }
    }
}
