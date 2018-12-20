/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using GKCommon.GEDCOM;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Operations;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ParentsEditDlgController : DialogController<IParentsEditDlg>
    {
        private GEDCOMChildToFamilyLink fLink;
        private GEDCOMIndividualRecord fPerson;

        public GEDCOMChildToFamilyLink Link
        {
            get { return fLink; }
            set {
                if (fLink != value) {
                    fLink = value;
                    UpdateView();
                }
            }
        }

        public GEDCOMIndividualRecord Person
        {
            get { return fPerson; }
            set { fPerson = value; }
        }


        public ParentsEditDlgController(IParentsEditDlg view) : base(view)
        {
            for (GEDCOMPedigreeLinkageType plt = GEDCOMPedigreeLinkageType.plNone; plt <= GEDCOMPedigreeLinkageType.plSealing; plt++) {
                fView.LinkageTypeCombo.Add(LangMan.LS(GKData.ParentTypes[(int)plt]));
            }
        }

        public override bool Accept()
        {
            try {
                fLink.PedigreeLinkageType = (GEDCOMPedigreeLinkageType)fView.LinkageTypeCombo.SelectedIndex;

                fLocalUndoman.Commit();

                fBase.NotifyRecord(fPerson, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.LogWrite("ParentsEditDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
            try {
                fView.ChildName.Text = GKUtils.GetNameString(fPerson, true, false);
                fView.LinkageTypeCombo.SelectedIndex = (sbyte)fLink.PedigreeLinkageType;

                UpdateControls();
            } catch (Exception ex) {
                Logger.LogWrite("ParentsEditDlgController.UpdateView(): " + ex.Message);
            }
        }

        public void UpdateControls()
        {
            if (fLink != null) {
                GEDCOMFamilyRecord family = fLink.Family;
                fView.SetParentsAvl(true);

                GEDCOMIndividualRecord relPerson = family.GetHusband();
                if (relPerson != null) {
                    fView.SetFatherAvl(true);
                    fView.Father.Text = GKUtils.GetNameString(relPerson, true, false);
                } else {
                    fView.SetFatherAvl(false);
                    fView.Father.Text = "";
                }

                relPerson = family.GetWife();
                if (relPerson != null) {
                    fView.SetMotherAvl(true);
                    fView.Mother.Text = GKUtils.GetNameString(relPerson, true, false);
                } else {
                    fView.SetMotherAvl(false);
                    fView.Mother.Text = "";
                }
            } else {
                fView.SetParentsAvl(false);
                fView.SetFatherAvl(false);
                fView.SetMotherAvl(false);

                fView.Father.Text = "";
                fView.Mother.Text = "";
            }
        }

        public void EditParents()
        {
            GEDCOMFamilyRecord family = fBase.Context.GetChildFamily(fPerson, false, null);
            if (family != null && BaseController.ModifyFamily(fBase, ref family, TargetMode.tmNone, null)) {
                UpdateControls();
            }
        }

        public void AddFather()
        {
            if (BaseController.AddIndividualFather(fBase, fLocalUndoman, fPerson)) {
                UpdateControls();
            }
        }

        public void DeleteFather()
        {
            if (BaseController.DeleteIndividualFather(fBase, fLocalUndoman, fPerson)) {
                UpdateControls();
            }
        }

        public void AddMother()
        {
            if (BaseController.AddIndividualMother(fBase, fLocalUndoman, fPerson)) {
                UpdateControls();
            }
        }

        public void DeleteMother()
        {
            if (BaseController.DeleteIndividualMother(fBase, fLocalUndoman, fPerson)) {
                UpdateControls();
            }
        }
    }
}
