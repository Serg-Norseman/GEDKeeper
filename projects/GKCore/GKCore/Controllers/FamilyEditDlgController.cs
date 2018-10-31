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
    public sealed class FamilyEditDlgController : DialogController<IFamilyEditDlg>
    {
        private GEDCOMFamilyRecord fFamily;

        public GEDCOMFamilyRecord Family
        {
            get { return fFamily; }
            set {
                if (fFamily != value) {
                    fFamily = value;
                    UpdateView();
                }
            }
        }


        public FamilyEditDlgController(IFamilyEditDlg view) : base(view)
        {
            for (GEDCOMRestriction res = GEDCOMRestriction.rnNone; res <= GEDCOMRestriction.rnLast; res++) {
                fView.Restriction.Add(LangMan.LS(GKData.Restrictions[(int)res]));
            }

            for (int i = 0; i < GKData.MarriageStatus.Length; i++) {
                fView.MarriageStatus.Add(LangMan.LS(GKData.MarriageStatus[i].Name));
            }
        }

        public void SetTarget(TargetMode targetType, GEDCOMIndividualRecord target)
        {
            if (targetType == TargetMode.tmNone || target == null) return;

            bool result = false;
            if (targetType == TargetMode.tmFamilySpouse) {
                result = fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, fFamily, target);
            } else if (targetType == TargetMode.tmFamilyChild) {
                result = fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, target, fFamily);
            }

            if (result) UpdateControls();
        }

        public override bool Accept()
        {
            try {
                string stat = GKData.MarriageStatus[fView.MarriageStatus.SelectedIndex].StatSign;
                fFamily.SetTagStringValue("_STAT", stat);

                fFamily.Restriction = (GEDCOMRestriction)fView.Restriction.SelectedIndex;

                fFamily.SortChilds();

                fLocalUndoman.Commit();

                fBase.NotifyRecord(fFamily, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.LogWrite("FamilyEditDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
            try {
                fView.ChildrenList.ListModel.DataOwner = fFamily;
                fView.EventsList.ListModel.DataOwner = fFamily;
                fView.NotesList.ListModel.DataOwner = fFamily;
                fView.MediaList.ListModel.DataOwner = fFamily;
                fView.SourcesList.ListModel.DataOwner = fFamily;

                if (fFamily == null) {
                    fView.MarriageStatus.Enabled = false;
                    fView.MarriageStatus.SelectedIndex = 0;
                    fView.Restriction.SelectedIndex = 0;
                } else {
                    string stat = fFamily.GetTagStringValue("_STAT");
                    int statIdx = GKUtils.GetMarriageStatusIndex(stat);
                    fView.MarriageStatus.Enabled = true;
                    fView.MarriageStatus.SelectedIndex = statIdx;
                    fView.Restriction.SelectedIndex = (sbyte)fFamily.Restriction;
                }

                UpdateControls();
            } catch (Exception ex) {
                Logger.LogWrite("FamilyEditDlgController.SetFamily(): " + ex.Message);
            }
        }

        private void UpdateControls()
        {
            GEDCOMIndividualRecord husband, wife;

            if (fFamily == null) {
                husband = null;
                wife = null;

                fView.LockEditor(true);
            } else {
                husband = fFamily.GetHusband();
                wife = fFamily.GetWife();

                fView.LockEditor(fFamily.Restriction == GEDCOMRestriction.rnLocked);
            }

            fView.SetHusband((husband != null) ? GKUtils.GetNameString(husband, true, false) : null);
            fView.SetWife((wife != null) ? GKUtils.GetNameString(wife, true, false) : null);

            fView.ChildrenList.UpdateSheet();
            fView.EventsList.UpdateSheet();
            fView.NotesList.UpdateSheet();
            fView.MediaList.UpdateSheet();
            fView.SourcesList.UpdateSheet();
        }

        public void AddHusband()
        {
            if (BaseController.AddFamilyHusband(fBase, fLocalUndoman, fFamily)) {
                UpdateControls();
            }
        }

        public void DeleteHusband()
        {
            if (BaseController.DeleteFamilyHusband(fBase, fLocalUndoman, fFamily)) {
                UpdateControls();
            }
        }

        public void AddWife()
        {
            if (BaseController.AddFamilyWife(fBase, fLocalUndoman, fFamily)) {
                UpdateControls();
            }
        }

        public void DeleteWife()
        {
            if (BaseController.DeleteFamilyWife(fBase, fLocalUndoman, fFamily)) {
                UpdateControls();
            }
        }

        public void JumpToRecord(GEDCOMRecord record)
        {
            if (record != null && Accept()) {
                fBase.SelectRecordByXRef(record.XRef);
                fView.Close();
            }
        }

        public void JumpToHusband()
        {
            JumpToRecord(fFamily.GetHusband());
        }

        public void JumpToWife()
        {
            JumpToRecord(fFamily.GetWife());
        }
    }
}
