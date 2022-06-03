﻿/*
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
using GKCore.Interfaces;
using GKCore.Lists;
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
        private GDMFamilyRecord fFamilyRecord;

        public GDMFamilyRecord FamilyRecord
        {
            get { return fFamilyRecord; }
            set {
                if (fFamilyRecord != value) {
                    fFamilyRecord = value;
                    UpdateView();
                }
            }
        }


        public FamilyEditDlgController(IFamilyEditDlg view) : base(view)
        {
            for (GDMRestriction res = GDMRestriction.rnNone; res <= GDMRestriction.rnLast; res++) {
                fView.Restriction.Add(LangMan.LS(GKData.Restrictions[(int)res]));
            }

            for (int i = 0; i < GKData.MarriageStatus.Length; i++) {
                fView.MarriageStatus.Add(LangMan.LS(GKData.MarriageStatus[i].Name));
            }
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.ChildrenList.ListModel = new FamilyChildrenListModel(baseWin, fLocalUndoman);
            fView.EventsList.ListModel = new EventsListModel(baseWin, fLocalUndoman, false);
            fView.NotesList.ListModel = new NoteLinksListModel(baseWin, fLocalUndoman);
            fView.MediaList.ListModel = new MediaLinksListModel(baseWin, fLocalUndoman);
            fView.SourcesList.ListModel = new SourceCitationsListModel(baseWin, fLocalUndoman);
        }

        public void SetTarget(TargetMode targetType, GDMIndividualRecord target)
        {
            if (targetType == TargetMode.tmNone || target == null) return;

            bool result = false;
            switch (targetType) {
                case TargetMode.tmSpouse:
                    result = fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, fFamilyRecord, target);
                    break;
                case TargetMode.tmFamilyChild:
                    result = fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, target, fFamilyRecord);
                    break;
            }

            if (result) UpdateControls();
        }

        public override bool Accept()
        {
            try {
                fFamilyRecord.Status = (GDMMarriageStatus)fView.MarriageStatus.SelectedIndex;
                fFamilyRecord.Restriction = (GDMRestriction)fView.Restriction.SelectedIndex;

                fBase.Context.ProcessFamily(fFamilyRecord);

                fLocalUndoman.Commit();

                fBase.NotifyRecord(fFamilyRecord, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("FamilyEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            try {
                fView.ChildrenList.ListModel.DataOwner = fFamilyRecord;
                fView.EventsList.ListModel.DataOwner = fFamilyRecord;
                fView.NotesList.ListModel.DataOwner = fFamilyRecord;
                fView.MediaList.ListModel.DataOwner = fFamilyRecord;
                fView.SourcesList.ListModel.DataOwner = fFamilyRecord;

                if (fFamilyRecord == null) {
                    fView.MarriageStatus.Enabled = false;
                    fView.MarriageStatus.SelectedIndex = 0;
                    fView.Restriction.SelectedIndex = 0;
                } else {
                    fView.MarriageStatus.Enabled = true;
                    fView.MarriageStatus.SelectedIndex = (int)fFamilyRecord.Status;
                    fView.Restriction.SelectedIndex = (sbyte)fFamilyRecord.Restriction;
                }

                UpdateControls();
            } catch (Exception ex) {
                Logger.WriteError("FamilyEditDlgController.SetFamily()", ex);
            }
        }

        private void UpdateControls()
        {
            GDMIndividualRecord husband, wife;

            if (fFamilyRecord == null) {
                husband = null;
                wife = null;

                fView.LockEditor(true);
            } else {
                fBase.Context.Tree.GetSpouses(fFamilyRecord, out husband, out wife);

                fView.LockEditor(fFamilyRecord.Restriction == GDMRestriction.rnLocked);
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
            if (BaseController.AddFamilyHusband(fBase, fLocalUndoman, fFamilyRecord)) {
                UpdateControls();
            }
        }

        public void DeleteHusband()
        {
            if (BaseController.DeleteFamilyHusband(fBase, fLocalUndoman, fFamilyRecord)) {
                UpdateControls();
            }
        }

        public void AddWife()
        {
            if (BaseController.AddFamilyWife(fBase, fLocalUndoman, fFamilyRecord)) {
                UpdateControls();
            }
        }

        public void DeleteWife()
        {
            if (BaseController.DeleteFamilyWife(fBase, fLocalUndoman, fFamilyRecord)) {
                UpdateControls();
            }
        }

        public void JumpToHusband()
        {
            JumpToRecord(fFamilyRecord.Husband);
        }

        public void JumpToWife()
        {
            JumpToRecord(fFamilyRecord.Wife);
        }

        public override void SetLocale()
        {
            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.LSID_DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.LSID_DlgCancel);
            GetControl<IGroupBox>("GroupBox1").Text = LangMan.LS(LSID.LSID_Family);
            GetControl<ILabel>("lblHusband").Text = LangMan.LS(LSID.LSID_Husband);
            GetControl<ILabel>("lblWife").Text = LangMan.LS(LSID.LSID_Wife);
            GetControl<ILabel>("lblStatus").Text = LangMan.LS(LSID.LSID_Status);
            GetControl<ITabPage>("pageChilds").Text = LangMan.LS(LSID.LSID_Childs);
            GetControl<ITabPage>("pageEvents").Text = LangMan.LS(LSID.LSID_Events);
            GetControl<ITabPage>("pageNotes").Text = LangMan.LS(LSID.LSID_RPNotes);
            GetControl<ITabPage>("pageMultimedia").Text = LangMan.LS(LSID.LSID_RPMultimedia);
            GetControl<ITabPage>("pageSources").Text = LangMan.LS(LSID.LSID_RPSources);
            GetControl<ILabel>("lblRestriction").Text = LangMan.LS(LSID.LSID_Restriction);

            SetToolTip("btnHusbandAdd", LangMan.LS(LSID.LSID_HusbandAddTip));
            SetToolTip("btnHusbandDelete", LangMan.LS(LSID.LSID_HusbandDeleteTip));
            SetToolTip("btnHusbandSel", LangMan.LS(LSID.LSID_HusbandSelTip));
            SetToolTip("btnWifeAdd", LangMan.LS(LSID.LSID_WifeAddTip));
            SetToolTip("btnWifeDelete", LangMan.LS(LSID.LSID_WifeDeleteTip));
            SetToolTip("btnWifeSel", LangMan.LS(LSID.LSID_WifeSelTip));
        }
    }
}
