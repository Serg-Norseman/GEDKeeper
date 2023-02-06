/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore.Design.MVP.Controls;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class CommunicationEditDlgController : DialogController<ICommunicationEditDlg>
    {
        private GDMCommunicationRecord fCommunicationRecord;
        private GDMIndividualRecord fTempInd;

        public GDMCommunicationRecord CommunicationRecord
        {
            get { return fCommunicationRecord; }
            set {
                if (fCommunicationRecord != value) {
                    fCommunicationRecord = value;
                    UpdateView();
                }
            }
        }


        public CommunicationEditDlgController(ICommunicationEditDlg view) : base(view)
        {
            fTempInd = null;

            for (GDMCommunicationType ct = GDMCommunicationType.ctCall; ct <= GDMCommunicationType.ctLast; ct++) {
                fView.CorrType.Add(LangMan.LS(GKData.CommunicationNames[(int)ct]));
            }

            fView.Dir.AddRange(new object[] {
                LangMan.LS(LSID.LSID_CD_1),
                LangMan.LS(LSID.LSID_CD_2)
            });

            fView.Name.Activate();
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.NotesList.ListModel = new NoteLinksListModel(baseWin, fLocalUndoman);
            fView.MediaList.ListModel = new MediaLinksListModel(baseWin, fLocalUndoman);
        }

        public override bool Accept()
        {
            try {
                fCommunicationRecord.CommName = fView.Name.Text;
                fCommunicationRecord.CommunicationType = (GDMCommunicationType)fView.CorrType.SelectedIndex;
                fCommunicationRecord.Date.Assign(GDMDate.CreateByFormattedStr(fView.Date.NormalizeDate, true));
                fCommunicationRecord.SetCorresponder((GDMCommunicationDir)fView.Dir.SelectedIndex, fTempInd);

                fBase.NotifyRecord(fCommunicationRecord, RecordAction.raEdit);

                CommitChanges();

                return true;
            } catch (Exception ex) {
                Logger.WriteError("CommunicationEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            try {
                fView.NotesList.ListModel.DataOwner = fCommunicationRecord;
                fView.MediaList.ListModel.DataOwner = fCommunicationRecord;

                if (fCommunicationRecord == null) {
                    fView.Name.Text = "";
                    fView.CorrType.SelectedIndex = -1;
                    fView.Date.Text = "";
                    fView.Dir.SelectedIndex = 0;
                    fView.Corresponder.Text = "";
                } else {
                    fView.Name.Text = fCommunicationRecord.CommName;
                    fView.CorrType.SelectedIndex = (int)fCommunicationRecord.CommunicationType;
                    fView.Date.NormalizeDate = fCommunicationRecord.Date.GetDisplayString(DateFormat.dfDD_MM_YYYY);

                    fTempInd = fBase.Context.Tree.GetPtrValue(fCommunicationRecord.Corresponder);

                    if (fTempInd != null) {
                        fView.Dir.SelectedIndex = (int)fCommunicationRecord.CommDirection;
                        fView.Corresponder.Text = GKUtils.GetNameString(fTempInd, true, false);
                    } else {
                        fView.Dir.SelectedIndex = 0;
                        fView.Corresponder.Text = "";
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("CommunicationEditDlgController.SetCommunication()", ex);
            }
        }

        public void SetPerson()
        {
            fTempInd = fBase.Context.SelectPerson(null, TargetMode.tmNone, GDMSex.svUnknown);
            fView.Corresponder.Text = ((fTempInd == null) ? "" : GKUtils.GetNameString(fTempInd, true, false));
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.LSID_WinCommunicationEdit);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.LSID_DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.LSID_DlgCancel);
            GetControl<ITabPage>("pageNotes").Text = LangMan.LS(LSID.LSID_RPNotes);
            GetControl<ITabPage>("pageMultimedia").Text = LangMan.LS(LSID.LSID_RPMultimedia);
            GetControl<ILabel>("lblTheme").Text = LangMan.LS(LSID.LSID_Theme);
            GetControl<ILabel>("lblCorresponder").Text = LangMan.LS(LSID.LSID_Corresponder);
            GetControl<ILabel>("lblType").Text = LangMan.LS(LSID.LSID_Type);
            GetControl<ILabel>("lblDate").Text = LangMan.LS(LSID.LSID_Date);

            SetToolTip("btnPersonAdd", LangMan.LS(LSID.LSID_PersonAttachTip));
        }
    }
}
