﻿/*
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
using GDModel;
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
        private GDMCommunicationRecord fCommunication;
        private GDMIndividualRecord fTempInd;

        public GDMCommunicationRecord Communication
        {
            get { return fCommunication; }
            set {
                if (fCommunication != value) {
                    fCommunication = value;
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

        public override bool Accept()
        {
            try {
                fCommunication.CommName = fView.Name.Text;
                fCommunication.CommunicationType = (GDMCommunicationType)fView.CorrType.SelectedIndex;
                fCommunication.Date.Assign(GDMDate.CreateByFormattedStr(fView.Date.Text, true));
                fCommunication.SetCorresponder((GDMCommunicationDir)fView.Dir.SelectedIndex, fTempInd);

                fBase.NotifyRecord(fCommunication, RecordAction.raEdit);

                CommitChanges();

                return true;
            } catch (Exception ex) {
                Logger.LogWrite("CommunicationEditDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
            try {
                fView.NotesList.ListModel.DataOwner = fCommunication;
                fView.MediaList.ListModel.DataOwner = fCommunication;

                if (fCommunication == null) {
                    fView.Name.Text = "";
                    fView.CorrType.SelectedIndex = -1;
                    fView.Date.Text = "";
                    fView.Dir.SelectedIndex = 0;
                    fView.Corresponder.Text = "";
                } else {
                    fView.Name.Text = fCommunication.CommName;
                    fView.CorrType.SelectedIndex = (int)fCommunication.CommunicationType;
                    fView.Date.Text = fCommunication.Date.GetDisplayString(DateFormat.dfDD_MM_YYYY);

                    fTempInd = fCommunication.Corresponder.Individual;

                    if (fTempInd != null) {
                        fView.Dir.SelectedIndex = (int)fCommunication.CommDirection;
                        fView.Corresponder.Text = GKUtils.GetNameString(fTempInd, true, false);
                    } else {
                        fView.Dir.SelectedIndex = 0;
                        fView.Corresponder.Text = "";
                    }
                }
            } catch (Exception ex) {
                Logger.LogWrite("CommunicationEditDlgController.SetCommunication(): " + ex.Message);
            }
        }

        public void SetPerson()
        {
            fTempInd = fBase.Context.SelectPerson(null, TargetMode.tmNone, GDMSex.svUnknown);
            fView.Corresponder.Text = ((fTempInd == null) ? "" : GKUtils.GetNameString(fTempInd, true, false));
        }
    }
}
