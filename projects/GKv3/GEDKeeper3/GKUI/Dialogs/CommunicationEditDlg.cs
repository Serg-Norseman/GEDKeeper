/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using Eto.Forms;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class CommunicationEditDlg : EditorDialog, ICommunicationEditDlg
    {
        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;

        private GEDCOMCommunicationRecord fCommunication;
        private GEDCOMIndividualRecord fTempInd;

        public GEDCOMCommunicationRecord Communication
        {
            get { return fCommunication; }
            set { SetCommunication(value); }
        }

        private void SetCommunication(GEDCOMCommunicationRecord value)
        {
            fCommunication = value;
            try
            {
                if (fCommunication == null)
                {
                    txtName.Text = "";
                    cmbCorrType.SelectedIndex = -1;
                    txtDate.Text = "";
                    txtDir.SelectedIndex = 0;
                    txtCorresponder.Text = "";
                }
                else
                {
                    txtName.Text = fCommunication.CommName;
                    cmbCorrType.SelectedIndex = (int)fCommunication.CommunicationType;
                    txtDate.Text = fCommunication.Date.GetDisplayString(DateFormat.dfDD_MM_YYYY);

                    GKCommunicationDir dir;
                    fCommunication.GetCorresponder(out dir, out fTempInd);

                    if (fTempInd != null)
                    {
                        txtDir.SelectedIndex = (int)dir;
                        txtCorresponder.Text = GKUtils.GetNameString(fTempInd, true, false);
                    }
                    else
                    {
                        txtDir.SelectedIndex = 0;
                        txtCorresponder.Text = "";
                    }
                }

                fNotesList.ListModel.DataOwner = fCommunication;
                fMediaList.ListModel.DataOwner = fCommunication;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("CommunicationEditDlg.SetCommunication(): " + ex.Message);
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                fCommunication.CommName = txtName.Text;
                fCommunication.CommunicationType = (GKCommunicationType)cmbCorrType.SelectedIndex;
                fCommunication.Date.Assign(GEDCOMDate.CreateByFormattedStr(txtDate.Text, true));
                fCommunication.SetCorresponder((GKCommunicationDir)txtDir.SelectedIndex, fTempInd);

                CommitChanges();

                fBase.NotifyRecord(fCommunication, RecordAction.raEdit);

                DialogResult = DialogResult.Ok;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("CommunicationEditDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DialogResult.None;
            }
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try
            {
                RollbackChanges();
                CancelClickHandler(sender, e);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("CommunicationEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        private void btnPersonAdd_Click(object sender, EventArgs e)
        {
            fTempInd = fBase.Context.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svNone);
            txtCorresponder.Text = ((fTempInd == null) ? "" : GKUtils.GetNameString(fTempInd, true, false));
        }

        public CommunicationEditDlg()
        {
            InitializeComponent();

            fTempInd = null;

            for (GKCommunicationType ct = GKCommunicationType.ctCall; ct <= GKCommunicationType.ctLast; ct++)
            {
                cmbCorrType.Items.Add(LangMan.LS(GKData.CommunicationNames[(int)ct]));
            }

            fNotesList = new GKSheetList(pageNotes);
            fMediaList = new GKSheetList(pageMultimedia);

            // SetLang()
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Title = LangMan.LS(LSID.LSID_WinCommunicationEdit);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
            lblTheme.Text = LangMan.LS(LSID.LSID_Theme);
            lblCorresponder.Text = LangMan.LS(LSID.LSID_Corresponder);
            lblType.Text = LangMan.LS(LSID.LSID_Type);
            lblDate.Text = LangMan.LS(LSID.LSID_Date);
            btnPersonAdd.ToolTip = LangMan.LS(LSID.LSID_PersonAttachTip);

            txtDir.Items.Clear();
            txtDir.Items.AddRange(GKComboItem.Convert(new string[] { LangMan.LS(LSID.LSID_CD_1), LangMan.LS(LSID.LSID_CD_2) }));
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);

            fNotesList.ListModel = new NoteLinksListModel(fBase, fLocalUndoman);
            fMediaList.ListModel = new MediaLinksListModel(fBase, fLocalUndoman);
        }
    }
}
