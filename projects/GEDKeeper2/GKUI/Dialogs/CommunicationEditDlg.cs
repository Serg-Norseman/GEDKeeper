/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Sheets;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class CommunicationEditDlg : Form, IBaseEditor
    {
        private readonly IBaseWindow fBase;
        private readonly GKNotesSheet fNotesList;
        private readonly GKMediaSheet fMediaList;
        
        private GEDCOMCommunicationRecord fCommunication;
        private GEDCOMIndividualRecord fTempInd;

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }

        public GEDCOMCommunicationRecord Communication
        {
            get { return this.fCommunication; }
            set { this.SetCommunication(value); }
        }

        private void SetCommunication(GEDCOMCommunicationRecord value)
        {
            this.fCommunication = value;
            try
            {
                if (this.fCommunication == null)
                {
                    this.EditName.Text = "";
                    this.EditCorrType.SelectedIndex = -1;
                    this.EditDate.Text = "";
                    this.EditDir.SelectedIndex = 0;
                    this.EditCorresponder.Text = "";
                }
                else
                {
                    this.EditName.Text = this.fCommunication.CommName;
                    this.EditCorrType.SelectedIndex = (int)this.fCommunication.CommunicationType;
                    this.EditDate.Text = GKUtils.GetDateFmtString(this.fCommunication.Date, DateFormat.dfDD_MM_YYYY);

                    GKCommunicationDir dir;
                    this.fCommunication.GetCorresponder(out dir, out this.fTempInd);

                    if (this.fTempInd != null)
                    {
                        this.EditDir.SelectedIndex = (int)dir;
                        this.EditCorresponder.Text = this.fTempInd.GetNameString(true, false);
                    }
                    else
                    {
                        this.EditDir.SelectedIndex = 0;
                        this.EditCorresponder.Text = "";
                    }

                    this.fNotesList.DataList = this.fCommunication.Notes.GetEnumerator();
                    this.fMediaList.DataList = this.fCommunication.MultimediaLinks.GetEnumerator();
                }
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TfmCommunicationEdit.SetCommunication(): " + ex.Message);
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                this.fCommunication.CommName = this.EditName.Text;
                this.fCommunication.CommunicationType = (GKCommunicationType)this.EditCorrType.SelectedIndex;
                this.fCommunication.Date.ParseString(GEDCOMUtils.StrToGEDCOMDate(this.EditDate.Text, true));
                this.fCommunication.SetCorresponder((GKCommunicationDir)this.EditDir.SelectedIndex, this.fTempInd);
                this.fBase.ChangeRecord(this.fCommunication);
                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TfmCommunicationEdit.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        private void btnPersonAdd_Click(object sender, EventArgs e)
        {
            this.fTempInd = this.fBase.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svNone);
            this.EditCorresponder.Text = ((this.fTempInd == null) ? "" : this.fTempInd.GetNameString(true, false));
        }

        public CommunicationEditDlg(IBaseWindow aBase)
        {
            this.InitializeComponent();

            this.fBase = aBase;
            this.fTempInd = null;

            for (GKCommunicationType ct = GKCommunicationType.ctCall; ct <= GKCommunicationType.ctLast; ct++)
            {
                this.EditCorrType.Items.Add(LangMan.LS(GKData.CommunicationNames[(int)ct]));
            }

            this.fNotesList = new GKNotesSheet(this, this.SheetNotes);
            this.fMediaList = new GKMediaSheet(this, this.SheetMultimedia);

            // SetLang()
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Text = LangMan.LS(LSID.LSID_WinCommunicationEdit);
            this.SheetNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            this.SheetMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
            this.Label1.Text = LangMan.LS(LSID.LSID_Theme);
            this.Label5.Text = LangMan.LS(LSID.LSID_Corresponder);
            this.Label2.Text = LangMan.LS(LSID.LSID_Type);
            this.Label4.Text = LangMan.LS(LSID.LSID_Date);
        }
    }
}
