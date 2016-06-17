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
                    this.txtName.Text = "";
                    this.cmbCorrType.SelectedIndex = -1;
                    this.txtDate.Text = "";
                    this.txtDir.SelectedIndex = 0;
                    this.txtCorresponder.Text = "";
                }
                else
                {
                    this.txtName.Text = this.fCommunication.CommName;
                    this.cmbCorrType.SelectedIndex = (int)this.fCommunication.CommunicationType;
                    this.txtDate.Text = GKUtils.GetDateFmtString(this.fCommunication.Date, DateFormat.dfDD_MM_YYYY);

                    GKCommunicationDir dir;
                    this.fCommunication.GetCorresponder(out dir, out this.fTempInd);

                    if (this.fTempInd != null)
                    {
                        this.txtDir.SelectedIndex = (int)dir;
                        this.txtCorresponder.Text = this.fTempInd.GetNameString(true, false);
                    }
                    else
                    {
                        this.txtDir.SelectedIndex = 0;
                        this.txtCorresponder.Text = "";
                    }

                    this.fNotesList.DataList = this.fCommunication.Notes.GetEnumerator();
                    this.fMediaList.DataList = this.fCommunication.MultimediaLinks.GetEnumerator();
                }
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("CommunicationEditDlg.SetCommunication(): " + ex.Message);
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                this.fCommunication.CommName = this.txtName.Text;
                this.fCommunication.CommunicationType = (GKCommunicationType)this.cmbCorrType.SelectedIndex;
                this.fCommunication.Date.ParseString(GEDCOMUtils.StrToGEDCOMDate(this.txtDate.Text, true));
                this.fCommunication.SetCorresponder((GKCommunicationDir)this.txtDir.SelectedIndex, this.fTempInd);
                this.fBase.ChangeRecord(this.fCommunication);
                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("CommunicationEditDlg.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        private void btnPersonAdd_Click(object sender, EventArgs e)
        {
            this.fTempInd = this.fBase.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svNone);
            this.txtCorresponder.Text = ((this.fTempInd == null) ? "" : this.fTempInd.GetNameString(true, false));
        }

        public CommunicationEditDlg(IBaseWindow aBase)
        {
            this.InitializeComponent();

            this.btnPersonAdd.Image = global::GKResources.iRecNew;
            this.btnAccept.Image = global::GKResources.iBtnAccept;
            this.btnCancel.Image = global::GKResources.iBtnCancel;

            this.fBase = aBase;
            this.fTempInd = null;

            for (GKCommunicationType ct = GKCommunicationType.ctCall; ct <= GKCommunicationType.ctLast; ct++)
            {
                this.cmbCorrType.Items.Add(LangMan.LS(GKData.CommunicationNames[(int)ct]));
            }

            this.fNotesList = new GKNotesSheet(this, this.pageNotes);
            this.fMediaList = new GKMediaSheet(this, this.pageMultimedia);

            // SetLang()
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Text = LangMan.LS(LSID.LSID_WinCommunicationEdit);
            this.pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            this.pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
            this.lblTheme.Text = LangMan.LS(LSID.LSID_Theme);
            this.lblCorresponder.Text = LangMan.LS(LSID.LSID_Corresponder);
            this.lblType.Text = LangMan.LS(LSID.LSID_Type);
            this.lblDate.Text = LangMan.LS(LSID.LSID_Date);

            this.toolTip1.SetToolTip(this.btnPersonAdd, LangMan.LS(LSID.LSID_PersonAttachTip));
            
            this.txtDir.Items.Clear();
            this.txtDir.Items.AddRange(new object[] { LangMan.LS(LSID.LSID_CD_1), LangMan.LS(LSID.LSID_CD_2) });
        }
    }
}
