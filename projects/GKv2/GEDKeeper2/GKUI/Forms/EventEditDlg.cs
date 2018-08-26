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
using System.Drawing;
using System.Windows.Forms;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class EventEditDlg : EditorDialog, IEventEditDlg
    {
        private readonly EventEditController fController;
        private readonly ControlsManager fControlsManager;

        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;
        private readonly GKSheetList fSourcesList;


        public GEDCOMCustomEvent Event
        {
            get { return fController.Event; }
            set {
                fNotesList.ListModel.DataOwner = value;
                fMediaList.ListModel.DataOwner = value;
                fSourcesList.ListModel.DataOwner = value;
                fController.Event = value;
                ActiveControl = cmbEventType;
            }
        }

        #region View Interface

        ISheetList IEventEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        ISheetList IEventEditDlg.MediaList
        {
            get { return fMediaList; }
        }

        ISheetList IEventEditDlg.SourcesList
        {
            get { return fSourcesList; }
        }

        IComboBoxHandler IEventEditDlg.EventType
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(cmbEventType); }
        }

        IComboBoxHandler IEventEditDlg.EventDateType
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(cmbEventDateType); }
        }

        ICheckBoxHandler IEventEditDlg.Date1BC
        {
            get { return fControlsManager.GetControlHandler<ICheckBoxHandler>(btnBC1); }
        }

        ICheckBoxHandler IEventEditDlg.Date2BC
        {
            get { return fControlsManager.GetControlHandler<ICheckBoxHandler>(btnBC2); }
        }

        IComboBoxHandler IEventEditDlg.Date1Calendar
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(cmbDate1Calendar); }
        }

        IComboBoxHandler IEventEditDlg.Date2Calendar
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(cmbDate2Calendar); }
        }

        ITextBoxHandler IEventEditDlg.Date1
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtEventDate1); }
        }

        ITextBoxHandler IEventEditDlg.Date2
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtEventDate2); }
        }

        IComboBoxHandler IEventEditDlg.Attribute
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(txtAttribute); }
        }

        ITextBoxHandler IEventEditDlg.Place
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtEventPlace); }
        }

        ITextBoxHandler IEventEditDlg.EventName
        {
            get { return  fControlsManager.GetControlHandler<ITextBoxHandler>(txtEventName); }
        }

        ITextBoxHandler IEventEditDlg.Cause
        {
            get { return  fControlsManager.GetControlHandler<ITextBoxHandler>(txtEventCause); }
        }

        ITextBoxHandler IEventEditDlg.Agency
        {
            get { return  fControlsManager.GetControlHandler<ITextBoxHandler>(txtEventOrg); }
        }

        #endregion

        public EventEditDlg()
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");
            btnPlaceAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            btnPlaceDelete.Image = UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif");

            int num = GKData.DateKinds.Length;
            for (int i = 0; i < num; i++) {
                cmbEventDateType.Items.Add(LangMan.LS(GKData.DateKinds[i].Name));
            }

            for (GEDCOMCalendar gc = GEDCOMCalendar.dcGregorian; gc <= GEDCOMCalendar.dcLast; gc++) {
                GKData.CalendarStruct cdr = GKData.DateCalendars[(int)gc];
                if (!cdr.HasSupport) continue;

                cmbDate1Calendar.Items.Add(new GKComboItem(LangMan.LS(cdr.Name), gc));
                cmbDate2Calendar.Items.Add(new GKComboItem(LangMan.LS(cdr.Name), gc));
            }

            cmbDate1Calendar.SelectedIndex = 0;
            cmbDate2Calendar.SelectedIndex = 0;

            fNotesList = new GKSheetList(pageNotes);
            fMediaList = new GKSheetList(pageMultimedia);
            fSourcesList = new GKSheetList(pageSources);

            // SetLang()
            Text = LangMan.LS(LSID.LSID_Event);
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            btnAddress.Text = LangMan.LS(LSID.LSID_Address) + @"...";
            pageCommon.Text = LangMan.LS(LSID.LSID_Common);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
            pageSources.Text = LangMan.LS(LSID.LSID_RPSources);
            lblEvent.Text = LangMan.LS(LSID.LSID_Event);
            lblAttrValue.Text = LangMan.LS(LSID.LSID_Value);
            lblPlace.Text = LangMan.LS(LSID.LSID_Place);
            lblDate.Text = LangMan.LS(LSID.LSID_Date);
            lblCause.Text = LangMan.LS(LSID.LSID_Cause);
            lblOrg.Text = LangMan.LS(LSID.LSID_Agency);

            toolTip1.SetToolTip(btnPlaceAdd, LangMan.LS(LSID.LSID_PlaceAddTip));
            toolTip1.SetToolTip(btnPlaceDelete, LangMan.LS(LSID.LSID_PlaceDeleteTip));

            fController = new EventEditController(this);
            fControlsManager = new ControlsManager();
        }

        public void SetLocationMode(bool active)
        {
            if (active) {
                txtEventPlace.ReadOnly = true;
                txtEventPlace.BackColor = SystemColors.Control;
                btnPlaceAdd.Enabled = false;
                btnPlaceDelete.Enabled = true;
            } else {
                txtEventPlace.ReadOnly = false;
                txtEventPlace.BackColor = SystemColors.Window;
                btnPlaceAdd.Enabled = true;
                btnPlaceDelete.Enabled = false;
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            bool res = fController.Accept();
            if (res) {
                CommitChanges();
            }
            DialogResult = res ? DialogResult.OK : DialogResult.None;
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try {
                RollbackChanges();
            } catch (Exception ex) {
                Logger.LogWrite("EventEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        private void btnAddress_Click(object sender, EventArgs e)
        {
            fController.ModifyAddress();
        }

        private void EditEventPlace_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Down && e.Control) {
                txtEventPlace.Text = txtEventPlace.Text.ToLower();
            }
        }

        private void btnPlaceAdd_Click(object sender, EventArgs e)
        {
            fController.AddPlace();
        }

        private void btnPlaceDelete_Click(object sender, EventArgs e)
        {
            fController.RemovePlace();
        }

        private void EditEventDate1_DragOver(object sender, DragEventArgs e)
        {
            e.Effect = e.Data.GetDataPresent(typeof(string)) ? DragDropEffects.Move : DragDropEffects.None;
        }

        private void EditEventDate1_DragDrop(object sender, DragEventArgs e)
        {
            try {
                IDataObject data = e.Data;
                if (data.GetDataPresent(typeof(string))) {
                    string txt = data.GetData(typeof(string)) as string;

                    MaskedTextBox txtBox = ((MaskedTextBox)sender);
                    string[] dt = txtBox.Text.Split('/');
                    txtBox.Text = dt[0] + "/" + dt[1] + "/" + txt.PadLeft(4, '_');
                }
            } catch (Exception ex) {
                Logger.LogWrite("EventEditDlg.DragDrop(): " + ex.Message);
            }
        }

        private void EditEventType_SelectedIndexChanged(object sender, EventArgs e)
        {
            fController.ChangeEventType();
        }

        private void EditEventDateType_SelectedIndexChanged(object sender, EventArgs e)
        {
            fController.ChangeDateType();
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);
            fController.Init(baseWin);

            fNotesList.ListModel = new NoteLinksListModel(fBase, fLocalUndoman);
            fMediaList.ListModel = new MediaLinksListModel(fBase, fLocalUndoman);
            fSourcesList.ListModel = new SourceCitationsListModel(fBase, fLocalUndoman);
        }
    }
}
