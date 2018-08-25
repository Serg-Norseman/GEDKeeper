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

using BSLib;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;
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



        int IEventEditDlg.EventType
        {
            get { return cmbEventType.SelectedIndex; }
            set { cmbEventType.SelectedIndex = value; }
        }

        int IEventEditDlg.EventDateType
        {
            get { return cmbEventDateType.SelectedIndex; }
            set { cmbEventDateType.SelectedIndex = value; }
        }

        bool IEventEditDlg.Date1BC
        {
            get { return btnBC1.Checked; }
            set { btnBC1.Checked = value; }
        }

        bool IEventEditDlg.Date2BC
        {
            get { return btnBC2.Checked; }
            set { btnBC2.Checked = value; }
        }

        GEDCOMCalendar IEventEditDlg.Date1Calendar
        {
            get { return GetComboCalendar(cmbDate1Calendar); }
            set { SetComboCalendar(cmbDate1Calendar, value); }
        }

        GEDCOMCalendar IEventEditDlg.Date2Calendar
        {
            get { return GetComboCalendar(cmbDate2Calendar); }
            set { SetComboCalendar(cmbDate2Calendar, value); }
        }

        /*string IEventEditDlg.Date1Text
        {
            get { return txtEventDate1.Text; }
            set { txtEventDate1.Text = value; }
        }

        string IEventEditDlg.Date2Text
        {
            get { return txtEventDate2.Text; }
            set { txtEventDate2.Text = value; }
        }*/

        public ITextBoxHandler Date1
        {
            get { return fControlsManager.GetControlHandler(txtEventDate1) as ITextBoxHandler; }
        }

        public ITextBoxHandler Date2
        {
            get { return fControlsManager.GetControlHandler(txtEventDate2) as ITextBoxHandler; }
        }

        string IEventEditDlg.AttributeText
        {
            get { return txtAttribute.Text; }
            set { txtAttribute.Text = value; }
        }

        string IEventEditDlg.PlaceText
        {
            get { return txtEventPlace.Text; }
            set { txtEventPlace.Text = value; }
        }

        string IEventEditDlg.EventNameText
        {
            get { return txtEventName.Text; }
            set { txtEventName.Text = value; }
        }

        string IEventEditDlg.CauseText
        {
            get { return txtEventCause.Text; }
            set { txtEventCause.Text = value; }
        }

        string IEventEditDlg.AgencyText
        {
            get { return txtEventOrg.Text; }
            set { txtEventOrg.Text = value; }
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

        void IEventEditDlg.SetEventTypes(GKData.EventStruct[] eventTypes)
        {
            cmbEventType.Items.Clear();
            int num = eventTypes.Length;
            for (int i = 0; i < num; i++) {
                cmbEventType.Items.Add(LangMan.LS(eventTypes[i].Name));
            }
        }

        private static GEDCOMCalendar GetComboCalendar(ComboBox comboBox)
        {
            GEDCOMCalendar result = (GEDCOMCalendar)(((GKComboItem)comboBox.SelectedItem).Tag);
            return result;
        }

        private static void SetComboCalendar(ComboBox comboBox, GEDCOMCalendar calendar)
        {
            foreach (object item in comboBox.Items) {
                GKComboItem comboItem = (GKComboItem)item;

                if ((GEDCOMCalendar)comboItem.Tag == calendar) {
                    comboBox.SelectedItem = item;
                    return;
                }
            }

            comboBox.SelectedIndex = 0;
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

        public void SetAttributeMode(bool active)
        {
            if (active) {
                txtAttribute.Enabled = true;
                txtAttribute.BackColor = SystemColors.Window;
            } else {
                txtAttribute.Enabled = false;
                txtAttribute.BackColor = SystemColors.Control;
                txtAttribute.Text = "";
            }
        }

        public void ChangeEventType()
        {
            if (fController.Event is GEDCOMFamilyEvent) {
                SetAttributeMode(false);
            } else {
                int idx = cmbEventType.SelectedIndex;
                if (idx >= 0) {
                    if (GKData.PersonEvents[idx].Kind == PersonEventKind.ekEvent) {
                        SetAttributeMode(false);
                    } else {
                        SetAttributeMode(true);
                    }
                }
            }

            string evName;
            int id = cmbEventType.SelectedIndex;
            if (fController.Event is GEDCOMFamilyEvent) {
                evName = GKData.FamilyEvents[id].Sign;
            } else {
                evName = GKData.PersonEvents[id].Sign;
            }

            // TODO: It is necessary to provide the registrable list of values for different tag types.
            string[] vals;
            bool canbeSorted, userInput;

            if (evName == "_BGRO") {
                vals = GKData.BloodGroups.Split('|');
                canbeSorted = false;
                userInput = false;
            } else {
                vals = fBase.Context.ValuesCollection.GetValues(evName);
                canbeSorted = true;
                userInput = true;
            }

            if (vals != null) {
                string tmp = txtAttribute.Text;
                txtAttribute.Sorted = false;

                txtAttribute.Items.Clear();
                txtAttribute.Items.AddRange(vals);

                txtAttribute.Sorted = canbeSorted;
                txtAttribute.Text = tmp;

                txtAttribute.DropDownStyle = (userInput) ? ComboBoxStyle.DropDown : ComboBoxStyle.DropDownList;
            }
        }

        private void EditEventType_SelectedIndexChanged(object sender, EventArgs e)
        {
            ChangeEventType();
        }

        public void ChangeDateType()
        {
            int idx = cmbEventDateType.SelectedIndex;
            if (idx < 0 || idx >= GKData.DateKinds.Length) return;

            byte dates = GKData.DateKinds[idx].Dates;
            bool vis1 = BitHelper.IsSetBit(dates, 0);
            bool vis2 = BitHelper.IsSetBit(dates, 1);

            txtEventDate1.Enabled = vis1;
            cmbDate1Calendar.Enabled = vis1;
            btnBC1.Enabled = vis1;

            txtEventDate2.Enabled = vis2;
            cmbDate2Calendar.Enabled = vis2;
            btnBC2.Enabled = vis2;
        }

        private void EditEventDateType_SelectedIndexChanged(object sender, EventArgs e)
        {
            ChangeDateType();
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
