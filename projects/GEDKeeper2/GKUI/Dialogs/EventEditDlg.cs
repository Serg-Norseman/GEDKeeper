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
using System.Drawing;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Controls;
using GKUI.Sheets;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class EventEditDlg : EditorDialog
    {
        private readonly GKNotesSheet fNotesList;
        private readonly GKMediaSheet fMediaList;
        private readonly GKSourcesSheet fSourcesList;

        private GEDCOMCustomEvent fEvent;
        private GEDCOMLocationRecord fLocation;

        public GEDCOMCustomEvent Event
        {
            get { return this.fEvent; }
            set { this.SetEvent(value); }
        }

        public EventEditDlg(IBaseWindow baseWin) : base(baseWin)
        {
            this.InitializeComponent();

            this.btnAccept.Image = GKResources.iBtnAccept;
            this.btnCancel.Image = GKResources.iBtnCancel;
            this.btnPlaceAdd.Image = GKResources.iRecNew;
            this.btnPlaceDelete.Image = GKResources.iRecDelete;

            int num = GKData.DateKinds.Length;
            for (int i = 0; i < num; i++)
            {
                cmbEventDateType.Items.Add(LangMan.LS(GKData.DateKinds[i].Name));
            }

            for (GEDCOMCalendar gc = GEDCOMCalendar.dcGregorian; gc <= GEDCOMCalendar.dcLast; gc++)
            {
                GKData.CalendarStruct cdr = GKData.DateCalendars[(int)gc];
                if (!cdr.HasSupport) continue;

                this.cmbDate1Calendar.Items.Add(new GKComboItem(LangMan.LS(cdr.Name), gc));
                this.cmbDate2Calendar.Items.Add(new GKComboItem(LangMan.LS(cdr.Name), gc));
            }

            this.cmbDate1Calendar.SelectedIndex = 0;
            this.cmbDate2Calendar.SelectedIndex = 0;

            this.fLocation = null;

            this.fNotesList = new GKNotesSheet(this, this.pageNotes, this.fLocalUndoman);
            this.fMediaList = new GKMediaSheet(this, this.pageMultimedia, this.fLocalUndoman);
            this.fSourcesList = new GKSourcesSheet(this, this.pageSources, this.fLocalUndoman);

            // SetLang()
            this.Text = LangMan.LS(LSID.LSID_Event);
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.btnAddress.Text = LangMan.LS(LSID.LSID_Address) + @"...";
            this.pageCommon.Text = LangMan.LS(LSID.LSID_Common);
            this.pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            this.pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
            this.pageSources.Text = LangMan.LS(LSID.LSID_RPSources);
            this.lblEvent.Text = LangMan.LS(LSID.LSID_Event);
            this.lblAttrValue.Text = LangMan.LS(LSID.LSID_Value);
            this.lblPlace.Text = LangMan.LS(LSID.LSID_Place);
            this.lblDate.Text = LangMan.LS(LSID.LSID_Date);
            this.lblCause.Text = LangMan.LS(LSID.LSID_Cause);
            this.lblOrg.Text = LangMan.LS(LSID.LSID_Agency);

            this.toolTip1.SetToolTip(this.btnPlaceAdd, LangMan.LS(LSID.LSID_PlaceAddTip));
            this.toolTip1.SetToolTip(this.btnPlaceDelete, LangMan.LS(LSID.LSID_PlaceDeleteTip));
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

        private string AssembleDate()
        {
            string result = "";

            GEDCOMCalendar cal1 = GetComboCalendar(this.cmbDate1Calendar);
            GEDCOMCalendar cal2 = GetComboCalendar(this.cmbDate2Calendar);

            string gcd1 = GEDCOMUtils.StrToGEDCOMDate(this.txtEventDate1.Text, true);
            string gcd2 = GEDCOMUtils.StrToGEDCOMDate(this.txtEventDate2.Text, true);

            if (cal1 != GEDCOMCalendar.dcGregorian) {
                gcd1 = GEDCOMCustomDate.GEDCOMDateEscapeArray[(int)cal1] + " " + gcd1;
            }

            if (cal2 != GEDCOMCalendar.dcGregorian) {
                gcd2 = GEDCOMCustomDate.GEDCOMDateEscapeArray[(int)cal2] + " " + gcd2;
            }

            if (btnBC1.Checked) {
                gcd1 = gcd1 + GEDCOMObject.GEDCOM_YEAR_BC;
            }

            if (btnBC2.Checked) {
                gcd2 = gcd2 + GEDCOMObject.GEDCOM_YEAR_BC;
            }

            switch (this.cmbEventDateType.SelectedIndex) {
                case 0:
                    result = gcd1;
                    break;

                case 1:
                    result = "BEF " + gcd2;
                    break;

                case 2:
                    result = "AFT " + gcd1;
                    break;

                case 3:
                    result = "BET " + gcd1 + " AND " + gcd2;
                    break;

                case 4:
                    result = "FROM " + gcd1;
                    break;

                case 5:
                    result = "TO " + gcd2;
                    break;

                case 6:
                    result = "FROM " + gcd1 + " TO " + gcd2;
                    break;

                case 7:
                    result = "ABT " + gcd1;
                    break;

                case 8:
                    result = "CAL " + gcd1;
                    break;

                case 9:
                    result = "EST " + gcd1;
                    break;
            }

            return result;
        }

        private void AcceptChanges()
        {
            this.fEvent.Detail.Place.StringValue = this.txtEventPlace.Text;
            this.fEvent.Detail.Place.Location.Value = this.fLocation;
            this.fEvent.Detail.Classification = this.txtEventName.Text;
            this.fEvent.Detail.Cause = this.txtEventCause.Text;
            this.fEvent.Detail.Agency = this.txtEventOrg.Text;

            string dt = this.AssembleDate();
            this.fEvent.Detail.Date.ParseString(dt);

            if (this.fEvent is GEDCOMFamilyEvent)
            {
                this.fEvent.SetName(GKData.FamilyEvents[this.cmbEventType.SelectedIndex].Sign);
            }
            else
            {
                int id = this.cmbEventType.SelectedIndex;
                this.fEvent.SetName(GKData.PersonEvents[id].Sign);
                if (GKData.PersonEvents[id].Kind == PersonEventKind.ekFact)
                {
                    this.fEvent.StringValue = this.txtAttribute.Text;
                }
                else
                {
                    this.fEvent.StringValue = "";
                }
            }

            if (this.fEvent is GEDCOMIndividualEvent)
            {
                int id = this.cmbEventType.SelectedIndex;
                if (GKData.PersonEvents[id].Kind == PersonEventKind.ekFact)
                {
                    GEDCOMIndividualAttribute attr = new GEDCOMIndividualAttribute(this.fEvent.Owner, this.fEvent.Parent, "", "");
                    attr.Assign(this.fEvent);
                    this.fEvent = attr;
                }
            }

            base.CommitChanges();
        }

        private void ControlsRefresh()
        {
            if (this.fLocation != null) {
                this.txtEventPlace.Text = this.fLocation.LocationName;
                this.txtEventPlace.ReadOnly = true;
                this.txtEventPlace.BackColor = SystemColors.Control;
                this.btnPlaceAdd.Enabled = false;
                this.btnPlaceDelete.Enabled = true;
            } else {
                this.txtEventPlace.Text = this.fEvent.Detail.Place.StringValue;
                this.txtEventPlace.ReadOnly = false;
                this.txtEventPlace.BackColor = SystemColors.Window;
                this.btnPlaceAdd.Enabled = true;
                this.btnPlaceDelete.Enabled = false;
            }

            this.fNotesList.DataList = this.fEvent.Detail.Notes.GetEnumerator();
            this.fMediaList.DataList = this.fEvent.Detail.MultimediaLinks.GetEnumerator();
            this.fSourcesList.DataList = this.fEvent.Detail.SourceCitations.GetEnumerator();
        }

        private void SetEvent(GEDCOMCustomEvent value)
        {
            this.fEvent = value;

            if (this.fEvent is GEDCOMFamilyEvent)
            {
                int num = GKData.FamilyEvents.Length;
                for (int i = 0; i < num; i++)
                {
                    this.cmbEventType.Items.Add(LangMan.LS(GKData.FamilyEvents[i].Name));
                }

                int idx = GKUtils.GetFamilyEventIndex(this.fEvent.Name);
                if (idx < 0) idx = 0;
                this.cmbEventType.SelectedIndex = idx;
            }
            else
            {
                int num = GKData.PersonEvents.Length;
                for (int i = 0; i < num; i++)
                {
                    this.cmbEventType.Items.Add(LangMan.LS(GKData.PersonEvents[i].Name));
                }

                int idx = GKUtils.GetPersonEventIndex(this.fEvent.Name);
                if (idx < 0) idx = 0;
                this.cmbEventType.SelectedIndex = idx;

                if (idx >= 0 && GKData.PersonEvents[idx].Kind == PersonEventKind.ekFact)
                {
                    this.txtAttribute.Text = this.fEvent.StringValue;
                }
            }

            this.EditEventType_SelectedIndexChanged(null, null);

            GEDCOMCustomDate date = this.fEvent.Detail.Date.Value;
            if (date is GEDCOMDateApproximated)
            {
                GEDCOMApproximated approximated = (date as GEDCOMDateApproximated).Approximated;

                switch (approximated) {
                    case GEDCOMApproximated.daExact:
                        this.cmbEventDateType.SelectedIndex = 0;
                        break;
                    case GEDCOMApproximated.daAbout:
                        this.cmbEventDateType.SelectedIndex = 7;
                        break;
                    case GEDCOMApproximated.daCalculated:
                        this.cmbEventDateType.SelectedIndex = 8;
                        break;
                    case GEDCOMApproximated.daEstimated:
                        this.cmbEventDateType.SelectedIndex = 9;
                        break;
                }

                this.txtEventDate1.Text = GKUtils.GetDateFmtString(date as GEDCOMDate, DateFormat.dfDD_MM_YYYY);
                SetComboCalendar(this.cmbDate1Calendar, (date as GEDCOMDate).DateCalendar);
                this.btnBC1.Checked = (date as GEDCOMDate).YearBC;
            }
            else
            {
                if (date is GEDCOMDateRange)
                {
                    GEDCOMDateRange dtRange = date as GEDCOMDateRange;
                    if (dtRange.After.StringValue == "" && dtRange.Before.StringValue != "")
                    {
                        this.cmbEventDateType.SelectedIndex = 1;
                    }
                    else
                    {
                        if (dtRange.After.StringValue != "" && dtRange.Before.StringValue == "")
                        {
                            this.cmbEventDateType.SelectedIndex = 2;
                        }
                        else
                        {
                            if (dtRange.After.StringValue != "" && dtRange.Before.StringValue != "")
                            {
                                this.cmbEventDateType.SelectedIndex = 3;
                            }
                        }
                    }

                    this.txtEventDate1.Text = GKUtils.GetDateFmtString(dtRange.After, DateFormat.dfDD_MM_YYYY);
                    this.txtEventDate2.Text = GKUtils.GetDateFmtString(dtRange.Before, DateFormat.dfDD_MM_YYYY);
                    SetComboCalendar(this.cmbDate1Calendar, dtRange.After.DateCalendar);
                    SetComboCalendar(this.cmbDate2Calendar, dtRange.Before.DateCalendar);
                    this.btnBC1.Checked = dtRange.After.YearBC;
                    this.btnBC2.Checked = dtRange.Before.YearBC;
                }
                else
                {
                    if (date is GEDCOMDatePeriod)
                    {
                        GEDCOMDatePeriod dtPeriod = date as GEDCOMDatePeriod;
                        if (dtPeriod.DateFrom.StringValue != "" && dtPeriod.DateTo.StringValue == "")
                        {
                            this.cmbEventDateType.SelectedIndex = 4;
                        }
                        else
                        {
                            if (dtPeriod.DateFrom.StringValue == "" && dtPeriod.DateTo.StringValue != "")
                            {
                                this.cmbEventDateType.SelectedIndex = 5;
                            }
                            else
                            {
                                if (dtPeriod.DateFrom.StringValue != "" && dtPeriod.DateTo.StringValue != "")
                                {
                                    this.cmbEventDateType.SelectedIndex = 6;
                                }
                            }
                        }

                        this.txtEventDate1.Text = GKUtils.GetDateFmtString(dtPeriod.DateFrom, DateFormat.dfDD_MM_YYYY);
                        this.txtEventDate2.Text = GKUtils.GetDateFmtString(dtPeriod.DateTo, DateFormat.dfDD_MM_YYYY);
                        SetComboCalendar(this.cmbDate1Calendar, dtPeriod.DateFrom.DateCalendar);
                        SetComboCalendar(this.cmbDate2Calendar, dtPeriod.DateTo.DateCalendar);
                        this.btnBC1.Checked = dtPeriod.DateFrom.YearBC;
                        this.btnBC2.Checked = dtPeriod.DateTo.YearBC;
                    }
                    else
                    {
                        if (date is GEDCOMDate)
                        {
                            this.cmbEventDateType.SelectedIndex = 0;
                            this.txtEventDate1.Text = GKUtils.GetDateFmtString(date as GEDCOMDate, DateFormat.dfDD_MM_YYYY);
                            SetComboCalendar(this.cmbDate1Calendar, (date as GEDCOMDate).DateCalendar);
                            this.btnBC1.Checked = (date as GEDCOMDate).YearBC;
                        }
                        else
                        {
                            this.cmbEventDateType.SelectedIndex = 0;
                            this.txtEventDate1.Text = "";
                            this.cmbDate1Calendar.SelectedIndex = 0;
                            this.btnBC1.Checked = false;
                        }
                    }
                }
            }

            this.EditEventDateType_SelectedIndexChanged(null, null);
            this.txtEventName.Text = this.fEvent.Detail.Classification;
            this.txtEventCause.Text = this.fEvent.Detail.Cause;
            this.txtEventOrg.Text = this.fEvent.Detail.Agency;
            this.fLocation = (this.fEvent.Detail.Place.Location.Value as GEDCOMLocationRecord);
            this.ControlsRefresh();

            this.ActiveControl = this.cmbEventType;
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                this.AcceptChanges();
                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("EventEditDlg.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try
            {
                base.RollbackChanges();
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("EventEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        private void btnAddress_Click(object sender, EventArgs e)
        {
            this.fBase.ModifyAddress(this.fEvent.Detail.Address);
        }

        private void EditEventPlace_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Down && e.Control)
            {
                this.txtEventPlace.Text = this.txtEventPlace.Text.ToLower();
            }
        }

        private void btnPlaceAdd_Click(object sender, EventArgs e)
        {
            this.fLocation = (this.fBase.SelectRecord(GEDCOMRecordType.rtLocation, null) as GEDCOMLocationRecord);
            this.ControlsRefresh();
        }

        private void btnPlaceDelete_Click(object sender, EventArgs e)
        {
            this.fLocation = null;
            this.ControlsRefresh();
        }

        private void EditEventDate1_DragOver(object sender, DragEventArgs e)
        {
            e.Effect = e.Data.GetDataPresent(typeof(string)) ? DragDropEffects.Move : DragDropEffects.None;
        }

        private void EditEventDate1_DragDrop(object sender, DragEventArgs e)
        {
            try
            {
                if (e.Data.GetDataPresent(typeof(string)))
                {
                    string txt = e.Data.GetData(typeof(string)) as string;
                    string[] dt = ((MaskedTextBox)sender).Text.Split('.');
                    ((MaskedTextBox)sender).Text = dt[0] + '.' + dt[1] + '.' + txt;
                }
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("EventEditDlg.DragDrop(): " + ex.Message);
            }
        }

        private void EditEventType_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (this.fEvent is GEDCOMFamilyEvent)
            {
                this.txtAttribute.Enabled = false;
                this.txtAttribute.BackColor = SystemColors.Control;
            }
            else
            {
                int idx = this.cmbEventType.SelectedIndex;
                if (idx >= 0) {
                    if (GKData.PersonEvents[idx].Kind == PersonEventKind.ekEvent)
                    {
                        this.txtAttribute.Enabled = false;
                        this.txtAttribute.BackColor = SystemColors.Control;
                        this.txtAttribute.Text = "";
                    }
                    else
                    {
                        this.txtAttribute.Enabled = true;
                        this.txtAttribute.BackColor = SystemColors.Window;
                    }
                }
            }

            string evName;
            int id = this.cmbEventType.SelectedIndex;
            if (this.fEvent is GEDCOMFamilyEvent) {
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
                vals = this.fBase.Context.ValuesCollection.GetValues(evName);
                canbeSorted = true;
                userInput = true;
            }

            if (vals != null) {
                string tmp = this.txtAttribute.Text;
                this.txtAttribute.Sorted = false;

                this.txtAttribute.Items.Clear();
                this.txtAttribute.Items.AddRange(vals);

                this.txtAttribute.Sorted = canbeSorted;
                this.txtAttribute.Text = tmp;

                this.txtAttribute.DropDownStyle = (userInput) ? ComboBoxStyle.DropDown : ComboBoxStyle.DropDownList;
            }
        }

        public void SetControlEnabled(Control ctl, bool enabled)
        {
            if (ctl == null) return;

            ctl.Enabled = enabled;
            ctl.BackColor = enabled ? SystemColors.Window : SystemColors.Control;
        }

        private void EditEventDateType_SelectedIndexChanged(object sender, EventArgs e)
        {
            int idx = this.cmbEventDateType.SelectedIndex;
            if (idx < 0 || idx >= GKData.DateKinds.Length) return;

            byte dates = GKData.DateKinds[idx].Dates;
            this.txtEventDate1.Enabled = SysUtils.IsSetBit(dates, 0);
            this.txtEventDate2.Enabled = SysUtils.IsSetBit(dates, 1);

            this.cmbDate1Calendar.Enabled = this.txtEventDate1.Enabled;
            this.cmbDate2Calendar.Enabled = this.txtEventDate2.Enabled;

            this.btnBC1.Enabled = this.txtEventDate1.Enabled;
            this.btnBC2.Enabled = this.txtEventDate2.Enabled;
        }
    }
}
