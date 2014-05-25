using System;
using System.Drawing;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKUI.Sheets;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Dialogs
{
	public partial class TfmEventEdit : Form, IBaseEditor
	{
        private readonly IBase fBase;
        private readonly GKNotesSheet fNotesList;
        private readonly GKMediaSheet fMediaList;
		private readonly GKSourcesSheet fSourcesList;

		private TGEDCOMCustomEvent fEvent;
		private TGEDCOMLocationRecord fLocation;

		public IBase Base
		{
			get { return this.fBase; }
		}

		public TGEDCOMCustomEvent Event
		{
			get { return this.fEvent; }
			set { this.SetEvent(value); }
		}

		private string AssembleDate()
		{
			string result = "";

			TGEDCOMCalendar cal = (TGEDCOMCalendar)this.cbDate1Calendar.SelectedIndex;
			TGEDCOMCalendar cal2 = (TGEDCOMCalendar)this.cbDate2Calendar.SelectedIndex;

			string gcd = GEDCOMUtils.StrToGEDCOMDate(this.EditEventDate1.Text, true);
			string gcd2 = GEDCOMUtils.StrToGEDCOMDate(this.EditEventDate2.Text, true);

			if (cal != TGEDCOMCalendar.dcGregorian) {
				gcd = TGEDCOMCustomDate.GEDCOMDateEscapeArray[(int)cal] + " " + gcd;
			}

			if (cal2 != TGEDCOMCalendar.dcGregorian) {
                gcd2 = TGEDCOMCustomDate.GEDCOMDateEscapeArray[(int)cal2] + " " + gcd2;
			}

			if (btnBC1.Checked) {
				gcd = gcd + GEDCOMObject.GEDCOMYearBC;
			}

			if (btnBC2.Checked) {
				gcd2 = gcd2 + GEDCOMObject.GEDCOMYearBC;
			}

			switch (this.EditEventDateType.SelectedIndex) {
				case 0:
					result = gcd;
					break;

				case 1:
					result = "BEF " + gcd2;
					break;

				case 2:
					result = "AFT " + gcd;
					break;

				case 3:
					result = "BET " + gcd + " AND " + gcd2;
					break;

				case 4:
					result = "FROM " + gcd;
					break;

				case 5:
					result = "TO " + gcd2;
					break;

				case 6:
					result = "FROM " + gcd + " TO " + gcd2;
					break;

				case 7:
					result = "ABT " + gcd;
					break;

				case 8:
					result = "CAL " + gcd;
					break;

				case 9:
					result = "EST " + gcd;
					break;
			}

			return result;
		}

		private void AcceptChanges()
		{
			this.fEvent.Detail.Place.StringValue = this.EditEventPlace.Text;
			this.fEvent.Detail.Place.Location.Value = this.fLocation;
			this.fEvent.Detail.Classification = this.EditEventName.Text;
			this.fEvent.Detail.Cause = this.EditEventCause.Text;
			this.fEvent.Detail.Agency = this.EditEventOrg.Text;

			string dt = this.AssembleDate();
			this.fEvent.Detail.Date.ParseString(dt);

			if (this.fEvent is TGEDCOMFamilyEvent)
			{
				this.fEvent.Name = GKData.FamilyEvents[this.EditEventType.SelectedIndex].Sign;
			}
			else
			{
				int id = this.EditEventType.SelectedIndex;
				this.fEvent.Name = GKData.PersonEvents[id].Sign;
				if (GKData.PersonEvents[id].Kind == TPersonEventKind.ekFact)
				{
					this.fEvent.StringValue = this.EditAttribute.Text;
				}
				else
				{
					this.fEvent.StringValue = "";
				}
			}

			if (this.fEvent is TGEDCOMIndividualEvent)
			{
				int id = this.EditEventType.SelectedIndex;
				if (GKData.PersonEvents[id].Kind == TPersonEventKind.ekFact)
				{
					TGEDCOMIndividualAttribute attr = new TGEDCOMIndividualAttribute(this.fEvent.Owner, this.fEvent.Parent, "", "");
					attr.Assign(this.fEvent);
					this.fEvent = attr;
				}
			}
		}

		private void ControlsRefresh()
		{
			if (this.fLocation != null) {
				this.EditEventPlace.Text = this.fLocation.LocationName;
				this.EditEventPlace.ReadOnly = true;
				this.EditEventPlace.BackColor = SystemColors.Control;
				this.btnPlaceAdd.Enabled = false;
				this.btnPlaceDelete.Enabled = true;
			} else {
				this.EditEventPlace.Text = this.fEvent.Detail.Place.StringValue;
				this.EditEventPlace.ReadOnly = false;
				this.EditEventPlace.BackColor = SystemColors.Window;
				this.btnPlaceAdd.Enabled = true;
				this.btnPlaceDelete.Enabled = false;
			}
			
			this.fNotesList.DataList = this.fEvent.Detail.Notes.GetEnumerator();
            this.fMediaList.DataList = this.fEvent.Detail.MultimediaLinks.GetEnumerator();
            this.fSourcesList.DataList = this.fEvent.Detail.SourceCitations.GetEnumerator();
		}

		private void SetEvent(TGEDCOMCustomEvent value)
		{
			this.fEvent = value;

			if (this.fEvent is TGEDCOMFamilyEvent)
			{
				for (int i = 0; i <= GKData.FamilyEvents.Length - 1; i++)
				{
					this.EditEventType.Items.Add(LangMan.LS(GKData.FamilyEvents[i].Name));
				}

				int idx = GKUtils.GetFamilyEventIndex(this.fEvent.Name);
				if (idx < 0) idx = 0;
				this.EditEventType.SelectedIndex = idx;
			}
			else
			{
				for (int i = 0; i <= GKData.PersonEvents.Length - 1; i++)
				{
					this.EditEventType.Items.Add(LangMan.LS(GKData.PersonEvents[i].Name));
				}

				int idx = GKUtils.GetPersonEventIndex(this.fEvent.Name);
				if (idx < 0) idx = 0;
				this.EditEventType.SelectedIndex = idx;

				if (idx >= 0 && GKData.PersonEvents[idx].Kind == TPersonEventKind.ekFact)
				{
					this.EditAttribute.Text = this.fEvent.StringValue;
				}
			}

			this.EditEventType_SelectedIndexChanged(null, null);

			TGEDCOMCustomDate date = this.fEvent.Detail.Date.Value;
			if (date is TGEDCOMDateApproximated)
			{
				TGEDCOMApproximated approximated = (date as TGEDCOMDateApproximated).Approximated;

				switch (approximated) {
					case TGEDCOMApproximated.daExact:
						this.EditEventDateType.SelectedIndex = 0;
						break;
					case TGEDCOMApproximated.daAbout:
						this.EditEventDateType.SelectedIndex = 7;
						break;
					case TGEDCOMApproximated.daCalculated:
						this.EditEventDateType.SelectedIndex = 8;
						break;
					case TGEDCOMApproximated.daEstimated:
						this.EditEventDateType.SelectedIndex = 9;
						break;
				}

				this.EditEventDate1.Text = GKUtils.GEDCOMDateToStr(date as TGEDCOMDate, DateFormat.dfDD_MM_YYYY);
				this.cbDate1Calendar.SelectedIndex = (int)(date as TGEDCOMDate).DateCalendar;
				this.btnBC1.Checked = (date as TGEDCOMDate).YearBC;
			}
			else
			{
				if (date is TGEDCOMDateRange)
				{
					TGEDCOMDateRange dtRange = date as TGEDCOMDateRange;
					if (dtRange.After.StringValue == "" && dtRange.Before.StringValue != "")
					{
						this.EditEventDateType.SelectedIndex = 1;
					}
					else
					{
						if (dtRange.After.StringValue != "" && dtRange.Before.StringValue == "")
						{
							this.EditEventDateType.SelectedIndex = 2;
						}
						else
						{
							if (dtRange.After.StringValue != "" && dtRange.Before.StringValue != "")
							{
								this.EditEventDateType.SelectedIndex = 3;
							}
						}
					}

					this.EditEventDate1.Text = GKUtils.GEDCOMDateToStr(dtRange.After, DateFormat.dfDD_MM_YYYY);
					this.EditEventDate2.Text = GKUtils.GEDCOMDateToStr(dtRange.Before, DateFormat.dfDD_MM_YYYY);
					this.cbDate1Calendar.SelectedIndex = (int)dtRange.After.DateCalendar;
					this.cbDate2Calendar.SelectedIndex = (int)dtRange.Before.DateCalendar;
					this.btnBC1.Checked = dtRange.After.YearBC;
					this.btnBC2.Checked = dtRange.Before.YearBC;
				}
				else
				{
					if (date is TGEDCOMDatePeriod)
					{
						TGEDCOMDatePeriod dtPeriod = date as TGEDCOMDatePeriod;
						if (dtPeriod.DateFrom.StringValue != "" && dtPeriod.DateTo.StringValue == "")
						{
							this.EditEventDateType.SelectedIndex = 4;
						}
						else
						{
							if (dtPeriod.DateFrom.StringValue == "" && dtPeriod.DateTo.StringValue != "")
							{
								this.EditEventDateType.SelectedIndex = 5;
							}
							else
							{
								if (dtPeriod.DateFrom.StringValue != "" && dtPeriod.DateTo.StringValue != "")
								{
									this.EditEventDateType.SelectedIndex = 6;
								}
							}
						}

						this.EditEventDate1.Text = GKUtils.GEDCOMDateToStr(dtPeriod.DateFrom, DateFormat.dfDD_MM_YYYY);
						this.EditEventDate2.Text = GKUtils.GEDCOMDateToStr(dtPeriod.DateTo, DateFormat.dfDD_MM_YYYY);
						this.cbDate1Calendar.SelectedIndex = (int)dtPeriod.DateFrom.DateCalendar;
						this.cbDate2Calendar.SelectedIndex = (int)dtPeriod.DateTo.DateCalendar;
						this.btnBC1.Checked = dtPeriod.DateFrom.YearBC;
						this.btnBC2.Checked = dtPeriod.DateTo.YearBC;
					}
					else
					{
						if (date is TGEDCOMDate)
						{
							this.EditEventDateType.SelectedIndex = 0;
							this.EditEventDate1.Text = GKUtils.GEDCOMDateToStr(date as TGEDCOMDate, DateFormat.dfDD_MM_YYYY);
							this.cbDate1Calendar.SelectedIndex = (int)(date as TGEDCOMDate).DateCalendar;
							this.btnBC1.Checked = (date as TGEDCOMDate).YearBC;
						}
						else
						{
							this.EditEventDateType.SelectedIndex = 0;
							this.EditEventDate1.Text = "";
							this.cbDate1Calendar.SelectedIndex = 0;
							this.btnBC1.Checked = false;
						}
					}
				}
			}

			this.EditEventDateType_SelectedIndexChanged(null, null);
			this.EditEventName.Text = this.fEvent.Detail.Classification;
			this.EditEventCause.Text = this.fEvent.Detail.Cause;
			this.EditEventOrg.Text = this.fEvent.Detail.Agency;
			this.fLocation = (this.fEvent.Detail.Place.Location.Value as TGEDCOMLocationRecord);
			this.ControlsRefresh();

			this.ActiveControl = this.EditEventType;
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
				this.fBase.Host.LogWrite("TfmEventEdit.Accept(): " + ex.Message);
				base.DialogResult = DialogResult.None;
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
				this.EditEventPlace.Text = this.EditEventPlace.Text.ToLower();
			}
		}

		private void btnPlaceAdd_Click(object sender, EventArgs e)
		{
			this.fLocation = (this.fBase.SelectRecord(TGEDCOMRecordType.rtLocation, null) as TGEDCOMLocationRecord);
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
					((MaskedTextBox)sender).Text = dt[0] + "." + dt[1] + "." + txt;
				}
			}
			catch (Exception ex)
			{
                this.fBase.Host.LogWrite("TfmEventEdit.DragDrop(): " + ex.Message);
			}
		}

		private void EditEventType_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (this.fEvent is TGEDCOMFamilyEvent)
			{
				this.EditAttribute.Enabled = false;
				this.EditAttribute.BackColor = SystemColors.Control;
			}
			else
			{
				int idx = this.EditEventType.SelectedIndex;
				if (idx >= 0) {
					if (GKData.PersonEvents[idx].Kind == TPersonEventKind.ekEvent)
					{
						this.EditAttribute.Enabled = false;
						this.EditAttribute.BackColor = SystemColors.Control;
						this.EditAttribute.Text = "";
					}
					else
					{
						this.EditAttribute.Enabled = true;
						this.EditAttribute.BackColor = SystemColors.Window;
					}
				}
			}

			string evName;
			int id = this.EditEventType.SelectedIndex;
			if (this.fEvent is TGEDCOMFamilyEvent) {
				evName = GKData.FamilyEvents[id].Sign;
			} else {
				evName = GKData.PersonEvents[id].Sign;
			}

			string[] vals = this.fBase.ValuesCollection.GetValues(evName);
			if (vals != null) {
				string tmp = this.EditAttribute.Text;
				this.EditAttribute.Sorted = false;

				this.EditAttribute.Items.Clear();
				this.EditAttribute.Items.AddRange(vals);

				this.EditAttribute.Sorted = true;
				this.EditAttribute.Text = tmp;
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
			int idx = this.EditEventDateType.SelectedIndex;
			if (idx < 0 || idx >= GKData.DateKinds.Length) return;

			GKData.TDateControlsRange dates = GKData.DateKinds[idx].Dates;

			this.EditEventDate1.Enabled = ((dates & (GKData.TDateControlsRange)2) > (GKData.TDateControlsRange)0);
			this.EditEventDate2.Enabled = ((dates & (GKData.TDateControlsRange)4) > (GKData.TDateControlsRange)0);

			this.cbDate1Calendar.Enabled = this.EditEventDate1.Enabled;
			this.cbDate2Calendar.Enabled = this.EditEventDate2.Enabled;

			this.btnBC1.Enabled = this.EditEventDate1.Enabled;
			this.btnBC2.Enabled = this.EditEventDate2.Enabled;
		}

		public TfmEventEdit(IBase aBase)
		{
			this.InitializeComponent();
			this.fBase = aBase;

			for (int i = 0; i <= GKData.DateKinds.Length - 1; i++)
			{
				this.EditEventDateType.Items.Add(LangMan.LS(GKData.DateKinds[i].Name));
			}

			for (TGEDCOMCalendar gc = TGEDCOMCalendar.dcGregorian; gc <= TGEDCOMCalendar.dcLast; gc++)
			{
				this.cbDate1Calendar.Items.Add(LangMan.LS(GKData.DateCalendars[(int)gc]));
				this.cbDate2Calendar.Items.Add(LangMan.LS(GKData.DateCalendars[(int)gc]));
			}

			this.cbDate1Calendar.SelectedIndex = 0;
			this.cbDate2Calendar.SelectedIndex = 0;

			this.fLocation = null;

			this.fNotesList = new GKNotesSheet(this, this.SheetNotes);
            this.fMediaList = new GKMediaSheet(this, this.SheetMultimedia);
			this.fSourcesList = new GKSourcesSheet(this, this.SheetSources);

			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.btnAddress.Text = LangMan.LS(LSID.LSID_Address) + "...";
			this.SheetCommon.Text = LangMan.LS(LSID.LSID_Common);
			this.SheetNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
			this.SheetMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
			this.SheetSources.Text = LangMan.LS(LSID.LSID_RPSources);
			this.Label1.Text = LangMan.LS(LSID.LSID_Event);
			this.LabelAttr.Text = LangMan.LS(LSID.LSID_Value);
			this.Label2.Text = LangMan.LS(LSID.LSID_Place);
			this.Label3.Text = LangMan.LS(LSID.LSID_Date);
			this.Label4.Text = LangMan.LS(LSID.LSID_Cause);
			this.Label5.Text = LangMan.LS(LSID.LSID_Agency);
		}

	}
}
