using System;
using System.Drawing;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKCore;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI
{
	public partial class TfmEventEdit : Form
	{
		private GKSheetList FNotesList;
		private GKSheetList FMediaList;
		private GKSheetList FSourcesList;

		private TfmBase FBase;
		private TGEDCOMCustomEvent FEvent;
		private TGEDCOMLocationRecord FLocation;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		public TGEDCOMCustomEvent Event
		{
			get { return this.FEvent; }
			set { this.SetEvent(value); }
		}

		private string AssembleDate()
		{
			string result = "";

			TGEDCOMCalendar cal = (TGEDCOMCalendar)this.cbDate1Calendar.SelectedIndex;
			TGEDCOMCalendar cal2 = (TGEDCOMCalendar)this.cbDate2Calendar.SelectedIndex;

			string gcd = GKUtils.StrToGEDCOMDate(this.EditEventDate1.Text, true);
			string gcd2 = GKUtils.StrToGEDCOMDate(this.EditEventDate2.Text, true);

			if (cal != TGEDCOMCalendar.dcGregorian) {
				gcd = TGEDCOMDate.GEDCOMDateEscapeArray[(int)cal] + " " + gcd;
			}

			if (cal2 != TGEDCOMCalendar.dcGregorian) {
				gcd2 = TGEDCOMDate.GEDCOMDateEscapeArray[(int)cal2] + " " + gcd2;
			}

			if (btnBC1.Checked) {
				gcd = gcd + TGEDCOMObject.GEDCOMYearBC;
			}

			if (btnBC2.Checked) {
				gcd2 = gcd2 + TGEDCOMObject.GEDCOMYearBC;
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
			this.FEvent.Detail.Place.StringValue = this.EditEventPlace.Text;
			this.FEvent.Detail.Place.Location.Value = this.FLocation;
			this.FEvent.Detail.Classification = this.EditEventName.Text;
			this.FEvent.Detail.Cause = this.EditEventCause.Text;
			this.FEvent.Detail.Agency = this.EditEventOrg.Text;

			string dt = this.AssembleDate();
			this.FEvent.Detail.Date.ParseString(dt);

			if (this.FEvent is TGEDCOMFamilyEvent)
			{
				this.FEvent.Name = GKData.FamilyEvents[this.EditEventType.SelectedIndex].Sign;
			}
			else
			{
				int id = this.EditEventType.SelectedIndex;
				this.FEvent.Name = GKData.PersonEvents[id].Sign;
				if (GKData.PersonEvents[id].Kind == TPersonEventKind.ekFact)
				{
					this.FEvent.StringValue = this.EditAttribute.Text;
				}
				else
				{
					this.FEvent.StringValue = "";
				}
			}

			if (this.FEvent is TGEDCOMIndividualEvent)
			{
				int id = this.EditEventType.SelectedIndex;
				if (GKData.PersonEvents[id].Kind == TPersonEventKind.ekFact)
				{
					TGEDCOMIndividualAttribute attr = new TGEDCOMIndividualAttribute(this.FEvent.Owner, this.FEvent.Parent, "", "");
					attr.Assign(this.FEvent);
					this.FEvent = attr;
				}
			}
		}

		private void ControlsRefresh()
		{
			if (this.FLocation != null) {
				this.EditEventPlace.Text = this.FLocation.LocationName;
				this.EditEventPlace.ReadOnly = true;
				this.EditEventPlace.BackColor = SystemColors.Control;
				this.btnPlaceAdd.Enabled = false;
				this.btnPlaceDelete.Enabled = true;
			} else {
				this.EditEventPlace.Text = this.FEvent.Detail.Place.StringValue;
				this.EditEventPlace.ReadOnly = false;
				this.EditEventPlace.BackColor = SystemColors.Window;
				this.btnPlaceAdd.Enabled = true;
				this.btnPlaceDelete.Enabled = false;
			}
			this.FNotesList.List.Items.Clear();

			int num = this.FEvent.Detail.Notes.Count - 1;
			for (int idx = 0; idx <= num; idx++)
			{
				TGEDCOMNotes note = this.FEvent.Detail.Notes[idx];
				string st;
				if (note.Notes.Count > 0)
				{
					st = note.Notes[0].Trim();
					if (st == "" && note.Notes.Count > 1)
					{
						st = note.Notes[1].Trim();
					}
				}
				else
				{
					st = "";
				}
				this.FNotesList.List.AddItem(st, note);
			}

			this.FMediaList.List.Items.Clear();
			int num2 = this.FEvent.Detail.MultimediaLinks.Count - 1;
			for (int idx = 0; idx <= num2; idx++)
			{
				TGEDCOMMultimediaLink mmLink = this.FEvent.Detail.MultimediaLinks[idx];
				TGEDCOMMultimediaRecord mmRec = mmLink.Value as TGEDCOMMultimediaRecord;
				if (mmRec == null) continue;

				if (mmRec != null && mmRec.FileReferences.Count != 0)
				{
					string st = mmRec.FileReferences[0].Title;
					this.FMediaList.List.AddItem(st, mmLink);
				}
			}

			this.FSourcesList.List.Items.Clear();
			int num3 = this.FEvent.Detail.SourceCitations.Count - 1;
			for (int idx = 0; idx <= num3; idx++)
			{
				TGEDCOMSourceCitation cit = this.FEvent.Detail.SourceCitations[idx];
				TGEDCOMSourceRecord sourceRec = cit.Value as TGEDCOMSourceRecord;
				if (sourceRec == null) continue;

				string st = "\"" + sourceRec.FiledByEntry + "\"";

				if (cit.Page != "") {
					st = st + ", " + cit.Page;
				}

				if (sourceRec != null) {
					ListViewItem item = this.FSourcesList.List.AddItem(sourceRec.Originator.Text.Trim(), cit);
					item.SubItems.Add(st);
				}
			}
		}

		private void ListModify(object sender, ModifyEventArgs eArgs)
		{
			bool res = false;

			if (sender == this.FNotesList) {
				res = this.Base.ModifyTagNote(this.FEvent.Detail, eArgs.ItemData as TGEDCOMNotes, eArgs.Action);
			} else if (sender == this.FMediaList) {
				res = this.Base.ModifyTagMultimedia(this.FEvent.Detail, eArgs.ItemData as TGEDCOMMultimediaLink, eArgs.Action);
			} else if (sender == this.FSourcesList) {
				res = this.Base.ModifyTagSource(this.FEvent.Detail, eArgs.ItemData as TGEDCOMSourceCitation, eArgs.Action);
			}

			if (res) this.ControlsRefresh();
		}

		private void SetEvent(TGEDCOMCustomEvent Value)
		{
			this.FEvent = Value;

			if (this.FEvent is TGEDCOMFamilyEvent)
			{
				for (int i = 0; i <= GKData.FamilyEvents.Length - 1; i++)
				{
					this.EditEventType.Items.Add(LangMan.LSList[(int)GKData.FamilyEvents[i].Name - 1]);
				}

				int idx = GKUtils.GetFamilyEventIndex(this.FEvent.Name);
				if (idx < 0) idx = 0;
				this.EditEventType.SelectedIndex = idx;
			}
			else
			{
				for (int i = 0; i <= GKData.PersonEvents.Length - 1; i++)
				{
					this.EditEventType.Items.Add(LangMan.LSList[(int)GKData.PersonEvents[i].Name - 1]);
				}

				int idx = GKUtils.GetPersonEventIndex(this.FEvent.Name);
				if (idx < 0) idx = 0;
				this.EditEventType.SelectedIndex = idx;

				if (idx >= 0 && GKData.PersonEvents[idx].Kind == TPersonEventKind.ekFact)
				{
					this.EditAttribute.Text = this.FEvent.StringValue;
				}
			}

			this.EditEventType_SelectedIndexChanged(null, null);

			TGEDCOMCustomDate date = this.FEvent.Detail.Date.Value;
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

				this.EditEventDate1.Text = GKUtils.GEDCOMDateToStr(date as TGEDCOMDate, TDateFormat.dfDD_MM_YYYY);
				this.cbDate1Calendar.SelectedIndex = (int)(date as TGEDCOMDate).DateCalendar;
				this.btnBC1.Checked = (date as TGEDCOMDate).YearBC;
			}
			else
			{
				if (date is TGEDCOMDateRange)
				{
					TGEDCOMDateRange dt_range = date as TGEDCOMDateRange;
					if (dt_range.After.StringValue == "" && dt_range.Before.StringValue != "")
					{
						this.EditEventDateType.SelectedIndex = 1;
					}
					else
					{
						if (dt_range.After.StringValue != "" && dt_range.Before.StringValue == "")
						{
							this.EditEventDateType.SelectedIndex = 2;
						}
						else
						{
							if (dt_range.After.StringValue != "" && dt_range.Before.StringValue != "")
							{
								this.EditEventDateType.SelectedIndex = 3;
							}
						}
					}

					this.EditEventDate1.Text = GKUtils.GEDCOMDateToStr(dt_range.After, TDateFormat.dfDD_MM_YYYY);
					this.EditEventDate2.Text = GKUtils.GEDCOMDateToStr(dt_range.Before, TDateFormat.dfDD_MM_YYYY);
					this.cbDate1Calendar.SelectedIndex = (int)dt_range.After.DateCalendar;
					this.cbDate2Calendar.SelectedIndex = (int)dt_range.Before.DateCalendar;
					this.btnBC1.Checked = (dt_range.After as TGEDCOMDate).YearBC;
					this.btnBC2.Checked = (dt_range.Before as TGEDCOMDate).YearBC;
				}
				else
				{
					if (date is TGEDCOMDatePeriod)
					{
						TGEDCOMDatePeriod dt_period = date as TGEDCOMDatePeriod;
						if (dt_period.DateFrom.StringValue != "" && dt_period.DateTo.StringValue == "")
						{
							this.EditEventDateType.SelectedIndex = 4;
						}
						else
						{
							if (dt_period.DateFrom.StringValue == "" && dt_period.DateTo.StringValue != "")
							{
								this.EditEventDateType.SelectedIndex = 5;
							}
							else
							{
								if (dt_period.DateFrom.StringValue != "" && dt_period.DateTo.StringValue != "")
								{
									this.EditEventDateType.SelectedIndex = 6;
								}
							}
						}

						this.EditEventDate1.Text = GKUtils.GEDCOMDateToStr(dt_period.DateFrom, TDateFormat.dfDD_MM_YYYY);
						this.EditEventDate2.Text = GKUtils.GEDCOMDateToStr(dt_period.DateTo, TDateFormat.dfDD_MM_YYYY);
						this.cbDate1Calendar.SelectedIndex = (int)dt_period.DateFrom.DateCalendar;
						this.cbDate2Calendar.SelectedIndex = (int)dt_period.DateTo.DateCalendar;
						this.btnBC1.Checked = (dt_period.DateFrom as TGEDCOMDate).YearBC;
						this.btnBC2.Checked = (dt_period.DateTo as TGEDCOMDate).YearBC;
					}
					else
					{
						if (date is TGEDCOMDate)
						{
							this.EditEventDateType.SelectedIndex = 0;
							this.EditEventDate1.Text = GKUtils.GEDCOMDateToStr(date as TGEDCOMDate, TDateFormat.dfDD_MM_YYYY);
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
			this.EditEventName.Text = this.FEvent.Detail.Classification;
			this.EditEventCause.Text = this.FEvent.Detail.Cause;
			this.EditEventOrg.Text = this.FEvent.Detail.Agency;
			this.FLocation = (this.FEvent.Detail.Place.Location.Value as TGEDCOMLocationRecord);
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
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmEventEdit.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void btnAddress_Click(object sender, EventArgs e)
		{
			this.Base.ModifyAddress(this, this.FEvent.Detail.Address);
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
			this.FLocation = (this.Base.SelectRecord(TGEDCOMRecordType.rtLocation, null) as TGEDCOMLocationRecord);
			this.ControlsRefresh();
		}

		private void btnPlaceDelete_Click(object sender, EventArgs e)
		{
			this.FLocation = null;
			this.ControlsRefresh();
		}

		private void EditEventDate1_DragOver(object sender, DragEventArgs e)
		{
			if (e.Data.GetDataPresent(typeof(string))) {
				e.Effect = DragDropEffects.Move;
			} else {
				e.Effect = DragDropEffects.None;
			}
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
			catch (Exception E)
			{
				SysUtils.LogWrite("EventEdit.DragDrop(): " + E.Message);
			}
		}

		private void EditEventType_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (this.FEvent is TGEDCOMFamilyEvent)
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
			if (this.FEvent is TGEDCOMFamilyEvent) {
				evName = GKData.FamilyEvents[id].Sign;
			} else {
				evName = GKData.PersonEvents[id].Sign;
			}

			string[] vals = this.FBase.ValuesCollection.GetValues(evName);
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
			ctl.Enabled = enabled;
			
			if (enabled) {
				ctl.BackColor = SystemColors.Window;
			} else {
				ctl.BackColor = SystemColors.Control;
			}
		}

		private void EditEventDateType_SelectedIndexChanged(object sender, EventArgs e)
		{
			int idx = this.EditEventDateType.SelectedIndex;
			if (idx < 0 || idx >= GKData.DateKinds.Length) return;

			TDateControlsRange dates = GKData.DateKinds[idx].Dates;

			this.EditEventDate1.Enabled = ((dates & (TDateControlsRange)2) > (TDateControlsRange)0);
			this.EditEventDate2.Enabled = ((dates & (TDateControlsRange)4) > (TDateControlsRange)0);

			this.cbDate1Calendar.Enabled = this.EditEventDate1.Enabled;
			this.cbDate2Calendar.Enabled = this.EditEventDate2.Enabled;

			this.btnBC1.Enabled = this.EditEventDate1.Enabled;
			this.btnBC2.Enabled = this.EditEventDate2.Enabled;
		}

		public TfmEventEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;

			for (int i = 0; i <= GKData.DateKinds.Length - 1; i++)
			{
				this.EditEventDateType.Items.Add(LangMan.LSList[(int)GKData.DateKinds[i].Name - 1]);
			}

			for (TGEDCOMCalendar gc = TGEDCOMCalendar.dcGregorian; gc <= TGEDCOMCalendar.dcLast; gc++)
			{
				this.cbDate1Calendar.Items.Add(LangMan.LS(GKData.DateCalendars[(int)gc]));
				this.cbDate2Calendar.Items.Add(LangMan.LS(GKData.DateCalendars[(int)gc]));
			}

			this.cbDate1Calendar.SelectedIndex = 0;
			this.cbDate2Calendar.SelectedIndex = 0;

			this.FLocation = null;

			this.FNotesList = new GKSheetList(this.SheetNotes);
			this.FNotesList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.Base.SetupRecNotesList(this.FNotesList);

			this.FMediaList = new GKSheetList(this.SheetMultimedia);
			this.FMediaList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.Base.SetupRecMediaList(this.FMediaList);

			this.FSourcesList = new GKSheetList(this.SheetSources);
			this.FSourcesList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.Base.SetupRecSourcesList(this.FSourcesList);

			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.btnAddress.Text = LangMan.LSList[82] + "...";
			this.SheetCommon.Text = LangMan.LSList[144];
			this.SheetNotes.Text = LangMan.LSList[54];
			this.SheetMultimedia.Text = LangMan.LSList[55];
			this.SheetSources.Text = LangMan.LSList[56];
			this.Label1.Text = LangMan.LSList[203];
			this.LabelAttr.Text = LangMan.LSList[202];
			this.Label2.Text = LangMan.LSList[204];
			this.Label3.Text = LangMan.LSList[139];
			this.Label4.Text = LangMan.LSList[205];
			this.Label5.Text = LangMan.LSList[206];
		}

	}
}
