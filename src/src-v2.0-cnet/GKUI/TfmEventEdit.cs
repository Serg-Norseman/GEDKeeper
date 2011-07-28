using GedCom551;
using GKCore;
using GKUI.Lists;
using GKSys;
using System;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmEventEdit : Form
	{
		private Button btnAccept;
		private Button btnCancel;
		private TabControl PageEventData;
		private TabPage SheetNotes;
		private TabPage SheetMultimedia;
		private TabPage SheetSources;
		private Button btnAddress;
		private TabPage SheetCommon;
		private Label Label1;
		private Label Label2;
		private Label Label3;
		private Label Label4;
		private Label Label5;
		private Label LabelAttr;
		private ComboBox EditEventType;
		private TextBox EditEventName;
		private TextBox EditEventPlace;
		private ComboBox EditEventDateType;
		private MaskedTextBox EditEventDate1;
		private MaskedTextBox EditEventDate2;
		private TextBox EditEventCause;
		private TextBox EditEventOrg;
		private TextBox EditAttribute;
		private Button btnPlaceAdd;
		private Button btnPlaceDelete;
		private Button btnPlaceSel;
		private ComboBox cbDate1Calendar;
		private ComboBox cbDate2Calendar;
		private TSheetList FNotesList;
		private TSheetList FMediaList;
		private TSheetList FSourcesList;
		private TfmBase FBase;
		private TGEDCOMCustomEvent FEvent;
		private TGEDCOMLocationRecord FLocation;

		[Browsable(false)]
		public TfmBase Base
		{
			get
			{
				return this.FBase;
			}
		}
		[Browsable(false)]
		public TGEDCOMCustomEvent Event
		{
			get
			{
				return this.FEvent;
			}
			set
			{
				this.SetEvent(value);
			}
		}
		private void AcceptChanges()
		{
			this.FEvent.Detail.Place.StringValue = this.EditEventPlace.Text;
			this.FEvent.Detail.Place.Location.Value = this.FLocation;
			this.FEvent.Detail.Classification = this.EditEventName.Text;
			this.FEvent.Detail.Cause = this.EditEventCause.Text;
			this.FEvent.Detail.Agency = this.EditEventOrg.Text;
			TGEDCOMDate.TGEDCOMCalendar cal = (TGEDCOMDate.TGEDCOMCalendar)this.cbDate1Calendar.SelectedIndex;
			TGEDCOMDate.TGEDCOMCalendar cal2 = (TGEDCOMDate.TGEDCOMCalendar)this.cbDate2Calendar.SelectedIndex;
			string gcd = TGenEngine.StrToGEDCOMDate(this.EditEventDate1.Text, true);
			string gcd2 = TGenEngine.StrToGEDCOMDate(this.EditEventDate2.Text, true);
			if (cal != TGEDCOMDate.TGEDCOMCalendar.dcGregorian)
			{
				gcd = TGEDCOMDate.GEDCOMDateEscapeArray[(int)cal] + " " + gcd;
			}
			if (cal2 != TGEDCOMDate.TGEDCOMCalendar.dcGregorian)
			{
				gcd2 = TGEDCOMDate.GEDCOMDateEscapeArray[(int)cal2] + " " + gcd2;
			}
			string dt = "";
			switch (this.EditEventDateType.SelectedIndex)
			{
				case 0:
				{
					dt = gcd;
					break;
				}
				case 1:
				{
					dt = "BEF " + gcd2;
					break;
				}
				case 2:
				{
					dt = "AFT " + gcd;
					break;
				}
				case 3:
				{
					dt = "BET " + gcd + " AND " + gcd2;
					break;
				}
				case 4:
				{
					dt = "FROM " + gcd;
					break;
				}
				case 5:
				{
					dt = "TO " + gcd2;
					break;
				}
				case 6:
				{
					dt = "FROM " + gcd + " TO " + gcd2;
					break;
				}
				case 7:
				{
					dt = "ABT " + gcd;
					break;
				}
				case 8:
				{
					dt = "CAL " + gcd;
					break;
				}
				case 9:
				{
					dt = "EST " + gcd;
					break;
				}
			}
			this.FEvent.Detail.Date.ParseString(dt);
			if (this.FEvent is TGEDCOMFamilyEvent)
			{
				this.FEvent.Name = TGenEngine.FamilyEvents[this.EditEventType.SelectedIndex].Sign;
			}
			else
			{
				int id = this.EditEventType.SelectedIndex;
				this.FEvent.Name = TGenEngine.PersonEvents[id].Sign;
				if (TGenEngine.PersonEvents[id].Kind == TGenEngine.TPersonEventKind.ekFact)
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
				if (TGenEngine.PersonEvents[id].Kind == TGenEngine.TPersonEventKind.ekFact)
				{
					TGEDCOMIndividualAttribute attr = new TGEDCOMIndividualAttribute(this.FEvent.Owner, this.FEvent.Parent, "", "");
					attr.Name = this.FEvent.Name;
					attr.Assign(this.FEvent);
					this.FEvent = attr;
				}
			}
		}

		private void ControlsRefresh()
		{
			if (this.FLocation != null)
			{
				this.EditEventPlace.Text = this.FLocation.Name;
				this.EditEventPlace.ReadOnly = true;
				this.EditEventPlace.BackColor = SystemColors.Control;
				this.btnPlaceAdd.Enabled = false;
				this.btnPlaceDelete.Enabled = true;
			}
			else
			{
				this.EditEventPlace.Text = this.FEvent.Detail.Place.StringValue;
				this.EditEventPlace.ReadOnly = false;
				this.EditEventPlace.BackColor = SystemColors.Window;
				this.btnPlaceAdd.Enabled = true;
				this.btnPlaceDelete.Enabled = false;
			}
			this.FNotesList.List.Items.Clear();
			int arg_D2_0 = 0;
			int num = this.FEvent.Detail.GetNotesCount() - 1;
			int idx = arg_D2_0;
			if (num >= idx)
			{
				num++;
				do
				{
					TGEDCOMNotes note = this.FEvent.Detail.GetNote(idx);
					string st;
					if (note.Notes.Count > 0)
					{
						st = note.Notes[0].Trim();
						if (BDSSystem.WStrCmp(st, "") == 0 && note.Notes.Count > 1)
						{
							st = note.Notes[1].Trim();
						}
					}
					else
					{
						st = "";
					}
					this.FNotesList.List.AddItem(st, note);
					idx++;
				}
				while (idx != num);
			}
			this.FMediaList.List.Items.Clear();
			int arg_196_0 = 0;
			int num2 = this.FEvent.Detail.GetMultimediaLinksCount() - 1;
			idx = arg_196_0;
			if (num2 >= idx)
			{
				num2++;
				do
				{
					TGEDCOMMultimediaLink mmLink = this.FEvent.Detail.GetMultimediaLink(idx);
					TGEDCOMMultimediaRecord mmRec = mmLink.Value as TGEDCOMMultimediaRecord;
					if (mmRec != null && mmRec.GetFileReferencesCount() != 0)
					{
						string st = mmRec.GetFileReference(0).Title;
						this.FMediaList.List.AddItem(st, mmLink);
					}
					idx++;
				}
				while (idx != num2);
			}
			this.FSourcesList.List.Items.Clear();
			int arg_223_0 = 0;
			int num3 = this.FEvent.Detail.GetSourceCitationsCount() - 1;
			idx = arg_223_0;
			if (num3 >= idx)
			{
				num3++;
				do
				{
					TGEDCOMSourceCitation cit = this.FEvent.Detail.GetSourceCitation(idx);
					TGEDCOMSourceRecord sourceRec = cit.Value as TGEDCOMSourceRecord;
					string st = "\"" + sourceRec.FiledByEntry + "\"";
					if (BDSSystem.WStrCmp(cit.Page, "") != 0)
					{
						st = st + ", " + cit.Page;
					}
					if (sourceRec != null)
					{
						ListViewItem item = this.FSourcesList.List.AddItem(sourceRec.Originator.Text.Trim(), cit);
						item.SubItems.Add(st);
					}
					idx++;
				}
				while (idx != num3);
			}
		}

		private void ListModify(object Sender, object ItemData, TGenEngine.TRecAction Action)
		{
			if (object.Equals(Sender, this.FNotesList))
			{
				if (this.Base.ModifyTagNote(this.FEvent.Detail, ItemData as TGEDCOMNotes, Action))
				{
					this.ControlsRefresh();
				}
			}
			else
			{
				if (object.Equals(Sender, this.FMediaList))
				{
					if (this.Base.ModifyTagMultimedia(this.FEvent.Detail, ItemData as TGEDCOMMultimediaLink, Action))
					{
						this.ControlsRefresh();
					}
				}
				else
				{
					if (object.Equals(Sender, this.FSourcesList) && this.Base.ModifyTagSource(this.FEvent.Detail, ItemData as TGEDCOMSourceCitation, Action))
					{
						this.ControlsRefresh();
					}
				}
			}
		}
		private void SetEvent([In] TGEDCOMCustomEvent Value)
		{
			this.FEvent = Value;
			if (this.FEvent is TGEDCOMFamilyEvent)
			{
				int i = 0;
				do
				{
					this.EditEventType.Items.Add(GKL.LSList[(int)TGenEngine.FamilyEvents[i].Name - 1]);
					i++;
				}
				while (i != 10);
				this.EditEventType.SelectedIndex = TGenEngine.GetFamilyEventIndex(this.FEvent.Name);
			}
			else
			{
				int i = 0;
				do
				{
					this.EditEventType.Items.Add(GKL.LSList[(int)TGenEngine.PersonEvents[i].Name - 1]);
					i++;
				}
				while (i != 36);
				i = TGenEngine.GetPersonEventIndex(this.FEvent.Name);
				this.EditEventType.SelectedIndex = i;
				if (TGenEngine.PersonEvents[i].Kind == TGenEngine.TPersonEventKind.ekFact)
				{
					this.EditAttribute.Text = this.FEvent.StringValue;
				}
			}
			this.EditEventType_SelectedIndexChanged(null, null);
			TGEDCOMCustomDate date = this.FEvent.Detail.Date.Value;
			if (date is TGEDCOMDateApproximated)
			{
				TGEDCOMDateApproximated.TGEDCOMApproximated approximated = (date as TGEDCOMDateApproximated).Approximated;
				if (approximated != TGEDCOMDateApproximated.TGEDCOMApproximated.daExact)
				{
					if (approximated != TGEDCOMDateApproximated.TGEDCOMApproximated.daAbout)
					{
						if (approximated != TGEDCOMDateApproximated.TGEDCOMApproximated.daCalculated)
						{
							if (approximated == TGEDCOMDateApproximated.TGEDCOMApproximated.daEstimated)
							{
								this.EditEventDateType.SelectedIndex = 9;
							}
						}
						else
						{
							this.EditEventDateType.SelectedIndex = 8;
						}
					}
					else
					{
						this.EditEventDateType.SelectedIndex = 7;
					}
				}
				else
				{
					this.EditEventDateType.SelectedIndex = 0;
				}
				this.EditEventDate1.Text = TGenEngine.GEDCOMDateToStr(date as TGEDCOMDate, TGenEngine.TDateFormat.dfDD_MM_YYYY);
				this.cbDate1Calendar.SelectedIndex = (int)((sbyte)(date as TGEDCOMDate).DateCalendar);
			}
			else
			{
				if (date is TGEDCOMDateRange)
				{
					TGEDCOMDateRange dt_range = date as TGEDCOMDateRange;
					if (BDSSystem.WStrCmp(dt_range.After.StringValue, "") == 0 && BDSSystem.WStrCmp(dt_range.Before.StringValue, "") != 0)
					{
						this.EditEventDateType.SelectedIndex = 1;
					}
					else
					{
						if (BDSSystem.WStrCmp(dt_range.After.StringValue, "") != 0 && BDSSystem.WStrCmp(dt_range.Before.StringValue, "") == 0)
						{
							this.EditEventDateType.SelectedIndex = 2;
						}
						else
						{
							if (BDSSystem.WStrCmp(dt_range.After.StringValue, "") != 0 && BDSSystem.WStrCmp(dt_range.Before.StringValue, "") != 0)
							{
								this.EditEventDateType.SelectedIndex = 3;
							}
						}
					}
					this.EditEventDate1.Text = TGenEngine.GEDCOMDateToStr(dt_range.After, TGenEngine.TDateFormat.dfDD_MM_YYYY);
					this.EditEventDate2.Text = TGenEngine.GEDCOMDateToStr(dt_range.Before, TGenEngine.TDateFormat.dfDD_MM_YYYY);
					this.cbDate1Calendar.SelectedIndex = (int)((sbyte)dt_range.After.DateCalendar);
					this.cbDate2Calendar.SelectedIndex = (int)((sbyte)dt_range.Before.DateCalendar);
				}
				else
				{
					if (date is TGEDCOMDatePeriod)
					{
						TGEDCOMDatePeriod dt_period = date as TGEDCOMDatePeriod;
						if (BDSSystem.WStrCmp(dt_period.DateFrom.StringValue, "") != 0 && BDSSystem.WStrCmp(dt_period.DateTo.StringValue, "") == 0)
						{
							this.EditEventDateType.SelectedIndex = 4;
						}
						else
						{
							if (BDSSystem.WStrCmp(dt_period.DateFrom.StringValue, "") == 0 && BDSSystem.WStrCmp(dt_period.DateTo.StringValue, "") != 0)
							{
								this.EditEventDateType.SelectedIndex = 5;
							}
							else
							{
								if (BDSSystem.WStrCmp(dt_period.DateFrom.StringValue, "") != 0 && BDSSystem.WStrCmp(dt_period.DateTo.StringValue, "") != 0)
								{
									this.EditEventDateType.SelectedIndex = 6;
								}
							}
						}
						this.EditEventDate1.Text = TGenEngine.GEDCOMDateToStr(dt_period.DateFrom, TGenEngine.TDateFormat.dfDD_MM_YYYY);
						this.EditEventDate2.Text = TGenEngine.GEDCOMDateToStr(dt_period.DateTo, TGenEngine.TDateFormat.dfDD_MM_YYYY);
						this.cbDate1Calendar.SelectedIndex = (int)((sbyte)dt_period.DateFrom.DateCalendar);
						this.cbDate2Calendar.SelectedIndex = (int)((sbyte)dt_period.DateTo.DateCalendar);
					}
					else
					{
						if (date is TGEDCOMDate)
						{
							this.EditEventDateType.SelectedIndex = 0;
							this.EditEventDate1.Text = TGenEngine.GEDCOMDateToStr(date as TGEDCOMDate, TGenEngine.TDateFormat.dfDD_MM_YYYY);
							this.cbDate1Calendar.SelectedIndex = (int)((sbyte)(date as TGEDCOMDate).DateCalendar);
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
		}

		private void InitializeComponent()
		{
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.PageEventData = new TabControl();
			this.SheetCommon = new TabPage();
			this.Label1 = new Label();
			this.Label2 = new Label();
			this.Label3 = new Label();
			this.Label4 = new Label();
			this.Label5 = new Label();
			this.LabelAttr = new Label();
			this.btnPlaceAdd = new Button();
			this.btnPlaceDelete = new Button();
			this.btnPlaceSel = new Button();
			this.EditEventType = new ComboBox();
			this.EditEventName = new TextBox();
			this.EditEventPlace = new TextBox();
			this.EditEventDateType = new ComboBox();
			this.EditEventDate1 = new MaskedTextBox();
			this.EditEventDate2 = new MaskedTextBox();
			this.EditEventCause = new TextBox();
			this.EditEventOrg = new TextBox();
			this.EditAttribute = new TextBox();
			this.cbDate1Calendar = new ComboBox();
			this.cbDate2Calendar = new ComboBox();
			this.SheetNotes = new TabPage();
			this.SheetMultimedia = new TabPage();
			this.SheetSources = new TabPage();
			this.btnAddress = new Button();
			this.PageEventData.SuspendLayout();
			this.SheetCommon.SuspendLayout();
			base.SuspendLayout();
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(240, 368);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 2;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(328, 368);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			this.PageEventData.Controls.Add(this.SheetCommon);
			this.PageEventData.Controls.Add(this.SheetNotes);
			this.PageEventData.Controls.Add(this.SheetMultimedia);
			this.PageEventData.Controls.Add(this.SheetSources);
			this.PageEventData.Location = new Point(0, 0);
			this.PageEventData.Name = "PageEventData";
			this.PageEventData.SelectedIndex = 0;
			this.PageEventData.Size = new Size(418, 353);
			this.PageEventData.TabIndex = 0;
			this.SheetCommon.Controls.Add(this.Label1);
			this.SheetCommon.Controls.Add(this.Label2);
			this.SheetCommon.Controls.Add(this.Label3);
			this.SheetCommon.Controls.Add(this.Label4);
			this.SheetCommon.Controls.Add(this.Label5);
			this.SheetCommon.Controls.Add(this.LabelAttr);
			this.SheetCommon.Controls.Add(this.btnPlaceAdd);
			this.SheetCommon.Controls.Add(this.btnPlaceDelete);
			this.SheetCommon.Controls.Add(this.btnPlaceSel);
			this.SheetCommon.Controls.Add(this.EditEventType);
			this.SheetCommon.Controls.Add(this.EditEventName);
			this.SheetCommon.Controls.Add(this.EditEventPlace);
			this.SheetCommon.Controls.Add(this.EditEventDateType);
			this.SheetCommon.Controls.Add(this.EditEventDate1);
			this.SheetCommon.Controls.Add(this.EditEventDate2);
			this.SheetCommon.Controls.Add(this.EditEventCause);
			this.SheetCommon.Controls.Add(this.EditEventOrg);
			this.SheetCommon.Controls.Add(this.EditAttribute);
			this.SheetCommon.Controls.Add(this.cbDate1Calendar);
			this.SheetCommon.Controls.Add(this.cbDate2Calendar);
			this.SheetCommon.Location = new Point(4, 22);
			this.SheetCommon.Name = "SheetCommon";
			this.SheetCommon.Size = new Size(410, 327);
			this.SheetCommon.TabIndex = 0;
			this.SheetCommon.Text = "Общее";
			this.Label1.Location = new Point(8, 8);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(50, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Событие";
			this.Label2.Location = new Point(8, 104);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(40, 13);
			this.Label2.TabIndex = 1;
			this.Label2.Text = "Место";
			this.Label3.Location = new Point(8, 152);
			this.Label3.Name = "Label3";
			this.Label3.Size = new Size(30, 13);
			this.Label3.TabIndex = 2;
			this.Label3.Text = "Дата";
			this.Label4.Location = new Point(8, 232);
			this.Label4.Name = "Label4";
			this.Label4.Size = new Size(50, 13);
			this.Label4.TabIndex = 3;
			this.Label4.Text = "Причина";
			this.Label5.Location = new Point(8, 280);
			this.Label5.Name = "Label5";
			this.Label5.Size = new Size(165, 13);
			this.Label5.TabIndex = 4;
			this.Label5.Text = "Засвидетельствовавший орган";
			this.LabelAttr.Location = new Point(8, 56);
			this.LabelAttr.Name = "LabelAttr";
			this.LabelAttr.Size = new Size(120, 13);
			this.LabelAttr.TabIndex = 5;
			this.LabelAttr.Text = "Значение атрибута";
			this.btnPlaceAdd.AccessibleDescription = "Выбрать или добавить место";
			this.btnPlaceAdd.Enabled = false;
			this.btnPlaceAdd.Location = new Point(312, 117);
			this.btnPlaceAdd.Name = "btnPlaceAdd";
			this.btnPlaceAdd.Size = new Size(26, 26);
			this.btnPlaceAdd.TabIndex = 6;
			this.btnPlaceAdd.Click += new EventHandler(this.btnPlaceAdd_Click);
			this.btnPlaceDelete.AccessibleDescription = "Отсоединить место";
			this.btnPlaceDelete.Enabled = false;
			this.btnPlaceDelete.Location = new Point(344, 117);
			this.btnPlaceDelete.Name = "btnPlaceDelete";
			this.btnPlaceDelete.Size = new Size(26, 26);
			this.btnPlaceDelete.TabIndex = 7;
			this.btnPlaceDelete.Click += new EventHandler(this.btnPlaceDelete_Click);
			this.btnPlaceSel.AccessibleDescription = "Перейти на запись места";
			this.btnPlaceSel.Enabled = false;
			this.btnPlaceSel.Location = new Point(376, 117);
			this.btnPlaceSel.Name = "btnPlaceSel";
			this.btnPlaceSel.Size = new Size(26, 26);
			this.btnPlaceSel.TabIndex = 8;
			this.btnPlaceSel.Click += new EventHandler(this.btnPlaceSel_Click);
			this.EditEventType.DropDownStyle = ComboBoxStyle.DropDownList;
			this.EditEventType.Location = new Point(8, 24);
			this.EditEventType.Name = "EditEventType";
			this.EditEventType.Size = new Size(185, 21);
			this.EditEventType.TabIndex = 0;
			this.EditEventType.SelectedIndexChanged += new EventHandler(this.EditEventType_SelectedIndexChanged);
			this.EditEventName.Location = new Point(200, 24);
			this.EditEventName.Name = "EditEventName";
			this.EditEventName.Size = new Size(201, 21);
			this.EditEventName.TabIndex = 1;
			this.EditEventName.Text = "";
			this.EditEventPlace.Location = new Point(8, 120);
			this.EditEventPlace.Name = "EditEventPlace";
			this.EditEventPlace.Size = new Size(297, 21);
			this.EditEventPlace.TabIndex = 3;
			this.EditEventPlace.Text = "";
			this.EditEventPlace.KeyDown += new KeyEventHandler(this.EditEventPlace_KeyDown);
			this.EditEventDateType.DropDownStyle = ComboBoxStyle.DropDownList;
			this.EditEventDateType.Location = new Point(8, 168);
			this.EditEventDateType.Name = "EditEventDateType";
			this.EditEventDateType.Size = new Size(113, 21);
			this.EditEventDateType.TabIndex = 4;
			this.EditEventDateType.SelectedIndexChanged += new EventHandler(this.EditEventDateType_SelectedIndexChanged);
			this.EditEventDate1.AllowDrop = true;
			this.EditEventDate1.BackColor = SystemColors.Window;
			this.EditEventDate1.Location = new Point(136, 168);
			this.EditEventDate1.Mask = "00/00/0000";
			this.EditEventDate1.MaxLength = 10;
			this.EditEventDate1.Name = "EditEventDate1";
			this.EditEventDate1.Size = new Size(129, 21);
			this.EditEventDate1.TabIndex = 5;
			this.EditEventDate1.Text = "  .  .    ";
			this.EditEventDate1.DragOver += new DragEventHandler(this.EditEventDate1_DragOver);
			this.EditEventDate1.DragDrop += new DragEventHandler(this.EditEventDate1_DragDrop);
			this.EditEventDate2.AllowDrop = true;
			this.EditEventDate2.Location = new Point(272, 168);
			this.EditEventDate2.Mask = "00/00/0000";
			this.EditEventDate2.MaxLength = 10;
			this.EditEventDate2.Name = "EditEventDate2";
			this.EditEventDate2.Size = new Size(129, 21);
			this.EditEventDate2.TabIndex = 6;
			this.EditEventDate2.Text = "  .  .    ";
			this.EditEventDate2.DragOver += new DragEventHandler(this.EditEventDate1_DragOver);
			this.EditEventDate2.DragDrop += new DragEventHandler(this.EditEventDate1_DragDrop);
			this.EditEventCause.Location = new Point(8, 248);
			this.EditEventCause.Name = "EditEventCause";
			this.EditEventCause.Size = new Size(393, 21);
			this.EditEventCause.TabIndex = 9;
			this.EditEventCause.Text = "";
			this.EditEventOrg.Location = new Point(8, 296);
			this.EditEventOrg.Name = "EditEventOrg";
			this.EditEventOrg.Size = new Size(393, 21);
			this.EditEventOrg.TabIndex = 10;
			this.EditEventOrg.Text = "";
			this.EditAttribute.Location = new Point(8, 72);
			this.EditAttribute.Name = "EditAttribute";
			this.EditAttribute.Size = new Size(393, 21);
			this.EditAttribute.TabIndex = 2;
			this.EditAttribute.Text = "";
			this.cbDate1Calendar.DropDownStyle = ComboBoxStyle.DropDownList;
			this.cbDate1Calendar.Location = new Point(136, 200);
			this.cbDate1Calendar.Name = "cbDate1Calendar";
			this.cbDate1Calendar.Size = new Size(129, 21);
			this.cbDate1Calendar.TabIndex = 7;
			this.cbDate2Calendar.DropDownStyle = ComboBoxStyle.DropDownList;
			this.cbDate2Calendar.Location = new Point(272, 200);
			this.cbDate2Calendar.Name = "cbDate2Calendar";
			this.cbDate2Calendar.Size = new Size(129, 21);
			this.cbDate2Calendar.TabIndex = 8;
			this.SheetNotes.Location = new Point(4, 22);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new Size(410, 327);
			this.SheetNotes.TabIndex = 1;
			this.SheetNotes.Text = "Заметки";
			this.SheetMultimedia.Location = new Point(4, 22);
			this.SheetMultimedia.Name = "SheetMultimedia";
			this.SheetMultimedia.Size = new Size(410, 327);
			this.SheetMultimedia.TabIndex = 2;
			this.SheetMultimedia.Text = "Мультимедиа";
			this.SheetSources.Location = new Point(4, 22);
			this.SheetSources.Name = "SheetSources";
			this.SheetSources.Size = new Size(410, 327);
			this.SheetSources.TabIndex = 3;
			this.SheetSources.Text = "Источники";
			this.btnAddress.Location = new Point(8, 368);
			this.btnAddress.Name = "btnAddress";
			this.btnAddress.Size = new Size(81, 25);
			this.btnAddress.TabIndex = 1;
			this.btnAddress.Text = "Адрес...";
			this.btnAddress.Click += new EventHandler(this.btnAddress_Click);
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(418, 401);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			base.Controls.Add(this.PageEventData);
			base.Controls.Add(this.btnAddress);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmEventEdit";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Событие";
			this.PageEventData.ResumeLayout(false);
			this.SheetCommon.ResumeLayout(false);
			base.ResumeLayout(false);
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
			TfmBase arg_11_0 = this.Base;
			TGEDCOMRecord.TGEDCOMRecordType arg_11_1 = TGEDCOMRecord.TGEDCOMRecordType.rtLocation;
			object[] anArgs = new object[0];
			this.FLocation = (arg_11_0.SelectRecord(arg_11_1, anArgs) as TGEDCOMLocationRecord);
			this.ControlsRefresh();
		}
		private void btnPlaceDelete_Click(object sender, EventArgs e)
		{
			this.FLocation = null;
			this.ControlsRefresh();
		}
		private void btnPlaceSel_Click(object sender, EventArgs e)
		{
		}
		private void EditEventDate1_DragOver(object sender, DragEventArgs e)
		{
			if (e.Data.GetDataPresent(typeof(string)))
			{
				e.Effect = DragDropEffects.Move;
			}
			else
			{
				e.Effect = DragDropEffects.None;
			}
		}
		private void EditEventDate1_DragDrop(object sender, DragEventArgs e)
		{
			if (e.Data.GetDataPresent(typeof(string)))
			{
				string txt = e.Data.GetData(typeof(string)) as string;
				string dt = ((TextBox)sender).Text;
				string sd = TGKSys.GetToken(dt, '.', 1);
				string sm = TGKSys.GetToken(dt, '.', 2);
				((TextBox)sender).Text = string.Concat(new string[]
				{
					sd, 
					".", 
					sm, 
					".", 
					txt
				});
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
				if (TGenEngine.PersonEvents[idx].Kind == TGenEngine.TPersonEventKind.ekEvent)
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
		private void EditEventDateType_SelectedIndexChanged(object sender, EventArgs e)
		{
			int idx = this.EditEventDateType.SelectedIndex;
			TGenEngine.TDateControlsRange dates = TGenEngine.DateKinds[idx].Dates;
			this.EditEventDate1.Enabled = ((dates & (TGenEngine.TDateControlsRange)2) > (TGenEngine.TDateControlsRange)0);
			if (this.EditEventDate1.Enabled)
			{
				this.EditEventDate1.BackColor = SystemColors.Window;
			}
			else
			{
				this.EditEventDate1.BackColor = SystemColors.Control;
			}
			this.EditEventDate2.Enabled = ((dates & (TGenEngine.TDateControlsRange)4) > (TGenEngine.TDateControlsRange)0);
			if (this.EditEventDate2.Enabled)
			{
				this.EditEventDate2.BackColor = SystemColors.Window;
			}
			else
			{
				this.EditEventDate2.BackColor = SystemColors.Control;
			}
			this.cbDate1Calendar.Enabled = ((dates & (TGenEngine.TDateControlsRange)2) > (TGenEngine.TDateControlsRange)0);
			if (this.cbDate1Calendar.Enabled)
			{
				this.cbDate1Calendar.BackColor = SystemColors.Window;
			}
			else
			{
				this.cbDate1Calendar.BackColor = SystemColors.Control;
			}
			this.cbDate2Calendar.Enabled = ((dates & (TGenEngine.TDateControlsRange)4) > (TGenEngine.TDateControlsRange)0);
			if (this.cbDate2Calendar.Enabled)
			{
				this.cbDate2Calendar.BackColor = SystemColors.Window;
			}
			else
			{
				this.cbDate2Calendar.BackColor = SystemColors.Control;
			}
		}
		public TfmEventEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			int i = 0;
			do
			{
				this.EditEventDateType.Items.Add(GKL.LSList[(int)TGenEngine.DateKinds[i].Name - 1]);
				i++;
			}
			while (i != 10);
			TGEDCOMDate.TGEDCOMCalendar gc = TGEDCOMDate.TGEDCOMCalendar.dcGregorian;
			do
			{
				this.cbDate1Calendar.Items.Add(GKL.LSList[(int)TGenEngine.DateCalendars[(int)gc] - 1]);
				this.cbDate2Calendar.Items.Add(GKL.LSList[(int)TGenEngine.DateCalendars[(int)gc] - 1]);
				this.cbDate1Calendar.SelectedIndex = 0;
				this.cbDate2Calendar.SelectedIndex = 0;
				gc++;
			}
			while (gc != (TGEDCOMDate.TGEDCOMCalendar)6);
			this.FLocation = null;
			this.FNotesList = new TSheetList(this.SheetNotes);
			this.FNotesList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecNotesList(this.FNotesList);
			this.FMediaList = new TSheetList(this.SheetMultimedia);
			this.FMediaList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecMediaList(this.FMediaList);
			this.FSourcesList = new TSheetList(this.SheetSources);
			this.FSourcesList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecSourcesList(this.FSourcesList);
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.btnAddress.Text = GKL.LSList[82] + "...";
			this.SheetCommon.Text = GKL.LSList[144];
			this.SheetNotes.Text = GKL.LSList[54];
			this.SheetMultimedia.Text = GKL.LSList[55];
			this.SheetSources.Text = GKL.LSList[56];
			this.Label1.Text = GKL.LSList[203];
			this.LabelAttr.Text = GKL.LSList[202];
			this.Label2.Text = GKL.LSList[204];
			this.Label3.Text = GKL.LSList[139];
			this.Label4.Text = GKL.LSList[205];
			this.Label5.Text = GKL.LSList[206];
		}
	}
}
