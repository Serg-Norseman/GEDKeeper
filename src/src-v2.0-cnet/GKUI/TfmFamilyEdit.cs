using GedCom551;
using GKCore;
using GKUI.Controls;
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
	public class TfmFamilyEdit : Form
	{
		private TabControl PagesFamilyData;
		private TabPage SheetEvents;
		private TabPage SheetNotes;
		private TabPage SheetMultimedia;
		private TabPage SheetSources;
		private TabPage SheetChilds;
		private Button btnAccept;
		private Button btnCancel;
		private GroupBox GroupBox1;
		private Label Label1;
		private TextBox EditHusband;
		private Button btnHusbandAdd;
		private Button btnHusbandDelete;
		private Button btnHusbandSel;
		private Button btnWifeSel;
		private Button btnWifeDelete;
		private Button btnWifeAdd;
		private TextBox EditWife;
		private Label Label2;
		private Label Label6;
		private ComboBox EditMarriageStatus;
		private Label Label5;
		private ComboBox cbRestriction;
		private TfmBase FBase;
		private TGEDCOMFamilyRecord FFamily;
		private TSheetList FChildsList;
		private TSheetList FEventsList;
		private TSheetList FNotesList;
		private TSheetList FMediaList;
		private TSheetList FSourcesList;

		[Browsable(false)]
		public TfmBase Base
		{
			get	{ return this.FBase; }
		}

		[Browsable(false)]
		public TGEDCOMFamilyRecord Family
		{
			get	{ return this.FFamily; }
			set	{ this.SetFamily(value); }
		}

		private void AcceptChanges()
		{
			string stat = TGenEngine.MarriageStatus[this.EditMarriageStatus.SelectedIndex].StatSign;
			this.FFamily.SetTagStringValue("_STAT", stat);
			this.FFamily.Restriction = (TGEDCOMObject.TGEDCOMRestriction)this.cbRestriction.SelectedIndex;
			this.FFamily.SortChilds();
			this.Base.ChangeRecord(this.FFamily);
		}

		private TGEDCOMIndividualRecord GetHusband()
		{
			return this.FFamily.Husband.Value as TGEDCOMIndividualRecord;
		}

		private TGEDCOMIndividualRecord GetWife()
		{
			return this.FFamily.Wife.Value as TGEDCOMIndividualRecord;
		}

		private void SetFamily([In] TGEDCOMFamilyRecord Value)
		{
			this.FFamily = Value;
			try
			{
				if (this.FFamily == null)
				{
					this.btnHusbandSel.Enabled = false;
					this.btnWifeSel.Enabled = false;
					this.EditMarriageStatus.Enabled = false;
					this.EditMarriageStatus.SelectedIndex = 0;
					this.cbRestriction.SelectedIndex = 0;
				}
				else
				{
					string stat = this.FFamily.GetTagStringValue("_STAT");
					int stat_idx = TGenEngine.GetMarriageStatusIndex(stat);
					this.EditMarriageStatus.Enabled = true;
					this.EditMarriageStatus.SelectedIndex = stat_idx;
					this.cbRestriction.SelectedIndex = (int)((sbyte)this.FFamily.Restriction);
					this.ControlsRefresh();
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("FamilyEdit.SetFamily(): " + E.Message);
			}
		}
		private void ControlsRefresh()
		{
			TGEDCOMIndividualRecord spouse = this.GetHusband();
			if (spouse != null)
			{
				this.EditHusband.Text = TGenEngine.GetNameStr(spouse, true, false);
			}
			else
			{
				this.EditHusband.Text = GKL.LSList[64];
			}
			this.btnHusbandAdd.Enabled = (spouse == null);
			this.btnHusbandDelete.Enabled = (spouse != null);
			this.btnHusbandSel.Enabled = (spouse != null);
			spouse = this.GetWife();
			if (spouse != null)
			{
				this.EditWife.Text = TGenEngine.GetNameStr(spouse, true, false);
			}
			else
			{
				this.EditWife.Text = GKL.LSList[63];
			}
			this.btnWifeAdd.Enabled = (spouse == null);
			this.btnWifeDelete.Enabled = (spouse != null);
			this.btnWifeSel.Enabled = (spouse != null);
			this.Base.RecListFamilyEventsRefresh(this.FFamily, this.FEventsList.List, null);
			this.Base.RecListNotesRefresh(this.FFamily, this.FNotesList.List, null);
			this.Base.RecListMediaRefresh(this.FFamily, this.FMediaList.List, null);
			this.Base.RecListSourcesRefresh(this.FFamily, this.FSourcesList.List, null);
			TGKListView list = this.FChildsList.List;
			list.BeginUpdate();
			list.Items.Clear();
			int arg_169_0 = 1;
			int num = this.FFamily.ChildrenCount;
			int i = arg_169_0;
			if (num >= i)
			{
				num++;
				do
				{
					TGEDCOMIndividualRecord child = this.FFamily.GetChildren(i - 1).Value as TGEDCOMIndividualRecord;
					ListViewItem item = list.AddItem(i.ToString(), child);
					item.SubItems.Add(TGenEngine.GetNameStr(child, true, false));
					item.SubItems.Add(TGenEngine.GetBirthDate(child, GKL.fmGEDKeeper.Options.DefDateFormat, false));
					i++;
				}
				while (i != num);
			}
			list.EndUpdate();
			this.btnHusbandAdd.Enabled = (this.btnHusbandAdd.Enabled && this.FFamily.Restriction != TGEDCOMObject.TGEDCOMRestriction.rnLocked);
			this.btnHusbandDelete.Enabled = (this.btnHusbandDelete.Enabled && this.FFamily.Restriction != TGEDCOMObject.TGEDCOMRestriction.rnLocked);
			this.btnWifeAdd.Enabled = (this.btnWifeAdd.Enabled && this.FFamily.Restriction != TGEDCOMObject.TGEDCOMRestriction.rnLocked);
			this.btnWifeDelete.Enabled = (this.btnWifeDelete.Enabled && this.FFamily.Restriction != TGEDCOMObject.TGEDCOMRestriction.rnLocked);
			this.EditMarriageStatus.Enabled = (this.EditMarriageStatus.Enabled && this.FFamily.Restriction != TGEDCOMObject.TGEDCOMRestriction.rnLocked);
			this.FChildsList.ReadOnly = (this.FFamily.Restriction == TGEDCOMObject.TGEDCOMRestriction.rnLocked);
			this.FEventsList.ReadOnly = (this.FFamily.Restriction == TGEDCOMObject.TGEDCOMRestriction.rnLocked);
			this.FNotesList.ReadOnly = (this.FFamily.Restriction == TGEDCOMObject.TGEDCOMRestriction.rnLocked);
			this.FMediaList.ReadOnly = (this.FFamily.Restriction == TGEDCOMObject.TGEDCOMRestriction.rnLocked);
			this.FSourcesList.ReadOnly = (this.FFamily.Restriction == TGEDCOMObject.TGEDCOMRestriction.rnLocked);
		}
		private void SetTitle()
		{
			this.Text = string.Concat(new string[]
			{
				GKL.LSList[114], 
				" \"", 
				this.EditHusband.Text, 
				" - ", 
				this.EditWife.Text, 
				"\""
			});
		}
		private void ListModify(object Sender, object ItemData, TGenEngine.TRecAction Action)
		{
			if (object.Equals(Sender, this.FChildsList))
			{
				if (Action != TGenEngine.TRecAction.raAdd)
				{
					if (Action != TGenEngine.TRecAction.raEdit)
					{
						if (Action != TGenEngine.TRecAction.raDelete)
						{
							if (Action == TGenEngine.TRecAction.raJump)
							{
								TGEDCOMIndividualRecord child = ItemData as TGEDCOMIndividualRecord;
								if (child != null)
								{
									this.AcceptChanges();
									this.Base.SelectRecordByXRef(child.XRef);
									base.Close();
								}
							}
						}
						else
						{
							TGEDCOMIndividualRecord child = ItemData as TGEDCOMIndividualRecord;
							if (child != null && TGKSys.ShowQuestion(GKL.LSList[121]) != DialogResult.No && this.Base.Engine.RemoveFamilyChild(this.FFamily, child))
							{
								this.ControlsRefresh();
							}
						}
					}
					else
					{
						TGEDCOMIndividualRecord child = ItemData as TGEDCOMIndividualRecord;
						if (this.Base.ModifyPerson(ref child))
						{
							this.ControlsRefresh();
						}
					}
				}
				else
				{
					TGEDCOMIndividualRecord child = this.Base.SelectPerson(this.GetHusband(), TGenEngine.TTargetMode.tmAncestor, TGEDCOMObject.TGEDCOMSex.svNone);
					if (child != null && this.Base.Engine.AddFamilyChild(this.FFamily, child))
					{
						this.ControlsRefresh();
					}
				}
			}
			else
			{
				if (object.Equals(Sender, this.FEventsList))
				{
					if (this.Base.ModifyRecEvent(this, this.FFamily, ItemData as TGEDCOMCustomEvent, Action))
					{
						this.ControlsRefresh();
					}
				}
				else
				{
					if (object.Equals(Sender, this.FNotesList))
					{
						if (this.Base.ModifyRecNote(this, this.FFamily, ItemData as TGEDCOMNotes, Action))
						{
							this.ControlsRefresh();
						}
					}
					else
					{
						if (object.Equals(Sender, this.FMediaList))
						{
							if (this.Base.ModifyRecMultimedia(this, this.FFamily, ItemData as TGEDCOMMultimediaLink, Action))
							{
								this.ControlsRefresh();
							}
						}
						else
						{
							if (object.Equals(Sender, this.FSourcesList) && this.Base.ModifyRecSource(this, this.FFamily, ItemData as TGEDCOMSourceCitation, Action))
							{
								this.ControlsRefresh();
							}
						}
					}
				}
			}
		}
		private void InitializeComponent()
		{
			this.PagesFamilyData = new TabControl();
			this.SheetChilds = new TabPage();
			this.SheetEvents = new TabPage();
			this.SheetNotes = new TabPage();
			this.SheetMultimedia = new TabPage();
			this.SheetSources = new TabPage();
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.GroupBox1 = new GroupBox();
			this.Label1 = new Label();
			this.btnHusbandAdd = new Button();
			this.btnHusbandDelete = new Button();
			this.btnHusbandSel = new Button();
			this.btnWifeSel = new Button();
			this.btnWifeDelete = new Button();
			this.btnWifeAdd = new Button();
			this.Label2 = new Label();
			this.Label6 = new Label();
			this.EditHusband = new TextBox();
			this.EditWife = new TextBox();
			this.EditMarriageStatus = new ComboBox();
			this.Label5 = new Label();
			this.cbRestriction = new ComboBox();
			this.PagesFamilyData.SuspendLayout();
			this.GroupBox1.SuspendLayout();
			base.SuspendLayout();
			this.PagesFamilyData.Controls.Add(this.SheetChilds);
			this.PagesFamilyData.Controls.Add(this.SheetEvents);
			this.PagesFamilyData.Controls.Add(this.SheetNotes);
			this.PagesFamilyData.Controls.Add(this.SheetMultimedia);
			this.PagesFamilyData.Controls.Add(this.SheetSources);
			this.PagesFamilyData.Location = new Point(0, 129);
			this.PagesFamilyData.Name = "PagesFamilyData";
			this.PagesFamilyData.SelectedIndex = 0;
			this.PagesFamilyData.Size = new Size(505, 264);
			this.PagesFamilyData.TabIndex = 0;
			this.SheetChilds.Location = new Point(4, 22);
			this.SheetChilds.Name = "SheetChilds";
			this.SheetChilds.Size = new Size(497, 238);
			this.SheetChilds.TabIndex = 0;
			this.SheetChilds.Text = "Дети";
			this.SheetEvents.Location = new Point(4, 22);
			this.SheetEvents.Name = "SheetEvents";
			this.SheetEvents.Size = new Size(497, 238);
			this.SheetEvents.TabIndex = 1;
			this.SheetEvents.Text = "События";
			this.SheetNotes.Location = new Point(4, 22);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new Size(497, 238);
			this.SheetNotes.TabIndex = 2;
			this.SheetNotes.Text = "Заметки";
			this.SheetMultimedia.Location = new Point(4, 22);
			this.SheetMultimedia.Name = "SheetMultimedia";
			this.SheetMultimedia.Size = new Size(497, 238);
			this.SheetMultimedia.TabIndex = 3;
			this.SheetMultimedia.Text = "Мультимедиа";
			this.SheetSources.Location = new Point(4, 22);
			this.SheetSources.Name = "SheetSources";
			this.SheetSources.Size = new Size(497, 238);
			this.SheetSources.TabIndex = 4;
			this.SheetSources.Text = "Источники";
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(328, 408);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 1;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(416, 408);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 2;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			this.GroupBox1.Controls.Add(this.Label1);
			this.GroupBox1.Controls.Add(this.btnHusbandAdd);
			this.GroupBox1.Controls.Add(this.btnHusbandDelete);
			this.GroupBox1.Controls.Add(this.btnHusbandSel);
			this.GroupBox1.Controls.Add(this.btnWifeSel);
			this.GroupBox1.Controls.Add(this.btnWifeDelete);
			this.GroupBox1.Controls.Add(this.btnWifeAdd);
			this.GroupBox1.Controls.Add(this.Label2);
			this.GroupBox1.Controls.Add(this.Label6);
			this.GroupBox1.Controls.Add(this.EditHusband);
			this.GroupBox1.Controls.Add(this.EditWife);
			this.GroupBox1.Controls.Add(this.EditMarriageStatus);
			this.GroupBox1.Location = new Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new Size(505, 129);
			this.GroupBox1.TabIndex = 3;
			this.GroupBox1.TabStop = false;
			this.GroupBox1.Text = "Семья";
			this.Label1.Location = new Point(16, 32);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(35, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Муж";
			this.btnHusbandAdd.AccessibleDescription = "Выбрать или добавить мужа";
			this.btnHusbandAdd.Enabled = false;
			this.btnHusbandAdd.Location = new Point(400, 21);
			this.btnHusbandAdd.Name = "btnHusbandAdd";
			this.btnHusbandAdd.Size = new Size(26, 26);
			this.btnHusbandAdd.TabIndex = 1;
			this.btnHusbandAdd.Click += new EventHandler(this.btnHusbandAddClick);
			this.btnHusbandDelete.AccessibleDescription = "Отсоединить мужа";
			this.btnHusbandDelete.Enabled = false;
			this.btnHusbandDelete.Location = new Point(429, 21);
			this.btnHusbandDelete.Name = "btnHusbandDelete";
			this.btnHusbandDelete.Size = new Size(26, 26);
			this.btnHusbandDelete.TabIndex = 2;
			this.btnHusbandDelete.Click += new EventHandler(this.btnHusbandDeleteClick);
			this.btnHusbandSel.AccessibleDescription = "Перейти на запись мужа";
			this.btnHusbandSel.Location = new Point(458, 21);
			this.btnHusbandSel.Name = "btnHusbandSel";
			this.btnHusbandSel.Size = new Size(26, 26);
			this.btnHusbandSel.TabIndex = 3;
			this.btnHusbandSel.Click += new EventHandler(this.btnHusbandSelClick);
			this.btnWifeSel.AccessibleDescription = "Перейти на запись жены";
			this.btnWifeSel.Location = new Point(458, 53);
			this.btnWifeSel.Name = "btnWifeSel";
			this.btnWifeSel.Size = new Size(26, 26);
			this.btnWifeSel.TabIndex = 4;
			this.btnWifeSel.Click += new EventHandler(this.btnWifeSelClick);
			this.btnWifeDelete.AccessibleDescription = "Отсоединить жену";
			this.btnWifeDelete.Enabled = false;
			this.btnWifeDelete.Location = new Point(429, 53);
			this.btnWifeDelete.Name = "btnWifeDelete";
			this.btnWifeDelete.Size = new Size(26, 26);
			this.btnWifeDelete.TabIndex = 5;
			this.btnWifeDelete.Click += new EventHandler(this.btnWifeDeleteClick);
			this.btnWifeAdd.AccessibleDescription = "Выбрать или добавить жену";
			this.btnWifeAdd.Enabled = false;
			this.btnWifeAdd.Location = new Point(400, 53);
			this.btnWifeAdd.Name = "btnWifeAdd";
			this.btnWifeAdd.Size = new Size(26, 26);
			this.btnWifeAdd.TabIndex = 6;
			this.btnWifeAdd.Click += new EventHandler(this.btnWifeAddClick);
			this.Label2.Location = new Point(16, 64);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(35, 13);
			this.Label2.TabIndex = 7;
			this.Label2.Text = "Жена";
			this.Label6.Location = new Point(16, 96);
			this.Label6.Name = "Label6";
			this.Label6.Size = new Size(45, 13);
			this.Label6.TabIndex = 8;
			this.Label6.Text = "Статус";
			this.EditHusband.ForeColor = SystemColors.Control;
			this.EditHusband.Location = new Point(64, 24);
			this.EditHusband.Name = "EditHusband";
			this.EditHusband.ReadOnly = true;
			this.EditHusband.Size = new Size(329, 21);
			this.EditHusband.TabIndex = 0;
			this.EditHusband.Text = "";
			this.EditHusband.TextChanged += new EventHandler(this.EditHusband_TextChanged);
			this.EditWife.ForeColor = SystemColors.Control;
			this.EditWife.Location = new Point(64, 56);
			this.EditWife.Name = "EditWife";
			this.EditWife.ReadOnly = true;
			this.EditWife.Size = new Size(329, 21);
			this.EditWife.TabIndex = 1;
			this.EditWife.Text = "";
			this.EditWife.TextChanged += new EventHandler(this.EditWife_TextChanged);
			this.EditMarriageStatus.DropDownStyle = ComboBoxStyle.DropDownList;
			this.EditMarriageStatus.Location = new Point(64, 88);
			this.EditMarriageStatus.Name = "EditMarriageStatus";
			this.EditMarriageStatus.Size = new Size(145, 21);
			this.EditMarriageStatus.TabIndex = 2;
			this.Label5.Location = new Point(8, 416);
			this.Label5.Name = "Label5";
			this.Label5.Size = new Size(150, 13);
			this.Label5.TabIndex = 4;
			this.Label5.Text = "Ограничение безопасности";
			this.cbRestriction.DropDownStyle = ComboBoxStyle.DropDownList;
			this.cbRestriction.Location = new Point(160, 408);
			this.cbRestriction.Name = "cbRestriction";
			this.cbRestriction.Size = new Size(145, 21);
			this.cbRestriction.TabIndex = 4;
			this.cbRestriction.SelectedIndexChanged += new EventHandler(this.cbRestriction_SelectedIndexChanged);
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(505, 441);
			base.Controls.Add(this.GroupBox1);
			base.Controls.Add(this.PagesFamilyData);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			base.Controls.Add(this.Label5);
			base.Controls.Add(this.cbRestriction);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmFamilyEdit";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Редактирование семьи";
			this.PagesFamilyData.ResumeLayout(false);
			this.GroupBox1.ResumeLayout(false);
			base.ResumeLayout(false);
		}
		private void btnHusbandAddClick(object sender, EventArgs e)
		{
			TGEDCOMIndividualRecord husband = this.Base.SelectPerson(null, TGenEngine.TTargetMode.tmNone, TGEDCOMObject.TGEDCOMSex.svMale);
			if (husband != null && BDSSystem.WStrCmp(this.FFamily.Husband.StringValue, "") == 0)
			{
				this.Base.Engine.AddFamilySpouse(this.FFamily, husband);
				this.ControlsRefresh();
			}
		}
		private void btnHusbandDeleteClick(object sender, EventArgs e)
		{
			if (TGKSys.ShowQuestion(GKL.LSList[119]) != DialogResult.No)
			{
				this.Base.Engine.RemoveFamilySpouse(this.FFamily, this.GetHusband());
				this.ControlsRefresh();
			}
		}
		private void btnHusbandSelClick(object sender, EventArgs e)
		{
			TGEDCOMIndividualRecord spouse = this.GetHusband();
			if (spouse != null)
			{
				this.AcceptChanges();
				this.Base.SelectRecordByXRef(spouse.XRef);
				base.Close();
			}
		}
		private void btnWifeAddClick(object sender, EventArgs e)
		{
			TGEDCOMIndividualRecord wife = this.Base.SelectPerson(null, TGenEngine.TTargetMode.tmNone, TGEDCOMObject.TGEDCOMSex.svFemale);
			if (wife != null && BDSSystem.WStrCmp(this.FFamily.Wife.StringValue, "") == 0)
			{
				this.Base.Engine.AddFamilySpouse(this.FFamily, wife);
				this.ControlsRefresh();
			}
		}
		private void btnWifeDeleteClick(object sender, EventArgs e)
		{
			if (TGKSys.ShowQuestion(GKL.LSList[120]) != DialogResult.No)
			{
				this.Base.Engine.RemoveFamilySpouse(this.FFamily, this.GetWife());
				this.ControlsRefresh();
			}
		}
		private void btnWifeSelClick(object sender, EventArgs e)
		{
			TGEDCOMIndividualRecord spouse = this.GetWife();
			if (spouse != null)
			{
				this.AcceptChanges();
				this.Base.SelectRecordByXRef(spouse.XRef);
				base.Close();
			}
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
		private void EditHusband_TextChanged(object sender, EventArgs e)
		{
			this.SetTitle();
		}
		private void EditWife_TextChanged(object sender, EventArgs e)
		{
			this.SetTitle();
		}
		private void cbRestriction_SelectedIndexChanged(object sender, EventArgs e)
		{
			this.ControlsRefresh();
		}
		public TfmFamilyEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			TGEDCOMObject.TGEDCOMRestriction res = TGEDCOMObject.TGEDCOMRestriction.rnNone;
			do
			{
				this.cbRestriction.Items.Add(TGenEngine.Restrictions[(int)res]);
				res++;
			}
			while (res != (TGEDCOMObject.TGEDCOMRestriction)4);
			int i = 0;
			do
			{
				this.EditMarriageStatus.Items.Add(GKL.LSList[(int)TGenEngine.MarriageStatus[i].Name - 1]);
				i++;
			}
			while (i != 4);
			this.FChildsList = new TSheetList(this.SheetChilds);
			this.FChildsList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.FChildsList.Buttons = TEnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbJump
			});
			this.FChildsList.List.AddListColumn("№", 25, false);
			this.FChildsList.List.AddListColumn(GKL.LSList[85], 300, false);
			this.FChildsList.List.AddListColumn(GKL.LSList[122], 100, false);
			this.FEventsList = new TSheetList(this.SheetEvents);
			this.FEventsList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecEventsList(this.FEventsList, false);
			this.FNotesList = new TSheetList(this.SheetNotes);
			this.FNotesList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecNotesList(this.FNotesList);
			this.FMediaList = new TSheetList(this.SheetMultimedia);
			this.FMediaList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecMediaList(this.FMediaList);
			this.FSourcesList = new TSheetList(this.SheetSources);
			this.FSourcesList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecSourcesList(this.FSourcesList);
			this.SetLang();
		}

		public void SetLang()
		{
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.GroupBox1.Text = GKL.LSList[114];
			this.Label1.Text = GKL.LSList[115];
			this.Label2.Text = GKL.LSList[116];
			this.Label6.Text = GKL.LSList[117];
			this.SheetChilds.Text = GKL.LSList[118];
			this.SheetEvents.Text = GKL.LSList[83];
			this.SheetNotes.Text = GKL.LSList[54];
			this.SheetMultimedia.Text = GKL.LSList[55];
			this.SheetSources.Text = GKL.LSList[56];
			this.Label5.Text = GKL.LSList[124];
		}
	}
}
