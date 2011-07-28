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
	public class TfmResearchEdit : Form
	{
		private GroupBox GroupBox1;
		private TextBox EditName;
		private Label Label1;
		private TabControl PagesGroupData;
		private TabPage SheetNotes;
		private TabPage SheetTasks;
		private Button btnAccept;
		private Button btnCancel;
		private Label Label2;
		private ComboBox EditPriority;
		private TabPage SheetCommunications;
		private Label Label3;
		private ComboBox EditStatus;
		private Label Label4;
		private MaskedTextBox EditStartDate;
		private Label Label5;
		private MaskedTextBox EditStopDate;
		private Label Label6;
		private NumericUpDown EditPercent;
		private TabPage SheetGroups;
		private TfmBase FBase;
		private TGEDCOMResearchRecord FResearch;
		private TSheetList FTasksList;
		private TSheetList FCommunicationsList;
		private TSheetList FGroupsList;
		private TSheetList FNotesList;

		[Browsable(false)]
		public TfmBase Base
		{
			get
			{
				return this.FBase;
			}
		}
		[Browsable(false)]
		public TGEDCOMResearchRecord Research
		{
			get
			{
				return this.FResearch;
			}
			set
			{
				this.SetResearch(value);
			}
		}
		private void AcceptChanges()
		{
			this.FResearch.Name = this.EditName.Text;
			this.FResearch.Priority = (TResearchPriority)this.EditPriority.SelectedIndex;
			this.FResearch.Status = (TResearchStatus)this.EditStatus.SelectedIndex;
			this.FResearch.StartDate.ParseString(TGenEngine.StrToGEDCOMDate(this.EditStartDate.Text, true));
			this.FResearch.StopDate.ParseString(TGenEngine.StrToGEDCOMDate(this.EditStopDate.Text, true));
			this.FResearch.Percent = int.Parse(this.EditPercent.Text);
			this.Base.ChangeRecord(this.FResearch);
		}
		private void SetResearch([In] TGEDCOMResearchRecord Value)
		{
			this.FResearch = Value;
			try
			{
				if (this.FResearch == null)
				{
					this.EditName.Text = "";
					this.EditPriority.SelectedIndex = -1;
					this.EditStatus.SelectedIndex = -1;
					this.EditStartDate.Text = "";
					this.EditStopDate.Text = "";
					this.EditPercent.Text = "0";
				}
				else
				{
					this.EditName.Text = this.FResearch.Name;
					this.EditPriority.SelectedIndex = (int)((sbyte)this.FResearch.Priority);
					this.EditStatus.SelectedIndex = (int)((sbyte)this.FResearch.Status);
					this.EditStartDate.Text = TGenEngine.GEDCOMDateToStr(this.FResearch.StartDate, TGenEngine.TDateFormat.dfDD_MM_YYYY);
					this.EditStopDate.Text = TGenEngine.GEDCOMDateToStr(this.FResearch.StopDate, TGenEngine.TDateFormat.dfDD_MM_YYYY);
					this.EditPercent.Text = this.FResearch.Percent.ToString();
				}
				this.ListsRefresh();
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("ResearchEdit.SetResearch(): " + E.Message);
			}
		}
		private void ListModify(object Sender, object ItemData, TGenEngine.TRecAction Action)
		{
			if (object.Equals(Sender, this.FNotesList))
			{
				if (this.Base.ModifyRecNote(this, this.FResearch, ItemData as TGEDCOMNotes, Action))
				{
					this.ListsRefresh();
				}
			}
			else
			{
				if (object.Equals(Sender, this.FTasksList))
				{
					if (Action != TGenEngine.TRecAction.raAdd)
					{
						if (Action != TGenEngine.TRecAction.raEdit)
						{
							if (Action != TGenEngine.TRecAction.raDelete)
							{
								if (Action == TGenEngine.TRecAction.raJump)
								{
									TGEDCOMTaskRecord task = ItemData as TGEDCOMTaskRecord;
									if (task != null)
									{
										this.AcceptChanges();
										this.Base.SelectRecordByXRef(task.XRef);
										base.Close();
									}
								}
							}
							else
							{
								TGEDCOMTaskRecord task = ItemData as TGEDCOMTaskRecord;
								if (task != null && TGKSys.ShowQuestion(GKL.LSList[186]) != DialogResult.No)
								{
									this.Base.Engine.RemoveResearchTask(this.FResearch, task);
									this.ListsRefresh();
								}
							}
						}
						else
						{
							TGEDCOMTaskRecord task = ItemData as TGEDCOMTaskRecord;
							if (task != null && this.Base.ModifyTask(ref task))
							{
								this.ListsRefresh();
							}
						}
					}
					else
					{
						TfmBase arg_6F_0 = this.Base;
						TGEDCOMRecord.TGEDCOMRecordType arg_6F_1 = TGEDCOMRecord.TGEDCOMRecordType.rtTask;
						object[] anArgs = new object[0];
						TGEDCOMTaskRecord task = arg_6F_0.SelectRecord(arg_6F_1, anArgs) as TGEDCOMTaskRecord;
						if (this.Base.Engine.AddResearchTask(this.FResearch, task))
						{
							this.ListsRefresh();
						}
					}
				}
				else
				{
					if (object.Equals(Sender, this.FCommunicationsList))
					{
						if (Action != TGenEngine.TRecAction.raAdd)
						{
							if (Action != TGenEngine.TRecAction.raEdit)
							{
								if (Action != TGenEngine.TRecAction.raDelete)
								{
									if (Action == TGenEngine.TRecAction.raJump)
									{
										TGEDCOMCommunicationRecord comm = ItemData as TGEDCOMCommunicationRecord;
										if (comm != null)
										{
											this.AcceptChanges();
											this.Base.SelectRecordByXRef(comm.XRef);
											base.Close();
										}
									}
								}
								else
								{
									TGEDCOMCommunicationRecord comm = ItemData as TGEDCOMCommunicationRecord;
									if (comm != null && TGKSys.ShowQuestion(GKL.LSList[187]) != DialogResult.No)
									{
										this.Base.Engine.RemoveResearchComm(this.FResearch, comm);
										this.ListsRefresh();
									}
								}
							}
							else
							{
								TGEDCOMCommunicationRecord comm = ItemData as TGEDCOMCommunicationRecord;
								if (comm != null && this.Base.ModifyCommunication(ref comm))
								{
									this.ListsRefresh();
								}
							}
						}
						else
						{
							TfmBase arg_17E_0 = this.Base;
							TGEDCOMRecord.TGEDCOMRecordType arg_17E_1 = TGEDCOMRecord.TGEDCOMRecordType.rtCommunication;
							object[] anArgs2 = new object[0];
							TGEDCOMCommunicationRecord comm = arg_17E_0.SelectRecord(arg_17E_1, anArgs2) as TGEDCOMCommunicationRecord;
							if (this.Base.Engine.AddResearchComm(this.FResearch, comm))
							{
								this.ListsRefresh();
							}
						}
					}
					else
					{
						if (object.Equals(Sender, this.FGroupsList))
						{
							if (Action != TGenEngine.TRecAction.raAdd)
							{
								if (Action != TGenEngine.TRecAction.raDelete)
								{
									if (Action == TGenEngine.TRecAction.raJump)
									{
										TGEDCOMGroupRecord group = ItemData as TGEDCOMGroupRecord;
										if (group != null)
										{
											this.AcceptChanges();
											this.Base.SelectRecordByXRef(group.XRef);
											base.Close();
										}
									}
								}
								else
								{
									TGEDCOMGroupRecord group = ItemData as TGEDCOMGroupRecord;
									if (group != null && TGKSys.ShowQuestion(GKL.LSList[188]) != DialogResult.No)
									{
										this.Base.Engine.RemoveResearchGroup(this.FResearch, group);
										this.ListsRefresh();
									}
								}
							}
							else
							{
								TfmBase arg_288_0 = this.Base;
								TGEDCOMRecord.TGEDCOMRecordType arg_288_1 = TGEDCOMRecord.TGEDCOMRecordType.rtGroup;
								object[] anArgs3 = new object[0];
								TGEDCOMGroupRecord group = arg_288_0.SelectRecord(arg_288_1, anArgs3) as TGEDCOMGroupRecord;
								if (this.Base.Engine.AddResearchGroup(this.FResearch, group))
								{
									this.ListsRefresh();
								}
							}
						}
					}
				}
			}
		}
		private void ListsRefresh()
		{
			this.Base.RecListNotesRefresh(this.FResearch, this.FNotesList.List, null);
			TGKListView list = this.FTasksList.List;
			list.BeginUpdate();
			list.Items.Clear();
			int arg_4D_0 = 0;
			int num = this.FResearch.TasksCount - 1;
			int i = arg_4D_0;
			if (num >= i)
			{
				num++;
				do
				{
					TGEDCOMTaskRecord task = this.FResearch.GetTask(i).Value as TGEDCOMTaskRecord;
					TExtListItem item = list.AddItem(TGenEngine.GetTaskGoalStr(this.Base.Tree, task), task);
					item.SubItems.Add(GKL.LSList[(int)TGenEngine.PriorityNames[(int)task.Priority] - 1]);
					item.SubItems.Add(TGenEngine.GEDCOMDateToStr(task.StartDate, GKL.fmGEDKeeper.Options.DefDateFormat));
					item.SubItems.Add(TGenEngine.GEDCOMDateToStr(task.StopDate, GKL.fmGEDKeeper.Options.DefDateFormat));
					i++;
				}
				while (i != num);
			}
			list.EndUpdate();
			TGKListView list2 = this.FCommunicationsList.List;
			list2.BeginUpdate();
			list2.Items.Clear();
			int arg_14D_0 = 0;
			int num2 = this.FResearch.CommunicationsCount - 1;
			i = arg_14D_0;
			if (num2 >= i)
			{
				num2++;
				do
				{
					TGEDCOMCommunicationRecord corr = this.FResearch.GetCommunication(i).Value as TGEDCOMCommunicationRecord;
					TExtListItem item = list2.AddItem(corr.Name, corr);
					item.SubItems.Add(TGenEngine.GetCorresponderStr(this.Base.Tree, corr, false));
					item.SubItems.Add(GKL.LSList[(int)TGenEngine.CommunicationNames[(int)corr.CommunicationType] - 1]);
					item.SubItems.Add(TGenEngine.GEDCOMDateToStr(corr.Date, GKL.fmGEDKeeper.Options.DefDateFormat));
					i++;
				}
				while (i != num2);
			}
			list2.EndUpdate();
			TGKListView list3 = this.FGroupsList.List;
			list3.BeginUpdate();
			list3.Items.Clear();
			int arg_235_0 = 0;
			int num3 = this.FResearch.GroupsCount - 1;
			i = arg_235_0;
			if (num3 >= i)
			{
				num3++;
				do
				{
					TGEDCOMGroupRecord grp = this.FResearch.GetGroup(i).Value as TGEDCOMGroupRecord;
					TExtListItem item = list3.AddItem(grp.Name, grp);
					i++;
				}
				while (i != num3);
			}
			list3.EndUpdate();
		}
		private void InitializeComponent()
		{
			this.GroupBox1 = new GroupBox();
			this.Label1 = new Label();
			this.Label2 = new Label();
			this.Label3 = new Label();
			this.Label4 = new Label();
			this.Label5 = new Label();
			this.Label6 = new Label();
			this.EditName = new TextBox();
			this.EditPriority = new ComboBox();
			this.EditStatus = new ComboBox();
			this.EditStartDate = new MaskedTextBox();
			this.EditStopDate = new MaskedTextBox();
			this.EditPercent = new NumericUpDown();
			this.PagesGroupData = new TabControl();
			this.SheetTasks = new TabPage();
			this.SheetCommunications = new TabPage();
			this.SheetGroups = new TabPage();
			this.SheetNotes = new TabPage();
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.GroupBox1.SuspendLayout();
			((ISupportInitialize)this.EditPercent).BeginInit();
			this.PagesGroupData.SuspendLayout();
			base.SuspendLayout();
			this.GroupBox1.Controls.Add(this.Label1);
			this.GroupBox1.Controls.Add(this.Label2);
			this.GroupBox1.Controls.Add(this.Label3);
			this.GroupBox1.Controls.Add(this.Label4);
			this.GroupBox1.Controls.Add(this.Label5);
			this.GroupBox1.Controls.Add(this.Label6);
			this.GroupBox1.Controls.Add(this.EditName);
			this.GroupBox1.Controls.Add(this.EditPriority);
			this.GroupBox1.Controls.Add(this.EditStatus);
			this.GroupBox1.Controls.Add(this.EditStartDate);
			this.GroupBox1.Controls.Add(this.EditStopDate);
			this.GroupBox1.Controls.Add(this.EditPercent);
			this.GroupBox1.Location = new Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new Size(609, 97);
			this.GroupBox1.TabIndex = 1;
			this.GroupBox1.TabStop = false;
			this.Label1.Location = new Point(8, 24);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(60, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Название";
			this.Label2.Location = new Point(8, 48);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(60, 13);
			this.Label2.TabIndex = 1;
			this.Label2.Text = "Приоритет";
			this.Label3.Location = new Point(248, 48);
			this.Label3.Name = "Label3";
			this.Label3.Size = new Size(60, 13);
			this.Label3.TabIndex = 2;
			this.Label3.Text = "Состояние";
			this.Label4.Location = new Point(8, 72);
			this.Label4.Name = "Label4";
			this.Label4.Size = new Size(60, 13);
			this.Label4.TabIndex = 3;
			this.Label4.Text = "Запущено";
			this.Label5.Location = new Point(248, 72);
			this.Label5.Name = "Label5";
			this.Label5.Size = new Size(64, 13);
			this.Label5.TabIndex = 4;
			this.Label5.Text = "Завершено";
			this.Label6.Location = new Point(488, 48);
			this.Label6.Name = "Label6";
			this.Label6.Size = new Size(50, 13);
			this.Label6.TabIndex = 5;
			this.Label6.Text = "Процент";
			this.EditName.Location = new Point(72, 16);
			this.EditName.Name = "EditName";
			this.EditName.Size = new Size(529, 21);
			this.EditName.TabIndex = 0;
			this.EditName.Text = "";
			this.EditPriority.DropDownStyle = ComboBoxStyle.DropDownList;
			this.EditPriority.Location = new Point(72, 40);
			this.EditPriority.Name = "EditPriority";
			this.EditPriority.Size = new Size(161, 21);
			this.EditPriority.TabIndex = 1;
			this.EditStatus.DropDownStyle = ComboBoxStyle.DropDownList;
			this.EditStatus.Location = new Point(312, 40);
			this.EditStatus.Name = "EditStatus";
			this.EditStatus.Size = new Size(161, 21);
			this.EditStatus.TabIndex = 2;
			this.EditStartDate.Location = new Point(72, 64);
			this.EditStartDate.Mask = "00/00/0000";
			this.EditStartDate.MaxLength = 10;
			this.EditStartDate.Name = "EditStartDate";
			this.EditStartDate.Size = new Size(161, 21);
			this.EditStartDate.TabIndex = 5;
			this.EditStartDate.Text = "  .  .    ";
			this.EditStopDate.Location = new Point(312, 64);
			this.EditStopDate.Mask = "00/00/0000";
			this.EditStopDate.MaxLength = 10;
			this.EditStopDate.Name = "EditStopDate";
			this.EditStopDate.Size = new Size(161, 21);
			this.EditStopDate.TabIndex = 6;
			this.EditStopDate.Text = "  .  .    ";
			NumericUpDown arg_666_0 = this.EditPercent;
			int[] array = null;
			int[] array2 = array;
			int[] array3;
			int[] expr_635 = array3 = new int[4];
			if (array2 != null)
			{
				int num;
				if ((num = array2.Length) > 4)
				{
					num = 4;
				}
				if (num > 0)
				{
					Array.Copy(array2, array3, num);
				}
			}
			array = expr_635;
			array[0] = 5;
			array[1] = 0;
			array[2] = 0;
			array[3] = 0;
			arg_666_0.Increment = new decimal(array);
			this.EditPercent.Location = new Point(544, 40);
			this.EditPercent.Name = "EditPercent";
			this.EditPercent.Size = new Size(41, 21);
			this.EditPercent.TabIndex = 3;
			this.PagesGroupData.Controls.Add(this.SheetTasks);
			this.PagesGroupData.Controls.Add(this.SheetCommunications);
			this.PagesGroupData.Controls.Add(this.SheetGroups);
			this.PagesGroupData.Controls.Add(this.SheetNotes);
			this.PagesGroupData.Location = new Point(0, 97);
			this.PagesGroupData.Name = "PagesGroupData";
			this.PagesGroupData.SelectedIndex = 0;
			this.PagesGroupData.Size = new Size(609, 312);
			this.PagesGroupData.TabIndex = 0;
			this.SheetTasks.Location = new Point(4, 22);
			this.SheetTasks.Name = "SheetTasks";
			this.SheetTasks.Size = new Size(601, 286);
			this.SheetTasks.TabIndex = 0;
			this.SheetTasks.Text = "Задачи";
			this.SheetCommunications.Location = new Point(4, 22);
			this.SheetCommunications.Name = "SheetCommunications";
			this.SheetCommunications.Size = new Size(601, 286);
			this.SheetCommunications.TabIndex = 1;
			this.SheetCommunications.Text = "Коммуникации";
			this.SheetGroups.Location = new Point(4, 22);
			this.SheetGroups.Name = "SheetGroups";
			this.SheetGroups.Size = new Size(601, 286);
			this.SheetGroups.TabIndex = 2;
			this.SheetGroups.Text = "Группы";
			this.SheetNotes.Location = new Point(4, 22);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new Size(601, 286);
			this.SheetNotes.TabIndex = 3;
			this.SheetNotes.Text = "Заметки";
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(432, 424);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 2;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(520, 424);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(609, 457);
			base.Controls.Add(this.GroupBox1);
			base.Controls.Add(this.PagesGroupData);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmResearchEdit";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Редактирование исследования";
			this.GroupBox1.ResumeLayout(false);
			((ISupportInitialize)this.EditPercent).EndInit();
			this.PagesGroupData.ResumeLayout(false);
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
		public TfmResearchEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;

			for (TResearchPriority rp = TResearchPriority.rpNone; rp <= TResearchPriority.rpTop; rp++)
			{
				this.EditPriority.Items.Add(GKL.LSList[(int)TGenEngine.PriorityNames[(int)rp] - 1]);
			}

			for (TResearchStatus rs = TResearchStatus.rsDefined; rs <= TResearchStatus.rsWithdrawn; rs++)
			{
				this.EditStatus.Items.Add(GKL.LSList[(int)TGenEngine.StatusNames[(int)rs] - 1]);
			}

			this.FTasksList = new TSheetList(this.SheetTasks);
			this.FTasksList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.FTasksList.Buttons = TEnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbJump
			});
			this.FTasksList.List.AddListColumn(GKL.LSList[182], 250, false);
			this.FTasksList.List.AddListColumn(GKL.LSList[178], 90, false);
			this.FTasksList.List.AddListColumn(GKL.LSList[180], 90, false);
			this.FTasksList.List.AddListColumn(GKL.LSList[181], 90, false);
			this.FCommunicationsList = new TSheetList(this.SheetCommunications);
			this.FCommunicationsList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.FCommunicationsList.Buttons = TEnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbJump
			});
			this.FCommunicationsList.List.AddListColumn(GKL.LSList[183], 150, false);
			this.FCommunicationsList.List.AddListColumn(GKL.LSList[184], 150, false);
			this.FCommunicationsList.List.AddListColumn(GKL.LSList[113], 90, false);
			this.FCommunicationsList.List.AddListColumn(GKL.LSList[139], 90, false);
			this.FGroupsList = new TSheetList(this.SheetGroups);
			this.FGroupsList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.FGroupsList.Buttons = TEnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbJump
			});
			this.FGroupsList.List.AddListColumn(GKL.LSList[185], 350, false);
			this.FNotesList = new TSheetList(this.SheetNotes);
			this.FNotesList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecNotesList(this.FNotesList);
			this.SetLang();
		}
		public void SetLang()
		{
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.Text = GKL.LSList[189];
			this.SheetTasks.Text = GKL.LSList[60];
			this.SheetCommunications.Text = GKL.LSList[61];
			this.SheetGroups.Text = GKL.LSList[58];
			this.SheetNotes.Text = GKL.LSList[54];
			this.Label1.Text = GKL.LSList[125];
			this.Label2.Text = GKL.LSList[178];
			this.Label3.Text = GKL.LSList[117];
			this.Label6.Text = GKL.LSList[179];
			this.Label4.Text = GKL.LSList[180];
			this.Label5.Text = GKL.LSList[181];
		}
	}
}
