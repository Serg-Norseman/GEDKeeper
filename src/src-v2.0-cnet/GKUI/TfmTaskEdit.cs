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
	public class TfmTaskEdit : Form
	{
		private GroupBox GroupBox1;
		private TabControl PagesGroupData;
		private TabPage SheetNotes;
		private Button btnAccept;
		private Button btnCancel;
		private Label Label2;
		private ComboBox EditPriority;
		private Label Label4;
		private MaskedTextBox EditStartDate;
		private MaskedTextBox EditStopDate;
		private Label Label5;
		private Label Label1;
		private ComboBox cbGoalType;
		private TextBox EditGoal;
		private Button btnGoalSelect;
		private TfmBase FBase;
		private TGEDCOMTaskRecord FTask;
		private TGEDCOMRecord FTempRec;
		private TSheetList FNotesList;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		public TGEDCOMTaskRecord Task
		{
			get { return this.FTask; }
			set { this.SetTask(value); }
		}

		private void SetTask([In] TGEDCOMTaskRecord Value)
		{
			this.FTask = Value;
			try
			{
				if (this.FTask == null)
				{
					this.EditPriority.SelectedIndex = -1;
					this.EditStartDate.Text = "";
					this.EditStopDate.Text = "";
					this.cbGoalType.SelectedIndex = 0;
					this.EditGoal.Text = "";
				}
				else
				{
					this.EditPriority.SelectedIndex = (int)((sbyte)this.FTask.Priority);
					this.EditStartDate.Text = TGenEngine.GEDCOMDateToStr(this.FTask.StartDate, TGenEngine.TDateFormat.dfDD_MM_YYYY);
					this.EditStopDate.Text = TGenEngine.GEDCOMDateToStr(this.FTask.StopDate, TGenEngine.TDateFormat.dfDD_MM_YYYY);
					TGoalType gt = TGoalType.gtOther;
					TGenEngine.GetTaskGoal(this.Base.Tree, this.FTask, ref gt, ref this.FTempRec);
					this.cbGoalType.SelectedIndex = (int)((sbyte)gt);
					if (gt != TGoalType.gtIndividual)
					{
						if (gt != TGoalType.gtFamily)
						{
							if (gt != TGoalType.gtSource)
							{
								if (gt == TGoalType.gtOther)
								{
									this.EditGoal.Text = this.FTask.Goal;
								}
							}
							else
							{
								this.EditGoal.Text = ((TGEDCOMSourceRecord)this.FTempRec).FiledByEntry;
							}
						}
						else
						{
							this.EditGoal.Text = TGenEngine.GetFamilyStr((TGEDCOMFamilyRecord)this.FTempRec);
						}
					}
					else
					{
						this.EditGoal.Text = TGenEngine.GetNameStr((TGEDCOMIndividualRecord)this.FTempRec, true, false);
					}
				}
				this.cbGoalType_SelectedIndexChanged(null, null);
				this.ListsRefresh();
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("TaskEdit.SetTask(): " + E.Message);
			}
		}

		private void ListModify(object Sender, object ItemData, TGenEngine.TRecAction Action)
		{
			if (object.Equals(Sender, this.FNotesList) && this.Base.ModifyRecNote(this, this.FTask, ItemData as TGEDCOMNotes, Action))
			{
				this.ListsRefresh();
			}
		}

		private void ListsRefresh()
		{
			this.Base.RecListNotesRefresh(this.FTask, this.FNotesList.List, null);
		}

		private void InitializeComponent()
		{
			this.GroupBox1 = new GroupBox();
			this.Label2 = new Label();
			this.Label4 = new Label();
			this.Label5 = new Label();
			this.Label1 = new Label();
			this.btnGoalSelect = new Button();
			this.EditPriority = new ComboBox();
			this.EditStartDate = new MaskedTextBox();
			this.EditStopDate = new MaskedTextBox();
			this.cbGoalType = new ComboBox();
			this.EditGoal = new TextBox();
			this.PagesGroupData = new TabControl();
			this.SheetNotes = new TabPage();
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.GroupBox1.SuspendLayout();
			this.PagesGroupData.SuspendLayout();
			base.SuspendLayout();
			this.GroupBox1.Controls.Add(this.Label2);
			this.GroupBox1.Controls.Add(this.Label4);
			this.GroupBox1.Controls.Add(this.Label5);
			this.GroupBox1.Controls.Add(this.Label1);
			this.GroupBox1.Controls.Add(this.btnGoalSelect);
			this.GroupBox1.Controls.Add(this.EditPriority);
			this.GroupBox1.Controls.Add(this.EditStartDate);
			this.GroupBox1.Controls.Add(this.EditStopDate);
			this.GroupBox1.Controls.Add(this.cbGoalType);
			this.GroupBox1.Controls.Add(this.EditGoal);
			this.GroupBox1.Location = new Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new Size(481, 97);
			this.GroupBox1.TabIndex = 1;
			this.GroupBox1.TabStop = false;
			this.Label2.Location = new Point(8, 48);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(60, 13);
			this.Label2.TabIndex = 0;
			this.Label2.Text = "Приоритет";
			this.Label4.Location = new Point(8, 72);
			this.Label4.Name = "Label4";
			this.Label4.Size = new Size(60, 13);
			this.Label4.TabIndex = 1;
			this.Label4.Text = "Запущено";
			this.Label5.Location = new Point(248, 72);
			this.Label5.Name = "Label5";
			this.Label5.Size = new Size(64, 13);
			this.Label5.TabIndex = 2;
			this.Label5.Text = "Завершено";
			this.Label1.Location = new Point(8, 24);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(30, 13);
			this.Label1.TabIndex = 3;
			this.Label1.Text = "Цель";
			this.btnGoalSelect.AccessibleDescription = "Выбрать запись цели";
			this.btnGoalSelect.Location = new Point(448, 14);
			this.btnGoalSelect.Name = "btnGoalSelect";
			this.btnGoalSelect.Size = new Size(26, 26);
			this.btnGoalSelect.TabIndex = 4;
			this.btnGoalSelect.Click += new EventHandler(this.btnGoalSelect_Click);
			this.EditPriority.DropDownStyle = ComboBoxStyle.DropDownList;
			this.EditPriority.Location = new Point(72, 40);
			this.EditPriority.Name = "EditPriority";
			this.EditPriority.Size = new Size(161, 21);
			this.EditPriority.TabIndex = 2;
			this.EditStartDate.Location = new Point(72, 64);
			this.EditStartDate.Mask = "00/00/0000";
			this.EditStartDate.MaxLength = 10;
			this.EditStartDate.Name = "EditStartDate";
			this.EditStartDate.Size = new Size(161, 21);
			this.EditStartDate.TabIndex = 3;
			this.EditStartDate.Text = "  .  .    ";
			this.EditStopDate.Location = new Point(312, 64);
			this.EditStopDate.Mask = "00/00/0000";
			this.EditStopDate.MaxLength = 10;
			this.EditStopDate.Name = "EditStopDate";
			this.EditStopDate.Size = new Size(161, 21);
			this.EditStopDate.TabIndex = 4;
			this.EditStopDate.Text = "  .  .    ";
			this.cbGoalType.DropDownStyle = ComboBoxStyle.DropDownList;
			this.cbGoalType.Location = new Point(72, 16);
			this.cbGoalType.Name = "cbGoalType";
			this.cbGoalType.Size = new Size(113, 21);
			this.cbGoalType.TabIndex = 0;
			this.cbGoalType.SelectedIndexChanged += new EventHandler(this.cbGoalType_SelectedIndexChanged);
			this.EditGoal.Location = new Point(192, 16);
			this.EditGoal.Name = "EditGoal";
			this.EditGoal.ReadOnly = true;
			this.EditGoal.Size = new Size(249, 21);
			this.EditGoal.TabIndex = 1;
			this.EditGoal.Text = "";
			this.PagesGroupData.Controls.Add(this.SheetNotes);
			this.PagesGroupData.Location = new Point(0, 97);
			this.PagesGroupData.Name = "PagesGroupData";
			this.PagesGroupData.SelectedIndex = 0;
			this.PagesGroupData.Size = new Size(481, 256);
			this.PagesGroupData.TabIndex = 0;
			this.SheetNotes.Location = new Point(4, 22);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new Size(473, 230);
			this.SheetNotes.TabIndex = 0;
			this.SheetNotes.Text = "Заметки";
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(304, 368);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 2;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(392, 368);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(481, 402);
			base.Controls.Add(this.GroupBox1);
			base.Controls.Add(this.PagesGroupData);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmTaskEdit";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Редактирование задачи";
			this.GroupBox1.ResumeLayout(false);
			this.PagesGroupData.ResumeLayout(false);
			base.ResumeLayout(false);
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			this.FTask.Priority = (TResearchPriority)this.EditPriority.SelectedIndex;
			this.FTask.StartDate.ParseString(TGenEngine.StrToGEDCOMDate(this.EditStartDate.Text, true));
			this.FTask.StopDate.ParseString(TGenEngine.StrToGEDCOMDate(this.EditStopDate.Text, true));
			TGoalType gt = (TGoalType)this.cbGoalType.SelectedIndex;
			if (gt >= TGoalType.gtOther)
			{
				if (gt == TGoalType.gtOther)
				{
					this.FTask.Goal = this.EditGoal.Text;
				}
			}
			else
			{
				this.FTask.Goal = TGEDCOMObject.EncloseXRef(this.FTempRec.XRef);
			}
			this.Base.ChangeRecord(this.FTask);
			base.DialogResult = DialogResult.OK;
		}

		private void btnGoalSelect_Click(object sender, EventArgs e)
		{
			TGoalType tGoalType = (TGoalType)this.cbGoalType.SelectedIndex;
			if (tGoalType != TGoalType.gtIndividual)
			{
				if (tGoalType != TGoalType.gtFamily)
				{
					if (tGoalType == TGoalType.gtSource)
					{
						TfmBase arg_A1_0 = this.Base;
						TGEDCOMRecord.TGEDCOMRecordType arg_A1_1 = TGEDCOMRecord.TGEDCOMRecordType.rtSource;
						object[] anArgs = new object[0];
						this.FTempRec = arg_A1_0.SelectRecord(arg_A1_1, anArgs);
						this.EditGoal.Text = ((TGEDCOMSourceRecord)this.FTempRec).FiledByEntry;
					}
				}
				else
				{
					TfmBase arg_65_0 = this.Base;
					TGEDCOMRecord.TGEDCOMRecordType arg_65_1 = TGEDCOMRecord.TGEDCOMRecordType.rtFamily;
					object[] anArgs2 = new object[0];
					this.FTempRec = arg_65_0.SelectRecord(arg_65_1, anArgs2);
					this.EditGoal.Text = TGenEngine.GetFamilyStr((TGEDCOMFamilyRecord)this.FTempRec);
				}
			}
			else
			{
				this.FTempRec = this.Base.SelectPerson(null, TGenEngine.TTargetMode.tmNone, TGEDCOMObject.TGEDCOMSex.svNone);
				this.EditGoal.Text = TGenEngine.GetNameStr((TGEDCOMIndividualRecord)this.FTempRec, true, false);
			}
		}

		private void cbGoalType_SelectedIndexChanged(object sender, EventArgs e)
		{
			TGoalType tGoalType = (TGoalType)this.cbGoalType.SelectedIndex;
			if (tGoalType != TGoalType.gtIndividual)
			{
				if (tGoalType != TGoalType.gtFamily)
				{
					if (tGoalType != TGoalType.gtSource)
					{
						if (tGoalType == TGoalType.gtOther)
						{
							this.btnGoalSelect.Enabled = false;
							this.EditGoal.ForeColor = SystemColors.Window;
							this.EditGoal.ReadOnly = false;
						}
					}
					else
					{
						this.btnGoalSelect.Enabled = true;
						this.EditGoal.ForeColor = SystemColors.Control;
						this.EditGoal.ReadOnly = true;
					}
				}
				else
				{
					this.btnGoalSelect.Enabled = true;
					this.EditGoal.ForeColor = SystemColors.Control;
					this.EditGoal.ReadOnly = true;
				}
			}
			else
			{
				this.btnGoalSelect.Enabled = true;
				this.EditGoal.ForeColor = SystemColors.Control;
				this.EditGoal.ReadOnly = true;
			}
		}

		public TfmTaskEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;

			for (TResearchPriority rp = TResearchPriority.rpNone; rp <= TResearchPriority.rpTop; rp++)
			{
				this.EditPriority.Items.Add(GKL.LSList[(int)TGenEngine.PriorityNames[(int)rp] - 1]);
			}

			for (TGoalType gt = TGoalType.gtIndividual; gt <= TGoalType.gtOther; gt++)
			{
				this.cbGoalType.Items.Add(GKL.LSList[(int)TGenEngine.GoalNames[(int)gt] - 1]);
			}

			this.FNotesList = new TSheetList(this.SheetNotes);
			this.FNotesList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecNotesList(this.FNotesList);
			this.FTempRec = null;
			this.Text = GKL.LSList[190];
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.SheetNotes.Text = GKL.LSList[54];
			this.Label1.Text = GKL.LSList[182];
			this.Label2.Text = GKL.LSList[178];
			this.Label4.Text = GKL.LSList[180];
			this.Label5.Text = GKL.LSList[181];
		}
	}
}
