using System;
using System.Drawing;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Lists;

namespace GKUI
{
	public partial class TfmTaskEdit : Form
	{
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
				SysUtils.LogWrite("TaskEdit.SetTask(): " + E.Message);
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

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
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
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmTaskEdit.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
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
						TGEDCOMRecordType arg_A1_1 = TGEDCOMRecordType.rtSource;
						object[] anArgs = new object[0];
						this.FTempRec = arg_A1_0.SelectRecord(arg_A1_1, anArgs);
						this.EditGoal.Text = ((TGEDCOMSourceRecord)this.FTempRec).FiledByEntry;
					}
				}
				else
				{
					TfmBase arg_65_0 = this.Base;
					TGEDCOMRecordType arg_65_1 = TGEDCOMRecordType.rtFamily;
					object[] anArgs2 = new object[0];
					this.FTempRec = arg_65_0.SelectRecord(arg_65_1, anArgs2);
					this.EditGoal.Text = TGenEngine.GetFamilyStr((TGEDCOMFamilyRecord)this.FTempRec);
				}
			}
			else
			{
				this.FTempRec = this.Base.SelectPerson(null, TGenEngine.TTargetMode.tmNone, TGEDCOMSex.svNone);
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
			this.FNotesList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
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
