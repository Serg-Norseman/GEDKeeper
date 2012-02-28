using System;
using System.Drawing;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKSys;
using GKUI.Lists;

/// <summary>
/// Localization: clean
/// </summary>

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
					string st;

					switch (gt) {
						case TGoalType.gtIndividual:
							st = ((this.FTempRec == null) ? "" : (this.FTempRec as TGEDCOMIndividualRecord).aux_GetNameStr(true, false));
							this.EditGoal.Text = st;
							break;
						case TGoalType.gtFamily:
							this.EditGoal.Text = TGenEngine.aux_GetFamilyStr((TGEDCOMFamilyRecord)this.FTempRec);
							break;
						case TGoalType.gtSource:
							this.EditGoal.Text = ((TGEDCOMSourceRecord)this.FTempRec).FiledByEntry;
							break;
						case TGoalType.gtOther:
							this.EditGoal.Text = this.FTask.Goal;
							break;
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
			if ((Sender == this.FNotesList) && this.Base.ModifyRecNote(this, this.FTask, ItemData as TGEDCOMNotes, Action))
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
				switch (gt) {
					case TGoalType.gtIndividual:
					case TGoalType.gtFamily:
					case TGoalType.gtSource:
						this.FTask.Goal = TGEDCOMObject.EncloseXRef(this.FTempRec.XRef);
						break;
					case TGoalType.gtOther:
						this.FTask.Goal = this.EditGoal.Text;
						break;
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
			TGoalType gt = (TGoalType)this.cbGoalType.SelectedIndex;
			switch (gt) {
				case TGoalType.gtIndividual:
					this.FTempRec = this.Base.SelectPerson(null, TGenEngine.TTargetMode.tmNone, TGEDCOMSex.svNone);
					this.EditGoal.Text = ((this.FTempRec == null) ? "" : (this.FTempRec as TGEDCOMIndividualRecord).aux_GetNameStr(true, false));
					break;
				case TGoalType.gtFamily:
					this.FTempRec = this.Base.SelectRecord(TGEDCOMRecordType.rtFamily, new object[0]);
					this.EditGoal.Text = TGenEngine.aux_GetFamilyStr((TGEDCOMFamilyRecord)this.FTempRec);
					break;
				case TGoalType.gtSource:
					this.FTempRec = this.Base.SelectRecord(TGEDCOMRecordType.rtSource, new object[0]);
					this.EditGoal.Text = ((TGEDCOMSourceRecord)this.FTempRec).FiledByEntry;
					break;
				case TGoalType.gtOther:
					break;
			}
		}

		private void cbGoalType_SelectedIndexChanged(object sender, EventArgs e)
		{
			TGoalType gt = (TGoalType)this.cbGoalType.SelectedIndex;
			switch (gt) {
				case TGoalType.gtIndividual:
					this.btnGoalSelect.Enabled = true;
					this.EditGoal.BackColor = SystemColors.Control;
					this.EditGoal.ReadOnly = true;
					break;
				case TGoalType.gtFamily:
					this.btnGoalSelect.Enabled = true;
					this.EditGoal.BackColor = SystemColors.Control;
					this.EditGoal.ReadOnly = true;
					break;
				case TGoalType.gtSource:
					this.btnGoalSelect.Enabled = true;
					this.EditGoal.BackColor = SystemColors.Control;
					this.EditGoal.ReadOnly = true;
					break;
				case TGoalType.gtOther:
					this.btnGoalSelect.Enabled = false;
					this.EditGoal.BackColor = SystemColors.Window;
					this.EditGoal.ReadOnly = false;
					break;
			}
		}

		public TfmTaskEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;

			for (TResearchPriority rp = TResearchPriority.rpNone; rp <= TResearchPriority.rpTop; rp++)
			{
				this.EditPriority.Items.Add(LangMan.LSList[(int)TGenEngine.PriorityNames[(int)rp] - 1]);
			}

			for (TGoalType gt = TGoalType.gtIndividual; gt <= TGoalType.gtOther; gt++)
			{
				this.cbGoalType.Items.Add(LangMan.LSList[(int)TGenEngine.GoalNames[(int)gt] - 1]);
			}

			this.FNotesList = new TSheetList(this.SheetNotes);
			this.FNotesList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecNotesList(this.FNotesList);

			this.FTempRec = null;
			this.Text = LangMan.LSList[190];
			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.SheetNotes.Text = LangMan.LSList[54];
			this.Label1.Text = LangMan.LSList[182];
			this.Label2.Text = LangMan.LSList[178];
			this.Label4.Text = LangMan.LSList[180];
			this.Label5.Text = LangMan.LSList[181];
		}
	}
}
