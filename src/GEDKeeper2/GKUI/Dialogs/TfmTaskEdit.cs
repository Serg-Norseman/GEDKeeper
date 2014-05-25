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
	public partial class TfmTaskEdit : Form, IBaseEditor
	{
		private readonly IBase fBase;
        private readonly GKNotesSheet fNotesList;
        
        private TGEDCOMTaskRecord fTask;
		private TGEDCOMRecord fTempRec;

		public TGEDCOMTaskRecord Task
		{
			get { return this.fTask; }
			set { this.SetTask(value); }
		}

		public IBase Base
		{
			get { return this.fBase; }
		}

		private void SetTask(TGEDCOMTaskRecord value)
		{
			this.fTask = value;
			try
			{
				if (this.fTask == null)
				{
					this.EditPriority.SelectedIndex = -1;
					this.EditStartDate.Text = "";
					this.EditStopDate.Text = "";
					this.cbGoalType.SelectedIndex = 0;
					this.EditGoal.Text = "";
				}
				else
				{
					this.EditPriority.SelectedIndex = (sbyte)this.fTask.Priority;
					this.EditStartDate.Text = GKUtils.GEDCOMDateToStr(this.fTask.StartDate, DateFormat.dfDD_MM_YYYY);
					this.EditStopDate.Text = GKUtils.GEDCOMDateToStr(this.fTask.StopDate, DateFormat.dfDD_MM_YYYY);

                    TGoalType gt;
                    this.fTask.aux_GetTaskGoal(out gt, out this.fTempRec);
					this.cbGoalType.SelectedIndex = (sbyte)gt;

					switch (gt) {
						case TGoalType.gtIndividual:
                            string st = ((this.fTempRec == null) ? "" : (this.fTempRec as TGEDCOMIndividualRecord).aux_GetNameStr(true, false));
							this.EditGoal.Text = st;
							break;
						case TGoalType.gtFamily:
							this.EditGoal.Text = GKUtils.aux_GetFamilyStr(this.fTempRec as TGEDCOMFamilyRecord);
							break;
						case TGoalType.gtSource:
							this.EditGoal.Text = (this.fTempRec as TGEDCOMSourceRecord).FiledByEntry;
							break;
						case TGoalType.gtOther:
							this.EditGoal.Text = this.fTask.Goal;
							break;
					}
				}
				this.cbGoalType_SelectedIndexChanged(null, null);
				
				this.fNotesList.DataList = this.fTask.Notes.GetEnumerator();
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmTaskEdit.SetTask(): " + ex.Message);
			}
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.fTask.Priority = (TResearchPriority)this.EditPriority.SelectedIndex;
				this.fTask.StartDate.ParseString(GEDCOMUtils.StrToGEDCOMDate(this.EditStartDate.Text, true));
				this.fTask.StopDate.ParseString(GEDCOMUtils.StrToGEDCOMDate(this.EditStopDate.Text, true));
				TGoalType gt = (TGoalType)this.cbGoalType.SelectedIndex;
				switch (gt) {
					case TGoalType.gtIndividual:
					case TGoalType.gtFamily:
					case TGoalType.gtSource:
						this.fTask.Goal = GEDCOMUtils.EncloseXRef(this.fTempRec.XRef);
						break;
					case TGoalType.gtOther:
						this.fTask.Goal = this.EditGoal.Text;
						break;
				}

				this.Base.ChangeRecord(this.fTask);
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmTaskEdit.Accept(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void btnGoalSelect_Click(object sender, EventArgs e)
		{
			TGoalType gt = (TGoalType)this.cbGoalType.SelectedIndex;
			switch (gt) {
				case TGoalType.gtIndividual:
					this.fTempRec = this.Base.SelectPerson(null, TargetMode.tmNone, TGEDCOMSex.svNone);
					this.EditGoal.Text = ((this.fTempRec == null) ? "" : (this.fTempRec as TGEDCOMIndividualRecord).aux_GetNameStr(true, false));
					break;
				case TGoalType.gtFamily:
					this.fTempRec = this.Base.SelectRecord(TGEDCOMRecordType.rtFamily, new object[0]);
					this.EditGoal.Text = GKUtils.aux_GetFamilyStr(this.fTempRec as TGEDCOMFamilyRecord);
					break;
				case TGoalType.gtSource:
					this.fTempRec = this.Base.SelectRecord(TGEDCOMRecordType.rtSource, new object[0]);
					this.EditGoal.Text = (this.fTempRec as TGEDCOMSourceRecord).FiledByEntry;
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

		public TfmTaskEdit(IBase aBase)
		{
			this.InitializeComponent();
			this.fBase = aBase;

			for (TResearchPriority rp = TResearchPriority.rpNone; rp <= TResearchPriority.rpTop; rp++)
			{
				this.EditPriority.Items.Add(LangMan.LS(GKData.PriorityNames[(int)rp]));
			}

			for (TGoalType gt = TGoalType.gtIndividual; gt <= TGoalType.gtOther; gt++)
			{
				this.cbGoalType.Items.Add(LangMan.LS(GKData.GoalNames[(int)gt]));
			}

			this.fNotesList = new GKNotesSheet(this, this.SheetNotes);

			this.fTempRec = null;
			this.Text = LangMan.LS(LSID.LSID_WinTaskEdit);
			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.SheetNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
			this.Label1.Text = LangMan.LS(LSID.LSID_Goal);
			this.Label2.Text = LangMan.LS(LSID.LSID_Priority);
			this.Label4.Text = LangMan.LS(LSID.LSID_StartDate);
			this.Label5.Text = LangMan.LS(LSID.LSID_StopDate);
		}
	}
}
