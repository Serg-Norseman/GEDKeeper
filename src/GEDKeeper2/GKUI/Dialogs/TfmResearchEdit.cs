using System;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKUI.Controls;
using GKUI.Sheets;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Dialogs
{
	public partial class TfmResearchEdit : Form, IBaseEditor
	{
		private readonly IBase fBase;

		private readonly GKSheetList fTasksList;
		private readonly GKSheetList fCommunicationsList;
		private readonly GKSheetList fGroupsList;
		private readonly GKNotesSheet fNotesList;

        private TGEDCOMResearchRecord fResearch;

		public TGEDCOMResearchRecord Research
		{
			get { return this.fResearch; }
			set { this.SetResearch(value); }
		}

		public IBase Base
		{
			get { return this.fBase; }
		}

		private void AcceptChanges()
		{
			this.fResearch.ResearchName = this.EditName.Text;
			this.fResearch.Priority = (TResearchPriority)this.EditPriority.SelectedIndex;
			this.fResearch.Status = (TResearchStatus)this.EditStatus.SelectedIndex;
			this.fResearch.StartDate.ParseString(GEDCOMUtils.StrToGEDCOMDate(this.EditStartDate.Text, true));
			this.fResearch.StopDate.ParseString(GEDCOMUtils.StrToGEDCOMDate(this.EditStopDate.Text, true));
			this.fResearch.Percent = int.Parse(this.EditPercent.Text);
			this.fBase.ChangeRecord(this.fResearch);
		}

		private void SetResearch(TGEDCOMResearchRecord value)
		{
			this.fResearch = value;
			try
			{
				if (this.fResearch == null)
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
					this.EditName.Text = this.fResearch.ResearchName;
					this.EditPriority.SelectedIndex = (int)this.fResearch.Priority;
					this.EditStatus.SelectedIndex = (int)this.fResearch.Status;
					this.EditStartDate.Text = GKUtils.GEDCOMDateToStr(this.fResearch.StartDate, DateFormat.dfDD_MM_YYYY);
					this.EditStopDate.Text = GKUtils.GEDCOMDateToStr(this.fResearch.StopDate, DateFormat.dfDD_MM_YYYY);
					this.EditPercent.Text = this.fResearch.Percent.ToString();
				}

				this.RefreshLists();
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmResearchEdit.SetResearch(): " + ex.Message);
			}
		}

        private bool ListTasksModify(object sender, ModifyEventArgs eArgs)
	    {
            bool res = false;

            TGEDCOMTaskRecord task = eArgs.ItemData as TGEDCOMTaskRecord;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    task = this.fBase.SelectRecord(TGEDCOMRecordType.rtTask, null) as TGEDCOMTaskRecord;
                    res = this.fResearch.aux_AddTask(task);
                    break;

                case RecordAction.raEdit:
                    res = (task != null && this.fBase.ModifyTask(ref task));
                    break;

                case RecordAction.raDelete:
                    if (task != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachTaskQuery)) != DialogResult.No)
                    {
                        this.fResearch.aux_RemoveTask(task);
                        res = true;
                    }
                    break;

                case RecordAction.raJump:
                    if (task != null)
                    {
                        this.AcceptChanges();
                        this.fBase.SelectRecordByXRef(task.XRef);
                        base.Close();
                    }
                    break;
            }
            
            return res;
	    }

        private bool ListCommunicationsModify(object sender, ModifyEventArgs eArgs)
        {
            bool res = false;

            TGEDCOMCommunicationRecord comm = eArgs.ItemData as TGEDCOMCommunicationRecord;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    comm = this.fBase.SelectRecord(TGEDCOMRecordType.rtCommunication, null) as TGEDCOMCommunicationRecord;
                    res = this.fResearch.aux_AddCommunication(comm);
                    break;

                case RecordAction.raEdit:
                    res = (comm != null && this.fBase.ModifyCommunication(ref comm));
                    break;

                case RecordAction.raDelete:
                    if (comm != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachCommunicationQuery)) != DialogResult.No)
                    {
                        this.fResearch.aux_RemoveCommunication(comm);
                        res = true;
                    }
                    break;

                case RecordAction.raJump:
                    if (comm != null)
                    {
                        this.AcceptChanges();
                        this.fBase.SelectRecordByXRef(comm.XRef);
                        base.Close();
                    }
                    break;
            }

            return res;
        }

        private bool ListGroupsModify(object sender, ModifyEventArgs eArgs)
        {
            bool res = false;

            TGEDCOMGroupRecord group = eArgs.ItemData as TGEDCOMGroupRecord;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    group = this.fBase.SelectRecord(TGEDCOMRecordType.rtGroup, null) as TGEDCOMGroupRecord;
                    res = this.fResearch.aux_AddGroup(group);
                    break;

                case RecordAction.raDelete:
                    if (group != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachGroupQuery)) != DialogResult.No)
                    {
                        this.fResearch.aux_RemoveGroup(group);
                        res = true;
                    }
                    break;

                case RecordAction.raJump:
                    if (group != null)
                    {
                        this.AcceptChanges();
                        this.fBase.SelectRecordByXRef(group.XRef);
                        base.Close();
                    }
                    break;
            }

            return res;
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
		{
            bool res = false;

            if (sender == this.fTasksList)
            {
                res = this.ListTasksModify(sender, eArgs);
            }
            else if (sender == this.fCommunicationsList)
            {
                res = this.ListCommunicationsModify(sender, eArgs);
            }
            else if (sender == this.fGroupsList)
            {
                res = this.ListGroupsModify(sender, eArgs);
            }

            if (res) this.RefreshLists();
		}

		private void RefreshLists()
		{
		    DateFormat defaultDateFormat = TfmGEDKeeper.Instance.Options.DefDateFormat;

		    this.fNotesList.DataList = this.fResearch.Notes.GetEnumerator();

			GKListView list = this.fTasksList.List;
			list.BeginUpdate();
			list.Items.Clear();

			int num = this.fResearch.Tasks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMTaskRecord task = this.fResearch.Tasks[i].Value as TGEDCOMTaskRecord;
				GKListItem item = list.AddItem(GKUtils.GetTaskGoalStr(task), task);
				item.SubItems.Add(LangMan.LS(GKData.PriorityNames[(int)task.Priority]));
                item.SubItems.Add(GKUtils.GEDCOMDateToStr(task.StartDate, defaultDateFormat));
                item.SubItems.Add(GKUtils.GEDCOMDateToStr(task.StopDate, defaultDateFormat));
			}
			list.EndUpdate();

			GKListView list2 = this.fCommunicationsList.List;
			list2.BeginUpdate();
			list2.Items.Clear();

			int num2 = this.fResearch.Communications.Count - 1;
			for (int i = 0; i <= num2; i++)
			{
				TGEDCOMCommunicationRecord corr = this.fResearch.Communications[i].Value as TGEDCOMCommunicationRecord;
				GKListItem item = list2.AddItem(corr.CommName, corr);
				item.SubItems.Add(GKUtils.GetCorresponderStr(this.fBase.Tree, corr, false));
				item.SubItems.Add(LangMan.LS(GKData.CommunicationNames[(int)corr.CommunicationType]));
                item.SubItems.Add(GKUtils.GEDCOMDateToStr(corr.Date, defaultDateFormat));
			}
			list2.EndUpdate();

			GKListView list3 = this.fGroupsList.List;
			list3.BeginUpdate();
			list3.Items.Clear();

			int num3 = this.fResearch.Groups.Count - 1;
			for (int i = 0; i <= num3; i++)
			{
				TGEDCOMGroupRecord grp = this.fResearch.Groups[i].Value as TGEDCOMGroupRecord;
				list3.AddItem(grp.GroupName, grp);
			}
			list3.EndUpdate();
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
				this.fBase.Host.LogWrite("TfmResearchEdit.Accept(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		public TfmResearchEdit(IBase aBase)
		{
			this.InitializeComponent();

			this.fBase = aBase;

			for (TResearchPriority rp = TResearchPriority.rpNone; rp <= TResearchPriority.rpTop; rp++)
			{
				this.EditPriority.Items.Add(LangMan.LS(GKData.PriorityNames[(int)rp]));
			}

			for (TResearchStatus rs = TResearchStatus.rsDefined; rs <= TResearchStatus.rsWithdrawn; rs++)
			{
				this.EditStatus.Items.Add(LangMan.LS(GKData.StatusNames[(int)rs]));
			}

			this.fTasksList = new GKSheetList(this.SheetTasks);
			this.fTasksList.OnModify += this.ListModify;
			this.fTasksList.Buttons = EnumSet<GKSheetList.SheetButton>.Create(
				GKSheetList.SheetButton.lbAdd, 
				GKSheetList.SheetButton.lbEdit, 
				GKSheetList.SheetButton.lbDelete, 
				GKSheetList.SheetButton.lbJump
			);
			this.fTasksList.List.AddListColumn(LangMan.LS(LSID.LSID_Goal), 250, false);
			this.fTasksList.List.AddListColumn(LangMan.LS(LSID.LSID_Priority), 90, false);
			this.fTasksList.List.AddListColumn(LangMan.LS(LSID.LSID_StartDate), 90, false);
			this.fTasksList.List.AddListColumn(LangMan.LS(LSID.LSID_StopDate), 90, false);

			this.fCommunicationsList = new GKSheetList(this.SheetCommunications);
			this.fCommunicationsList.OnModify += this.ListModify;
			this.fCommunicationsList.Buttons = EnumSet<GKSheetList.SheetButton>.Create(
				GKSheetList.SheetButton.lbAdd, 
				GKSheetList.SheetButton.lbEdit, 
				GKSheetList.SheetButton.lbDelete, 
				GKSheetList.SheetButton.lbJump
			);
			this.fCommunicationsList.List.AddListColumn(LangMan.LS(LSID.LSID_Theme), 150, false);
			this.fCommunicationsList.List.AddListColumn(LangMan.LS(LSID.LSID_Corresponder), 150, false);
			this.fCommunicationsList.List.AddListColumn(LangMan.LS(LSID.LSID_Type), 90, false);
			this.fCommunicationsList.List.AddListColumn(LangMan.LS(LSID.LSID_Date), 90, false);

			this.fGroupsList = new GKSheetList(this.SheetGroups);
			this.fGroupsList.OnModify += this.ListModify;
			this.fGroupsList.Buttons = EnumSet<GKSheetList.SheetButton>.Create(
				GKSheetList.SheetButton.lbAdd, 
				GKSheetList.SheetButton.lbEdit, 
				GKSheetList.SheetButton.lbDelete, 
				GKSheetList.SheetButton.lbJump
			);
			this.fGroupsList.List.AddListColumn(LangMan.LS(LSID.LSID_Group), 350, false);

			this.fNotesList = new GKNotesSheet(this, this.SheetNotes);

			this.SetLang();
		}

		public void SetLang()
		{
			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.Text = LangMan.LS(LSID.LSID_WinResearchEdit);
			this.SheetTasks.Text = LangMan.LS(LSID.LSID_RPTasks);
			this.SheetCommunications.Text = LangMan.LS(LSID.LSID_RPCommunications);
			this.SheetGroups.Text = LangMan.LS(LSID.LSID_RPGroups);
			this.SheetNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
			this.Label1.Text = LangMan.LS(LSID.LSID_Title);
			this.Label2.Text = LangMan.LS(LSID.LSID_Priority);
			this.Label3.Text = LangMan.LS(LSID.LSID_Status);
			this.Label6.Text = LangMan.LS(LSID.LSID_Percent);
			this.Label4.Text = LangMan.LS(LSID.LSID_StartDate);
			this.Label5.Text = LangMan.LS(LSID.LSID_StopDate);
		}
	}
}
