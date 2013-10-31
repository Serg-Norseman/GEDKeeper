using System;
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
	public partial class TfmResearchEdit : Form
	{
		private TfmBase FBase;
		private TGEDCOMResearchRecord FResearch;
		private GKSheetList FTasksList;
		private GKSheetList FCommunicationsList;
		private GKSheetList FGroupsList;
		private GKSheetList FNotesList;


		public TfmBase Base
		{
			get { return this.FBase; }
		}

		public TGEDCOMResearchRecord Research
		{
			get { return this.FResearch; }
			set { this.SetResearch(value); }
		}

		private void AcceptChanges()
		{
			this.FResearch.ResearchName = this.EditName.Text;
			this.FResearch.Priority = (TResearchPriority)this.EditPriority.SelectedIndex;
			this.FResearch.Status = (TResearchStatus)this.EditStatus.SelectedIndex;
			this.FResearch.StartDate.ParseString(GKUtils.StrToGEDCOMDate(this.EditStartDate.Text, true));
			this.FResearch.StopDate.ParseString(GKUtils.StrToGEDCOMDate(this.EditStopDate.Text, true));
			this.FResearch.Percent = int.Parse(this.EditPercent.Text);
			this.Base.ChangeRecord(this.FResearch);
		}

		private void SetResearch(TGEDCOMResearchRecord Value)
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
					this.EditName.Text = this.FResearch.ResearchName;
					this.EditPriority.SelectedIndex = (int)this.FResearch.Priority;
					this.EditStatus.SelectedIndex = (int)this.FResearch.Status;
					this.EditStartDate.Text = GKUtils.GEDCOMDateToStr(this.FResearch.StartDate, TDateFormat.dfDD_MM_YYYY);
					this.EditStopDate.Text = GKUtils.GEDCOMDateToStr(this.FResearch.StopDate, TDateFormat.dfDD_MM_YYYY);
					this.EditPercent.Text = this.FResearch.Percent.ToString();
				}
				this.ListsRefresh();
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("ResearchEdit.SetResearch(): " + E.Message);
			}
		}

		private void ListModify(object sender, ModifyEventArgs eArgs)
		{
            bool res = false;

            if (sender == this.FNotesList)
            {
            	res = this.Base.ModifyRecNote(this, this.FResearch, eArgs.ItemData as TGEDCOMNotes, eArgs.Action);
            }
            else if (sender == this.FTasksList)
            {
            	TGEDCOMTaskRecord task = eArgs.ItemData as TGEDCOMTaskRecord;

            	switch (eArgs.Action)
            	{
            		case TRecAction.raAdd:
            			task = this.Base.SelectRecord(TGEDCOMRecordType.rtTask, null) as TGEDCOMTaskRecord;
            			res = this.FResearch.aux_AddTask(task);
            			break;

            		case TRecAction.raEdit:
            			res = (task != null && this.Base.ModifyTask(ref task));
            			break;

            		case TRecAction.raDelete:
            			if (task != null && GKUtils.ShowQuestion(LangMan.LSList[186]) != DialogResult.No)
            			{
            				this.FResearch.aux_RemoveTask(task);
            				res = true;
            			}
            			break;

            		case TRecAction.raJump:
            			if (task != null)
            			{
            				this.AcceptChanges();
            				this.Base.SelectRecordByXRef(task.XRef);
            				base.Close();
            			}
            			break;
            	}
            }
            else if (sender == this.FCommunicationsList)
            {
            	TGEDCOMCommunicationRecord comm = eArgs.ItemData as TGEDCOMCommunicationRecord;

            	switch (eArgs.Action)
            	{
            		case TRecAction.raAdd:
            			comm = this.Base.SelectRecord(TGEDCOMRecordType.rtCommunication, null) as TGEDCOMCommunicationRecord;
            			res = this.FResearch.aux_AddCommunication(comm);
            			break;

            		case TRecAction.raEdit:
            			res = (comm != null && this.Base.ModifyCommunication(ref comm));
            			break;

            		case TRecAction.raDelete:
            			if (comm != null && GKUtils.ShowQuestion(LangMan.LSList[187]) != DialogResult.No)
            			{
            				this.FResearch.aux_RemoveCommunication(comm);
            				res = true;
            			}
            			break;

            		case TRecAction.raJump:
            			if (comm != null)
            			{
            				this.AcceptChanges();
            				this.Base.SelectRecordByXRef(comm.XRef);
            				base.Close();
            			}
            			break;
            	}
            }
            else if (sender == this.FGroupsList)
            {
            	TGEDCOMGroupRecord group = eArgs.ItemData as TGEDCOMGroupRecord;

            	switch (eArgs.Action)
            	{
            		case TRecAction.raAdd:
            			group = this.Base.SelectRecord(TGEDCOMRecordType.rtGroup, null) as TGEDCOMGroupRecord;
            			res = this.FResearch.aux_AddGroup(group);
            			break;

            		case TRecAction.raDelete:
            			if (group != null && GKUtils.ShowQuestion(LangMan.LSList[188]) != DialogResult.No)
            			{
            				this.FResearch.aux_RemoveGroup(group);
            				res = true;
            			}
            			break;

            		case TRecAction.raJump:
            			if (group != null)
            			{
            				this.AcceptChanges();
            				this.Base.SelectRecordByXRef(group.XRef);
            				base.Close();
            			}
            			break;
            	}
            }

            if (res) this.ListsRefresh();
		}

		private void ListsRefresh()
		{
			this.Base.RecListNotesRefresh(this.FResearch, this.FNotesList.List, null);
			GKListView list = this.FTasksList.List;
			list.BeginUpdate();
			list.Items.Clear();

			int num = this.FResearch.Tasks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMTaskRecord task = this.FResearch.Tasks[i].Value as TGEDCOMTaskRecord;
				GKListItem item = list.AddItem(GKUtils.GetTaskGoalStr(task), task);
				item.SubItems.Add(LangMan.LSList[(int)GKData.PriorityNames[(int)task.Priority] - 1]);
				item.SubItems.Add(GKUtils.GEDCOMDateToStr(task.StartDate, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat));
				item.SubItems.Add(GKUtils.GEDCOMDateToStr(task.StopDate, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat));
			}
			list.EndUpdate();

			GKListView list2 = this.FCommunicationsList.List;
			list2.BeginUpdate();
			list2.Items.Clear();

			int num2 = this.FResearch.Communications.Count - 1;
			for (int i = 0; i <= num2; i++)
			{
				TGEDCOMCommunicationRecord corr = this.FResearch.Communications[i].Value as TGEDCOMCommunicationRecord;
				GKListItem item = list2.AddItem(corr.CommName, corr);
				item.SubItems.Add(GKUtils.GetCorresponderStr(this.Base.Tree, corr, false));
				item.SubItems.Add(LangMan.LSList[(int)GKData.CommunicationNames[(int)corr.CommunicationType] - 1]);
				item.SubItems.Add(GKUtils.GEDCOMDateToStr(corr.Date, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat));
			}
			list2.EndUpdate();

			GKListView list3 = this.FGroupsList.List;
			list3.BeginUpdate();
			list3.Items.Clear();

			int num3 = this.FResearch.Groups.Count - 1;
			for (int i = 0; i <= num3; i++)
			{
				TGEDCOMGroupRecord grp = this.FResearch.Groups[i].Value as TGEDCOMGroupRecord;
				GKListItem item = list3.AddItem(grp.GroupName, grp);
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
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmResearchEdit.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		public TfmResearchEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;

			for (TResearchPriority rp = TResearchPriority.rpNone; rp <= TResearchPriority.rpTop; rp++)
			{
				this.EditPriority.Items.Add(LangMan.LSList[(int)GKData.PriorityNames[(int)rp] - 1]);
			}

			for (TResearchStatus rs = TResearchStatus.rsDefined; rs <= TResearchStatus.rsWithdrawn; rs++)
			{
				this.EditStatus.Items.Add(LangMan.LSList[(int)GKData.StatusNames[(int)rs] - 1]);
			}

			this.FTasksList = new GKSheetList(this.SheetTasks);
			this.FTasksList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.FTasksList.Buttons = EnumSet.Create(new Enum[]
			{
				GKSheetList.TListButton.lbAdd, 
				GKSheetList.TListButton.lbEdit, 
				GKSheetList.TListButton.lbDelete, 
				GKSheetList.TListButton.lbJump
			});
			this.FTasksList.List.AddListColumn(LangMan.LSList[182], 250, false);
			this.FTasksList.List.AddListColumn(LangMan.LSList[178], 90, false);
			this.FTasksList.List.AddListColumn(LangMan.LSList[180], 90, false);
			this.FTasksList.List.AddListColumn(LangMan.LSList[181], 90, false);

			this.FCommunicationsList = new GKSheetList(this.SheetCommunications);
			this.FCommunicationsList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.FCommunicationsList.Buttons = EnumSet.Create(new Enum[]
			{
				GKSheetList.TListButton.lbAdd, 
				GKSheetList.TListButton.lbEdit, 
				GKSheetList.TListButton.lbDelete, 
				GKSheetList.TListButton.lbJump
			});
			this.FCommunicationsList.List.AddListColumn(LangMan.LSList[183], 150, false);
			this.FCommunicationsList.List.AddListColumn(LangMan.LSList[184], 150, false);
			this.FCommunicationsList.List.AddListColumn(LangMan.LSList[113], 90, false);
			this.FCommunicationsList.List.AddListColumn(LangMan.LSList[139], 90, false);

			this.FGroupsList = new GKSheetList(this.SheetGroups);
			this.FGroupsList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.FGroupsList.Buttons = EnumSet.Create(new Enum[]
			{
				GKSheetList.TListButton.lbAdd, 
				GKSheetList.TListButton.lbEdit, 
				GKSheetList.TListButton.lbDelete, 
				GKSheetList.TListButton.lbJump
			});
			this.FGroupsList.List.AddListColumn(LangMan.LSList[185], 350, false);

			this.FNotesList = new GKSheetList(this.SheetNotes);
			this.FNotesList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.Base.SetupRecNotesList(this.FNotesList);

			this.SetLang();
		}

		public void SetLang()
		{
			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.Text = LangMan.LSList[189];
			this.SheetTasks.Text = LangMan.LSList[60];
			this.SheetCommunications.Text = LangMan.LSList[61];
			this.SheetGroups.Text = LangMan.LSList[58];
			this.SheetNotes.Text = LangMan.LSList[54];
			this.Label1.Text = LangMan.LSList[125];
			this.Label2.Text = LangMan.LSList[178];
			this.Label3.Text = LangMan.LSList[117];
			this.Label6.Text = LangMan.LSList[179];
			this.Label4.Text = LangMan.LSList[180];
			this.Label5.Text = LangMan.LSList[181];
		}
	}
}
