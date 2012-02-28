using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKSys;
using GKUI.Controls;
using GKUI.Lists;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI
{
	public partial class TfmResearchEdit : Form
	{
		private TfmBase FBase;
		private TGEDCOMResearchRecord FResearch;
		private TSheetList FTasksList;
		private TSheetList FCommunicationsList;
		private TSheetList FGroupsList;
		private TSheetList FNotesList;


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
					this.EditName.Text = this.FResearch.ResearchName;
					this.EditPriority.SelectedIndex = (int)this.FResearch.Priority;
					this.EditStatus.SelectedIndex = (int)this.FResearch.Status;
					this.EditStartDate.Text = TGenEngine.GEDCOMDateToStr(this.FResearch.StartDate, TGenEngine.TDateFormat.dfDD_MM_YYYY);
					this.EditStopDate.Text = TGenEngine.GEDCOMDateToStr(this.FResearch.StopDate, TGenEngine.TDateFormat.dfDD_MM_YYYY);
					this.EditPercent.Text = this.FResearch.Percent.ToString();
				}
				this.ListsRefresh();
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("ResearchEdit.SetResearch(): " + E.Message);
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
								if (task != null && TGenEngine.ShowQuestion(LangMan.LSList[186]) != DialogResult.No)
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
						TGEDCOMRecordType arg_6F_1 = TGEDCOMRecordType.rtTask;
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
									if (comm != null && TGenEngine.ShowQuestion(LangMan.LSList[187]) != DialogResult.No)
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
							TGEDCOMRecordType arg_17E_1 = TGEDCOMRecordType.rtCommunication;
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
									if (group != null && TGenEngine.ShowQuestion(LangMan.LSList[188]) != DialogResult.No)
									{
										this.Base.Engine.RemoveResearchGroup(this.FResearch, group);
										this.ListsRefresh();
									}
								}
							}
							else
							{
								TfmBase arg_288_0 = this.Base;
								TGEDCOMRecordType arg_288_1 = TGEDCOMRecordType.rtGroup;
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

			int num = this.FResearch.Tasks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMTaskRecord task = this.FResearch.Tasks[i].Value as TGEDCOMTaskRecord;
				TExtListItem item = list.AddItem(TGenEngine.GetTaskGoalStr(this.Base.Tree, task), task);
				item.SubItems.Add(LangMan.LSList[(int)TGenEngine.PriorityNames[(int)task.Priority] - 1]);
				item.SubItems.Add(TGenEngine.GEDCOMDateToStr(task.StartDate, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat));
				item.SubItems.Add(TGenEngine.GEDCOMDateToStr(task.StopDate, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat));
			}
			list.EndUpdate();

			TGKListView list2 = this.FCommunicationsList.List;
			list2.BeginUpdate();
			list2.Items.Clear();

			int num2 = this.FResearch.Communications.Count - 1;
			for (int i = 0; i <= num2; i++)
			{
				TGEDCOMCommunicationRecord corr = this.FResearch.Communications[i].Value as TGEDCOMCommunicationRecord;
				TExtListItem item = list2.AddItem(corr.CommName, corr);
				item.SubItems.Add(TGenEngine.GetCorresponderStr(this.Base.Tree, corr, false));
				item.SubItems.Add(LangMan.LSList[(int)TGenEngine.CommunicationNames[(int)corr.CommunicationType] - 1]);
				item.SubItems.Add(TGenEngine.GEDCOMDateToStr(corr.Date, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat));
			}
			list2.EndUpdate();

			TGKListView list3 = this.FGroupsList.List;
			list3.BeginUpdate();
			list3.Items.Clear();

			int num3 = this.FResearch.Groups.Count - 1;
			for (int i = 0; i <= num3; i++)
			{
				TGEDCOMGroupRecord grp = this.FResearch.Groups[i].Value as TGEDCOMGroupRecord;
				TExtListItem item = list3.AddItem(grp.GroupName, grp);
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
				this.EditPriority.Items.Add(LangMan.LSList[(int)TGenEngine.PriorityNames[(int)rp] - 1]);
			}

			for (TResearchStatus rs = TResearchStatus.rsDefined; rs <= TResearchStatus.rsWithdrawn; rs++)
			{
				this.EditStatus.Items.Add(LangMan.LSList[(int)TGenEngine.StatusNames[(int)rs] - 1]);
			}

			this.FTasksList = new TSheetList(this.SheetTasks);
			this.FTasksList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.FTasksList.Buttons = EnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbJump
			});
			this.FTasksList.List.AddListColumn(LangMan.LSList[182], 250, false);
			this.FTasksList.List.AddListColumn(LangMan.LSList[178], 90, false);
			this.FTasksList.List.AddListColumn(LangMan.LSList[180], 90, false);
			this.FTasksList.List.AddListColumn(LangMan.LSList[181], 90, false);

			this.FCommunicationsList = new TSheetList(this.SheetCommunications);
			this.FCommunicationsList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.FCommunicationsList.Buttons = EnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbJump
			});
			this.FCommunicationsList.List.AddListColumn(LangMan.LSList[183], 150, false);
			this.FCommunicationsList.List.AddListColumn(LangMan.LSList[184], 150, false);
			this.FCommunicationsList.List.AddListColumn(LangMan.LSList[113], 90, false);
			this.FCommunicationsList.List.AddListColumn(LangMan.LSList[139], 90, false);

			this.FGroupsList = new TSheetList(this.SheetGroups);
			this.FGroupsList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.FGroupsList.Buttons = EnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbJump
			});
			this.FGroupsList.List.AddListColumn(LangMan.LSList[185], 350, false);

			this.FNotesList = new TSheetList(this.SheetNotes);
			this.FNotesList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
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
