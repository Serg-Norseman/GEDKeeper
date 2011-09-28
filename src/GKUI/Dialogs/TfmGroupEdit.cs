using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Controls;
using GKUI.Lists;

namespace GKUI
{
	public partial class TfmGroupEdit : Form
	{
		private TfmBase FBase;
		private TGEDCOMGroupRecord FGroup;
		private TSheetList FMembersList;
		private TSheetList FNotesList;
		private TSheetList FMediaList;


		public TfmBase Base
		{
			get	{ return this.FBase; }
		}


		public TGEDCOMGroupRecord Group
		{
			get { return this.FGroup; }
			set { this.SetGroup(value); }
		}

		private void AcceptChanges()
		{
			this.FGroup.GroupName = this.edName.Text;
			this.Base.ChangeRecord(this.FGroup);
		}

		private void SetGroup([In] TGEDCOMGroupRecord Value)
		{
			this.FGroup = Value;
			try
			{
				if (this.FGroup == null)
				{
					this.edName.Text = "";
				}
				else
				{
					this.edName.Text = this.FGroup.GroupName;
				}
				this.ListsRefresh();
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GroupEdit.SetGroup(): " + E.Message);
			}
		}

		private void ListModify(object Sender, object ItemData, TGenEngine.TRecAction Action)
		{
			if (object.Equals(Sender, this.FNotesList))
			{
				if (this.Base.ModifyRecNote(this, this.FGroup, ItemData as TGEDCOMNotes, Action))
				{
					this.ListsRefresh();
				}
			}
			else
			{
				if (object.Equals(Sender, this.FMediaList))
				{
					if (this.Base.ModifyRecMultimedia(this, this.FGroup, ItemData as TGEDCOMMultimediaLink, Action))
					{
						this.ListsRefresh();
					}
				}
				else
				{
					if (object.Equals(Sender, this.FMembersList))
					{
						if (Action != TGenEngine.TRecAction.raAdd)
						{
							if (Action != TGenEngine.TRecAction.raDelete)
							{
								if (Action == TGenEngine.TRecAction.raJump)
								{
									TGEDCOMIndividualRecord member = ItemData as TGEDCOMIndividualRecord;
									if (member != null)
									{
										this.AcceptChanges();
										this.Base.SelectRecordByXRef(member.XRef);
										base.Close();
									}
								}
							}
							else
							{
								TGEDCOMIndividualRecord member = ItemData as TGEDCOMIndividualRecord;
								if (member != null && SysUtils.ShowQuestion(GKL.LSList[128]) != DialogResult.No && this.Base.Engine.RemoveGroupMember(this.FGroup, member))
								{
									this.ListsRefresh();
								}
							}
						}
						else
						{
							TGEDCOMIndividualRecord member = this.Base.SelectPerson(null, TGenEngine.TTargetMode.tmNone, TGEDCOMSex.svNone);
							if (member != null && this.Base.Engine.AddGroupMember(this.FGroup, member))
							{
								this.ListsRefresh();
							}
						}
					}
				}
			}
		}

		private void ListsRefresh()
		{
			this.Base.RecListNotesRefresh(this.FGroup, this.FNotesList.List, null);
			this.Base.RecListMediaRefresh(this.FGroup, this.FMediaList.List, null);

			TGKListView list = this.FMembersList.List;

			list.BeginUpdate();
			list.Items.Clear();
			int num = this.FGroup.Members.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMIndividualRecord member = this.FGroup.Members[i].Value as TGEDCOMIndividualRecord;
				TExtListItem item = list.AddItem(TGenEngine.GetNameStr(member, true, false), member);
			}
			list.EndUpdate();
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
				SysUtils.LogWrite("TfmGroupEdit.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		public TfmGroupEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.FMembersList = new TSheetList(this.SheetMembers);
			this.FMembersList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.FMembersList.Buttons = TEnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbJump
			});
			this.FMembersList.List.AddListColumn(GKL.LSList[85], 300, false);

			this.FNotesList = new TSheetList(this.SheetNotes);
			this.FNotesList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecNotesList(this.FNotesList);

			this.FMediaList = new TSheetList(this.SheetMultimedia);
			this.FMediaList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecMediaList(this.FMediaList);

			this.Text = GKL.LSList[127];
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.Label1.Text = GKL.LSList[125];
			this.SheetMembers.Text = GKL.LSList[126];
			this.SheetNotes.Text = GKL.LSList[54];
			this.SheetMultimedia.Text = GKL.LSList[55];
		}
	}
}
