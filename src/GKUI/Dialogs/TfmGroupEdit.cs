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
	public partial class TfmGroupEdit : Form
	{
		private TfmBase FBase;
		private TGEDCOMGroupRecord FGroup;
		private GKSheetList FMembersList;
		private GKSheetList FNotesList;
		private GKSheetList FMediaList;


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

		private void SetGroup(TGEDCOMGroupRecord Value)
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
            bool res = false;

			if (object.Equals(Sender, this.FNotesList))
			{
                res = (this.Base.ModifyRecNote(this, this.FGroup, ItemData as TGEDCOMNotes, Action));
			}
			else
			{
				if (object.Equals(Sender, this.FMediaList))
				{
                    res = (this.Base.ModifyRecMultimedia(this, this.FGroup, ItemData as TGEDCOMMultimediaLink, Action));
				}
				else
				{
					if (object.Equals(Sender, this.FMembersList))
					{
                        TGEDCOMIndividualRecord member = (Action == TGenEngine.TRecAction.raAdd) ? null : ItemData as TGEDCOMIndividualRecord;

                        switch (Action)
                        {
                            case TGenEngine.TRecAction.raAdd:
							    member = this.Base.SelectPerson(null, TGenEngine.TTargetMode.tmNone, TGEDCOMSex.svNone);
                                res = (member != null && this.FGroup.aux_AddMember(member));
                                break;

                            case TGenEngine.TRecAction.raDelete:
                                res = (member != null && TGenEngine.ShowQuestion(LangMan.LSList[128]) != DialogResult.No && this.FGroup.aux_RemoveMember(member));
                                break;

                            case TGenEngine.TRecAction.raJump:
								if (member != null)
								{
									this.AcceptChanges();
									this.Base.SelectRecordByXRef(member.XRef);
									base.Close();
								}
                                break;
                        }
					}
				}
			}

            if (res) this.ListsRefresh();
		}

		private void ListsRefresh()
		{
			this.Base.RecListNotesRefresh(this.FGroup, this.FNotesList.List, null);
			this.Base.RecListMediaRefresh(this.FGroup, this.FMediaList.List, null);

			GKListView list = this.FMembersList.List;

			list.BeginUpdate();
			list.Items.Clear();
			int num = this.FGroup.Members.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMIndividualRecord member = this.FGroup.Members[i].Value as TGEDCOMIndividualRecord;
				list.AddItem(member.aux_GetNameStr(true, false), member);
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
			this.FMembersList = new GKSheetList(this.SheetMembers);
			this.FMembersList.OnModify += new GKSheetList.TModifyEvent(this.ListModify);
			this.FMembersList.Buttons = EnumSet.Create(new Enum[]
			{
				GKSheetList.TListButton.lbAdd, 
				GKSheetList.TListButton.lbEdit, 
				GKSheetList.TListButton.lbDelete, 
				GKSheetList.TListButton.lbJump
			});
			this.FMembersList.List.AddListColumn(LangMan.LSList[85], 300, false);

			this.FNotesList = new GKSheetList(this.SheetNotes);
			this.FNotesList.OnModify += new GKSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecNotesList(this.FNotesList);

			this.FMediaList = new GKSheetList(this.SheetMultimedia);
			this.FMediaList.OnModify += new GKSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecMediaList(this.FMediaList);

			this.Text = LangMan.LSList[127];
			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.Label1.Text = LangMan.LSList[125];
			this.SheetMembers.Text = LangMan.LSList[126];
			this.SheetNotes.Text = LangMan.LSList[54];
			this.SheetMultimedia.Text = LangMan.LSList[55];
		}
	}
}
