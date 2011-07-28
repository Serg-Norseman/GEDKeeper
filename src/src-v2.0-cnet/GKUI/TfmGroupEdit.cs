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
	public class TfmGroupEdit : Form
	{
		private GroupBox GroupBox1;
		private TextBox edName;
		private Label Label1;
		private TabControl PagesGroupData;
		private TabPage SheetNotes;
		private TabPage SheetMultimedia;
		private TabPage SheetMembers;
		private Button btnAccept;
		private Button btnCancel;
		private TfmBase FBase;
		private TGEDCOMGroupRecord FGroup;
		private TSheetList FMembersList;
		private TSheetList FNotesList;
		private TSheetList FMediaList;

		[Browsable(false)]
		public TfmBase Base
		{
			get	{ return this.FBase; }
		}

		[Browsable(false)]
		public TGEDCOMGroupRecord Group
		{
			get
			{
				return this.FGroup;
			}
			set
			{
				this.SetGroup(value);
			}
		}
		private void AcceptChanges()
		{
			this.FGroup.Name = this.edName.Text;
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
					this.edName.Text = this.FGroup.Name;
				}
				this.ListsRefresh();
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GroupEdit.SetGroup(): " + E.Message);
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
								if (member != null && TGKSys.ShowQuestion(GKL.LSList[128]) != DialogResult.No && this.Base.Engine.RemoveGroupMember(this.FGroup, member))
								{
									this.ListsRefresh();
								}
							}
						}
						else
						{
							TGEDCOMIndividualRecord member = this.Base.SelectPerson(null, TGenEngine.TTargetMode.tmNone, TGEDCOMObject.TGEDCOMSex.svNone);
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
			int arg_66_0 = 0;
			int num = this.FGroup.GetMembersCount() - 1;
			int i = arg_66_0;
			if (num >= i)
			{
				num++;
				do
				{
					TGEDCOMIndividualRecord member = this.FGroup.GetMember(i).Value as TGEDCOMIndividualRecord;
					TExtListItem item = list.AddItem(TGenEngine.GetNameStr(member, true, false), member);
					i++;
				}
				while (i != num);
			}
			list.EndUpdate();
		}
		private void InitializeComponent()
		{
			this.GroupBox1 = new GroupBox();
			this.Label1 = new Label();
			this.edName = new TextBox();
			this.PagesGroupData = new TabControl();
			this.SheetMembers = new TabPage();
			this.SheetNotes = new TabPage();
			this.SheetMultimedia = new TabPage();
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.GroupBox1.SuspendLayout();
			this.PagesGroupData.SuspendLayout();
			base.SuspendLayout();
			this.GroupBox1.Controls.Add(this.Label1);
			this.GroupBox1.Controls.Add(this.edName);
			this.GroupBox1.Location = new Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new Size(482, 49);
			this.GroupBox1.TabIndex = 1;
			this.GroupBox1.TabStop = false;
			this.Label1.Location = new Point(8, 24);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(55, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Название";
			this.edName.Location = new Point(72, 16);
			this.edName.Name = "edName";
			this.edName.Size = new Size(401, 21);
			this.edName.TabIndex = 0;
			this.edName.Text = "";
			this.PagesGroupData.Controls.Add(this.SheetMembers);
			this.PagesGroupData.Controls.Add(this.SheetNotes);
			this.PagesGroupData.Controls.Add(this.SheetMultimedia);
			this.PagesGroupData.Location = new Point(0, 49);
			this.PagesGroupData.Name = "PagesGroupData";
			this.PagesGroupData.SelectedIndex = 0;
			this.PagesGroupData.Size = new Size(482, 360);
			this.PagesGroupData.TabIndex = 0;
			this.SheetMembers.Location = new Point(4, 22);
			this.SheetMembers.Name = "SheetMembers";
			this.SheetMembers.Size = new Size(474, 334);
			this.SheetMembers.TabIndex = 0;
			this.SheetMembers.Text = "Участники";
			this.SheetNotes.Location = new Point(4, 22);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new Size(474, 334);
			this.SheetNotes.TabIndex = 1;
			this.SheetNotes.Text = "Заметки";
			this.SheetMultimedia.Location = new Point(4, 22);
			this.SheetMultimedia.Name = "SheetMultimedia";
			this.SheetMultimedia.Size = new Size(474, 334);
			this.SheetMultimedia.TabIndex = 2;
			this.SheetMultimedia.Text = "Мультимедиа";
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(304, 424);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 2;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(392, 424);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(482, 457);
			base.Controls.Add(this.btnCancel);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.GroupBox1);
			base.Controls.Add(this.PagesGroupData);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmGroupEdit";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Правка группы";
			this.GroupBox1.ResumeLayout(false);
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
		public TfmGroupEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.FMembersList = new TSheetList(this.SheetMembers);
			this.FMembersList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.FMembersList.Buttons = TEnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbJump
			});
			this.FMembersList.List.AddListColumn(GKL.LSList[85], 300, false);
			this.FNotesList = new TSheetList(this.SheetNotes);
			this.FNotesList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecNotesList(this.FNotesList);
			this.FMediaList = new TSheetList(this.SheetMultimedia);
			this.FMediaList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
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
