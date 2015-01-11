using System;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKUI.Controls;
using GKUI.Sheets;

/// <summary>
/// 
/// </summary>

namespace GKUI.Dialogs
{
	public partial class TfmGroupEdit : Form, IBaseEditor
	{
		private readonly IBase fBase;
        private readonly GKMembersSheet fMembersList;
		private readonly GKNotesSheet fNotesList;
        private readonly GKMediaSheet fMediaList;

        private TGEDCOMGroupRecord fGroup;

		public TGEDCOMGroupRecord Group
		{
			get { return this.fGroup; }
			set { this.SetGroup(value); }
		}

		public IBase Base
		{
			get { return this.fBase; }
		}

		private void AcceptChanges()
		{
			this.fGroup.GroupName = this.edName.Text;
			this.fBase.ChangeRecord(this.fGroup);
		}

		private void SetGroup(TGEDCOMGroupRecord value)
		{
			this.fGroup = value;
			try
			{
			    this.edName.Text = (this.fGroup == null) ? "" : this.fGroup.GroupName;

                this.fNotesList.DataList = this.fGroup.Notes.GetEnumerator();
                this.fMediaList.DataList = this.fGroup.MultimediaLinks.GetEnumerator();
                this.fMembersList.DataList = this.fGroup.Members.GetEnumerator();
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmGroupEdit.SetGroup(): " + ex.Message);
			}
		}

		private void ListModify(object sender, ModifyEventArgs eArgs)
		{
            TGEDCOMIndividualRecord member = eArgs.ItemData as TGEDCOMIndividualRecord;

            if (sender == this.fMembersList && eArgs.Action == RecordAction.raJump && member != null)
            {
            	this.AcceptChanges();
            	this.fBase.SelectRecordByXRef(member.XRef);
            	base.Close();
            }
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
				this.fBase.Host.LogWrite("TfmGroupEdit.Accept(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		public TfmGroupEdit(IBase aBase)
		{
			this.InitializeComponent();
			
            this.fBase = aBase;

            this.fMembersList = new GKMembersSheet(this, this.SheetMembers);
			this.fMembersList.OnModify += this.ListModify;
			
			this.fNotesList = new GKNotesSheet(this, this.SheetNotes);
            this.fMediaList = new GKMediaSheet(this, this.SheetMultimedia);

            this.Text = LangMan.LS(LSID.LSID_WinGroupEdit);
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Label1.Text = LangMan.LS(LSID.LSID_Title);
            this.SheetMembers.Text = LangMan.LS(LSID.LSID_Members);
            this.SheetNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            this.SheetMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
		}
	}
}
