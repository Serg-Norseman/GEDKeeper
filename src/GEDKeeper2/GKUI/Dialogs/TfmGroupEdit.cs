using System;
using System.Windows.Forms;

using BSLib;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Controls;
using GKUI.Sheets;

namespace GKUI.Dialogs
{
	/// <summary>
	/// 
	/// </summary>
	public partial class TfmGroupEdit : Form, IBaseEditor
	{
		private readonly IBaseWindow fBase;
        private readonly GKSheetList fMembersList;
		private readonly GKNotesSheet fNotesList;
        private readonly GKMediaSheet fMediaList;

        private GEDCOMGroupRecord fGroup;

		public GEDCOMGroupRecord Group
		{
			get { return this.fGroup; }
			set { this.SetGroup(value); }
		}

		public IBaseWindow Base
		{
			get { return this.fBase; }
		}

		private void SetGroup(GEDCOMGroupRecord value)
		{
			this.fGroup = value;
			try
			{
			    this.edName.Text = (this.fGroup == null) ? "" : this.fGroup.GroupName;

                this.fNotesList.DataList = this.fGroup.Notes.GetEnumerator();
                this.fMediaList.DataList = this.fGroup.MultimediaLinks.GetEnumerator();
                this.UpdateMembersSheet();
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmGroupEdit.SetGroup(): " + ex.Message);
			}
		}
		
		public TfmGroupEdit(IBaseWindow aBase)
		{
			this.InitializeComponent();
			
            this.fBase = aBase;

            this.fMembersList = CreateMembersSheet(this.SheetMembers);
			this.fNotesList = new GKNotesSheet(this, this.SheetNotes);
            this.fMediaList = new GKMediaSheet(this, this.SheetMultimedia);

            // SetLang()
            this.Text = LangMan.LS(LSID.LSID_WinGroupEdit);
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Label1.Text = LangMan.LS(LSID.LSID_Title);
            this.SheetMembers.Text = LangMan.LS(LSID.LSID_Members);
            this.SheetNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            this.SheetMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
		}

		private GKSheetList CreateMembersSheet(Control owner)
		{
			GKSheetList sheet = new GKSheetList(owner);
			
            sheet.Columns_BeginUpdate();
            sheet.AddColumn(LangMan.LS(LSID.LSID_Name), 300, false);
            sheet.Columns_EndUpdate();

            sheet.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbDelete, SheetButton.lbJump);
            sheet.OnModify += this.ModifyMembersSheet;
            
            return sheet;
		}
		
		private void UpdateMembersSheet()
		{
            try
            {
                this.fMembersList.ClearItems();

                foreach (GEDCOMPointer ptrMember in this.fGroup.Members) {
                    GEDCOMIndividualRecord member = ptrMember.Value as GEDCOMIndividualRecord;
                    if (member == null) continue;

                    this.fMembersList.AddItem(member.GetNameString(true, false), member);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TfmGroupEdit.UpdateMembersSheet(): " + ex.Message);
            }
		}

		private void ModifyMembersSheet(object sender, ModifyEventArgs eArgs)
		{
			bool result = false;

			GEDCOMIndividualRecord member = eArgs.ItemData as GEDCOMIndividualRecord;

			switch (eArgs.Action)
			{
				case RecordAction.raAdd:
					member = this.fBase.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svNone);
					result = (member != null && this.fGroup.AddMember(member));
					break;

				case RecordAction.raDelete:
					result = (member != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachMemberQuery)) != DialogResult.No && this.fGroup.RemoveMember(member));
					break;
					
				case RecordAction.raJump:
					if (member != null) {
						this.AcceptChanges();
						base.DialogResult = DialogResult.OK;
						this.fBase.SelectRecordByXRef(member.XRef);
						base.Close();
					}
					break;
			}

			if (result) this.UpdateMembersSheet();
		}

		private void AcceptChanges()
		{
			this.fGroup.GroupName = this.edName.Text;
			this.fBase.ChangeRecord(this.fGroup);
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
				this.fBase.Host.LogWrite("TfmGroupEdit.btnAccept_Click(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}
	}
}
