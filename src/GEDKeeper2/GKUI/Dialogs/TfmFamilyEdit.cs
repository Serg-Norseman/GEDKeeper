using System;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
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
	public partial class TfmFamilyEdit : Form, IBaseEditor
	{
		private readonly IBaseWindow fBase;

        private readonly GKSheetList fChildsList;
        private readonly GKEventsSheet fEventsList;
		private readonly GKNotesSheet fNotesList;
        private readonly GKMediaSheet fMediaList;
		private readonly GKSourcesSheet fSourcesList;

        private GEDCOMFamilyRecord fFamily;

		public IBaseWindow Base
		{
			get { return this.fBase; }
		}
        
		public GEDCOMFamilyRecord Family
		{
			get	{ return this.fFamily; }
			set	{ this.SetFamily(value); }
		}

		private void SetFamily(GEDCOMFamilyRecord value)
		{
			this.fFamily = value;
			try
			{
				if (this.fFamily == null)
				{
					this.btnHusbandSel.Enabled = false;
					this.btnWifeSel.Enabled = false;
					this.edMarriageStatus.Enabled = false;
					this.edMarriageStatus.SelectedIndex = 0;
					this.cbRestriction.SelectedIndex = 0;
				}
				else
				{
					string stat = this.fFamily.GetTagStringValue("_STAT");
					int statIdx = GKUtils.GetMarriageStatusIndex(stat);
					this.edMarriageStatus.Enabled = true;
					this.edMarriageStatus.SelectedIndex = statIdx;
					this.cbRestriction.SelectedIndex = (sbyte)this.fFamily.Restriction;
					
					this.UpdateControls();
				}
			}
			catch (Exception ex)
			{
                this.fBase.Host.LogWrite("TfmFamilyEdit.SetFamily(): " + ex.Message);
			}
		}

		public TfmFamilyEdit(IBaseWindow aBase)
		{
			this.InitializeComponent();
			this.fBase = aBase;

			for (GEDCOMRestriction res = GEDCOMRestriction.rnNone; res <= GEDCOMRestriction.rnLast; res++)
			{
				this.cbRestriction.Items.Add(LangMan.LS(GKData.Restrictions[(int)res]));
			}

			for (int i = 0; i < GKData.MarriageStatus.Length; i++)
			{
				this.edMarriageStatus.Items.Add(LangMan.LS(GKData.MarriageStatus[i].Name));
			}

            this.fChildsList = this.CreateChildsSheet(this.SheetChilds);
            this.fEventsList = new GKEventsSheet(this, this.SheetEvents, false);
			this.fNotesList = new GKNotesSheet(this, this.SheetNotes);
            this.fMediaList = new GKMediaSheet(this, this.SheetMultimedia);
			this.fSourcesList = new GKSourcesSheet(this, this.SheetSources);

			// SetLang()
			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.GroupBox1.Text = LangMan.LS(LSID.LSID_Family);
			this.Label1.Text = LangMan.LS(LSID.LSID_Husband);
			this.Label2.Text = LangMan.LS(LSID.LSID_Wife);
			this.Label6.Text = LangMan.LS(LSID.LSID_Status);
			this.SheetChilds.Text = LangMan.LS(LSID.LSID_Childs);
			this.SheetEvents.Text = LangMan.LS(LSID.LSID_Events);
			this.SheetNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
			this.SheetMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
			this.SheetSources.Text = LangMan.LS(LSID.LSID_RPSources);
			this.Label5.Text = LangMan.LS(LSID.LSID_Restriction);
		}

		private void UpdateControls()
		{
			GEDCOMIndividualRecord husband = this.fFamily.GetHusband();
			this.EditHusband.Text = (husband != null) ? husband.GetNameString(true, false) : LangMan.LS(LSID.LSID_UnkMale);

            this.btnHusbandAdd.Enabled = (husband == null);
			this.btnHusbandDelete.Enabled = (husband != null);
			this.btnHusbandSel.Enabled = (husband != null);

			GEDCOMIndividualRecord wife = this.fFamily.GetWife();
			this.EditWife.Text = (wife != null) ? wife.GetNameString(true, false) : LangMan.LS(LSID.LSID_UnkFemale);

            this.btnWifeAdd.Enabled = (wife == null);
			this.btnWifeDelete.Enabled = (wife != null);
			this.btnWifeSel.Enabled = (wife != null);

            this.fEventsList.DataList = this.fFamily.Events.GetEnumerator();
			this.fNotesList.DataList = this.fFamily.Notes.GetEnumerator();
		    this.fMediaList.DataList = this.fFamily.MultimediaLinks.GetEnumerator();
		    this.fSourcesList.DataList = this.fFamily.SourceCitations.GetEnumerator();
		    this.UpdateChildsSheet();

			this.LockEditor(this.fFamily.Restriction == GEDCOMRestriction.rnLocked);
		}

		private void LockEditor(bool locked)
		{
			this.btnHusbandAdd.Enabled = (this.btnHusbandAdd.Enabled && !locked);
			this.btnHusbandDelete.Enabled = (this.btnHusbandDelete.Enabled && !locked);
			this.btnWifeAdd.Enabled = (this.btnWifeAdd.Enabled && !locked);
			this.btnWifeDelete.Enabled = (this.btnWifeDelete.Enabled && !locked);

			this.edMarriageStatus.Enabled = (this.edMarriageStatus.Enabled && !locked);

			this.fChildsList.ReadOnly = locked;
			this.fEventsList.ReadOnly = locked;
			this.fNotesList.ReadOnly = locked;
			this.fMediaList.ReadOnly = locked;
			this.fSourcesList.ReadOnly = locked;
		}

		private void cbRestriction_SelectedIndexChanged(object sender, EventArgs e)
		{
			this.LockEditor(this.cbRestriction.SelectedIndex == (int)GEDCOMRestriction.rnLocked);
		}

		private GKSheetList CreateChildsSheet(Control owner)
		{
			GKSheetList sheet = new GKSheetList(owner);
			
            sheet.Columns_BeginUpdate();
            sheet.AddColumn("№", 25, false);
            sheet.AddColumn(LangMan.LS(LSID.LSID_Name), 300, false);
            sheet.AddColumn(LangMan.LS(LSID.LSID_BirthDate), 100, false);
            sheet.Columns_EndUpdate();

            sheet.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete, SheetButton.lbJump);
            sheet.OnModify += this.ModifyChildsSheet;

            return sheet;
		}
		
		private void UpdateChildsSheet()
		{
            try
            {
                this.fChildsList.SwitchSorter();
                this.fChildsList.BeginUpdate();
                this.fChildsList.ClearItems();

                int idx = 0;
                foreach (GEDCOMPointer ptr in this.fFamily.Childrens) {
                    idx += 1;

                    GEDCOMIndividualRecord child = ptr.Value as GEDCOMIndividualRecord;

                    GKListItem item = this.fChildsList.AddItem(idx, child);
                    item.AddSubItem(child.GetNameString(true, false));
                    item.AddSubItem(GKUtils.GetBirthDate(child));
                }

                this.fChildsList.EndUpdate();
                this.fChildsList.SwitchSorter();
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("TfmFamilyEdit.UpdateChildsSheet(): " + ex.Message);
            }
		}

		private void ModifyChildsSheet(object sender, ModifyEventArgs eArgs)
		{
            bool result = false;

            GEDCOMIndividualRecord child = eArgs.ItemData as GEDCOMIndividualRecord;

            switch (eArgs.Action)
            {
            	case RecordAction.raAdd:
            		child = this.fBase.SelectPerson(this.fFamily.GetHusband(), TargetMode.tmParent, GEDCOMSex.svNone);
            		result = (child != null && this.fFamily.AddChild(child));
            		break;

            	case RecordAction.raEdit:
            		result = (this.fBase.ModifyPerson(ref child));
            		break;

            	case RecordAction.raDelete:
            		result = (child != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachChildQuery)) != DialogResult.No && this.fFamily.RemoveChild(child));
            		break;

            	case RecordAction.raJump:
            		if (child != null) {
            			this.AcceptChanges();
            			this.fBase.SelectRecordByXRef(child.XRef);
            			base.Close();
            		}
            		break;
            }

            if (result) this.UpdateChildsSheet();
		}

		private void AcceptChanges()
		{
			string stat = GKData.MarriageStatus[this.edMarriageStatus.SelectedIndex].StatSign;
			this.fFamily.SetTagStringValue("_STAT", stat);

			this.fFamily.Restriction = (GEDCOMRestriction)this.cbRestriction.SelectedIndex;

			this.fFamily.SortChilds();

			this.fBase.ChangeRecord(this.fFamily);
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
                this.fBase.Host.LogWrite("TfmFamilyEdit.btnAccept_Click(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void SetTitle()
		{
			this.Text = LangMan.LS(LSID.LSID_Family) + " \"" + this.EditHusband.Text + " - " + this.EditWife.Text + "\"";
		}

		private void btnHusbandAddClick(object sender, EventArgs e)
		{
			GEDCOMIndividualRecord husband = this.fBase.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svMale);
			if (husband != null && this.fFamily.Husband.StringValue == "")
			{
				this.fFamily.AddSpouse(husband);
				this.UpdateControls();
			}
		}

		private void btnHusbandDeleteClick(object sender, EventArgs e)
		{
			if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachHusbandQuery)) != DialogResult.No)
			{
				this.fFamily.RemoveSpouse(this.fFamily.GetHusband());
				this.UpdateControls();
			}
		}

		private void btnHusbandSelClick(object sender, EventArgs e)
		{
			GEDCOMIndividualRecord spouse = this.fFamily.GetHusband();
			if (spouse != null)
			{
				this.AcceptChanges();
				this.fBase.SelectRecordByXRef(spouse.XRef);
				base.Close();
			}
		}

		private void btnWifeAddClick(object sender, EventArgs e)
		{
			GEDCOMIndividualRecord wife = this.fBase.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svFemale);
			if (wife != null && this.fFamily.Wife.StringValue == "")
			{
				this.fFamily.AddSpouse(wife);
				this.UpdateControls();
			}
		}

		private void btnWifeDeleteClick(object sender, EventArgs e)
		{
			if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachWifeQuery)) != DialogResult.No)
			{
				this.fFamily.RemoveSpouse(this.fFamily.GetWife());
				this.UpdateControls();
			}
		}

		private void btnWifeSelClick(object sender, EventArgs e)
		{
			GEDCOMIndividualRecord spouse = this.fFamily.GetWife();
			if (spouse != null)
			{
				this.AcceptChanges();
				this.fBase.SelectRecordByXRef(spouse.XRef);
				base.Close();
			}
		}

		private void EditHusband_TextChanged(object sender, EventArgs e)
		{
			this.SetTitle();
		}

		private void EditWife_TextChanged(object sender, EventArgs e)
		{
			this.SetTitle();
		}
	}
}
