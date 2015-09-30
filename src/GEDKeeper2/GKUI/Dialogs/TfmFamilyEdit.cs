using System;
using System.Windows.Forms;

using ExtUtils;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Controls;
using GKUI.Sheets;

/// <summary>
/// 
/// </summary>

namespace GKUI.Dialogs
{
	public partial class TfmFamilyEdit : Form, IBaseEditor
	{
		private readonly IBase fBase;

        private readonly GKChildsSheet fChildsList;
        private readonly GKEventsSheet fEventsList;
		private readonly GKNotesSheet fNotesList;
        private readonly GKMediaSheet fMediaList;
		private readonly GKSourcesSheet fSourcesList;

        private GEDCOMFamilyRecord fFamily;

		public IBase Base
		{
			get { return this.fBase; }
		}
        
		public GEDCOMFamilyRecord Family
		{
			get	{ return this.fFamily; }
			set	{ this.SetFamily(value); }
		}

		private void AcceptChanges()
		{
			string stat = GKData.MarriageStatus[this.EditMarriageStatus.SelectedIndex].StatSign;
			this.fFamily.SetTagStringValue("_STAT", stat);
			this.fFamily.Restriction = (GEDCOMRestriction)this.cbRestriction.SelectedIndex;
			this.fFamily.SortChilds();
			this.fBase.ChangeRecord(this.fFamily);
		}

		private GEDCOMIndividualRecord GetHusband()
		{
			return this.fFamily.Husband.Value as GEDCOMIndividualRecord;
		}

		private GEDCOMIndividualRecord GetWife()
		{
			return this.fFamily.Wife.Value as GEDCOMIndividualRecord;
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
					this.EditMarriageStatus.Enabled = false;
					this.EditMarriageStatus.SelectedIndex = 0;
					this.cbRestriction.SelectedIndex = 0;
				}
				else
				{
					string stat = this.fFamily.GetTagStringValue("_STAT");
					int statIdx = GKUtils.GetMarriageStatusIndex(stat);
					this.EditMarriageStatus.Enabled = true;
					this.EditMarriageStatus.SelectedIndex = statIdx;
					this.cbRestriction.SelectedIndex = (sbyte)this.fFamily.Restriction;
					
					this.ControlsRefresh();
				}
			}
			catch (Exception ex)
			{
                this.fBase.Host.LogWrite("TfmFamilyEdit.SetFamily(): " + ex.Message);
			}
		}

		private void ControlsRefresh()
		{
			GEDCOMIndividualRecord spouse = this.GetHusband();
			this.EditHusband.Text = (spouse != null) ? spouse.GetNameString(true, false) : LangMan.LS(LSID.LSID_UnkMale);

            this.btnHusbandAdd.Enabled = (spouse == null);
			this.btnHusbandDelete.Enabled = (spouse != null);
			this.btnHusbandSel.Enabled = (spouse != null);

			spouse = this.GetWife();
			this.EditWife.Text = (spouse != null) ? spouse.GetNameString(true, false) : LangMan.LS(LSID.LSID_UnkFemale);

            this.btnWifeAdd.Enabled = (spouse == null);
			this.btnWifeDelete.Enabled = (spouse != null);
			this.btnWifeSel.Enabled = (spouse != null);

            this.fEventsList.DataList = this.fFamily.FamilyEvents.GetEnumerator();
			this.fNotesList.DataList = this.fFamily.Notes.GetEnumerator();
		    this.fMediaList.DataList = this.fFamily.MultimediaLinks.GetEnumerator();
		    this.fSourcesList.DataList = this.fFamily.SourceCitations.GetEnumerator();
		    this.fChildsList.DataList = this.fFamily.Childrens.GetEnumerator();

			bool locked = (this.fFamily.Restriction == GEDCOMRestriction.rnLocked);
			
			this.btnHusbandAdd.Enabled = (this.btnHusbandAdd.Enabled && !locked);
			this.btnHusbandDelete.Enabled = (this.btnHusbandDelete.Enabled && !locked);
			this.btnWifeAdd.Enabled = (this.btnWifeAdd.Enabled && !locked);
			this.btnWifeDelete.Enabled = (this.btnWifeDelete.Enabled && !locked);
			this.EditMarriageStatus.Enabled = (this.EditMarriageStatus.Enabled && !locked);
			this.fChildsList.ReadOnly = locked;
			this.fEventsList.ReadOnly = locked;
			this.fNotesList.ReadOnly = locked;
			this.fMediaList.ReadOnly = locked;
			this.fSourcesList.ReadOnly = locked;
		}

		private void SetTitle()
		{
			this.Text = LangMan.LS(LSID.LSID_Family) + " \"" + this.EditHusband.Text + " - " + this.EditWife.Text + "\"";
		}

		private void ListModify(object sender, ModifyEventArgs eArgs)
		{
			if (sender == this.fChildsList && eArgs.Action == RecordAction.raJump)
			{
				GEDCOMIndividualRecord child = eArgs.ItemData as GEDCOMIndividualRecord;
				if (child != null) {
					this.AcceptChanges();
					this.fBase.SelectRecordByXRef(child.XRef);
					base.Close();
				}
			}
		}

		private void btnHusbandAddClick(object sender, EventArgs e)
		{
			GEDCOMIndividualRecord husband = this.fBase.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svMale);
			if (husband != null && this.fFamily.Husband.StringValue == "")
			{
				this.fFamily.AddSpouse(husband);
				this.ControlsRefresh();
			}
		}

		private void btnHusbandDeleteClick(object sender, EventArgs e)
		{
			if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachHusbandQuery)) != DialogResult.No)
			{
				this.fFamily.RemoveSpouse(this.GetHusband());
				this.ControlsRefresh();
			}
		}

		private void btnHusbandSelClick(object sender, EventArgs e)
		{
			GEDCOMIndividualRecord spouse = this.GetHusband();
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
				this.ControlsRefresh();
			}
		}

		private void btnWifeDeleteClick(object sender, EventArgs e)
		{
			if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachWifeQuery)) != DialogResult.No)
			{
				this.fFamily.RemoveSpouse(this.GetWife());
				this.ControlsRefresh();
			}
		}

		private void btnWifeSelClick(object sender, EventArgs e)
		{
			GEDCOMIndividualRecord spouse = this.GetWife();
			if (spouse != null)
			{
				this.AcceptChanges();
				this.fBase.SelectRecordByXRef(spouse.XRef);
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
                this.fBase.Host.LogWrite("TfmFamilyEdit.Accept(): " + ex.Message);
				base.DialogResult = DialogResult.None;
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

		private void cbRestriction_SelectedIndexChanged(object sender, EventArgs e)
		{
			this.ControlsRefresh();
		}

		public TfmFamilyEdit(IBase aBase)
		{
			this.InitializeComponent();
			this.fBase = aBase;

			for (GEDCOMRestriction res = GEDCOMRestriction.rnNone; res <= GEDCOMRestriction.rnLast; res++)
			{
				this.cbRestriction.Items.Add(GKData.Restrictions[(int)res]);
			}

			for (int i = 0; i < GKData.MarriageStatus.Length; i++)
			{
				this.EditMarriageStatus.Items.Add(LangMan.LS(GKData.MarriageStatus[i].Name));
			}

            this.fChildsList = new GKChildsSheet(this, this.SheetChilds);
			this.fChildsList.OnModify += this.ListModify;

            this.fEventsList = new GKEventsSheet(this, this.SheetEvents, false);
			this.fNotesList = new GKNotesSheet(this, this.SheetNotes);
            this.fMediaList = new GKMediaSheet(this, this.SheetMultimedia);
			this.fSourcesList = new GKSourcesSheet(this, this.SheetSources);

			this.SetLang();
		}

		public void SetLang()
		{
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
	}
}
