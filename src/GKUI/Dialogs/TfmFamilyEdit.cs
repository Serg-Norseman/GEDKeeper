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
	public partial class TfmFamilyEdit : Form
	{
		private TfmBase FBase;
		private TGEDCOMFamilyRecord FFamily;
		private GKSheetList FChildsList;
		private GKSheetList FEventsList;
		private GKSheetList FNotesList;
		private GKSheetList FMediaList;
		private GKSheetList FSourcesList;

		public TfmBase Base
		{
			get	{ return this.FBase; }
		}

		public TGEDCOMFamilyRecord Family
		{
			get	{ return this.FFamily; }
			set	{ this.SetFamily(value); }
		}

		private void AcceptChanges()
		{
			string stat = GKData.MarriageStatus[this.EditMarriageStatus.SelectedIndex].StatSign;
			this.FFamily.SetTagStringValue("_STAT", stat);
			this.FFamily.Restriction = (TGEDCOMRestriction)this.cbRestriction.SelectedIndex;
			this.FFamily.aux_SortChilds();
			this.Base.ChangeRecord(this.FFamily);
		}

		private TGEDCOMIndividualRecord GetHusband()
		{
			return this.FFamily.Husband.Value as TGEDCOMIndividualRecord;
		}

		private TGEDCOMIndividualRecord GetWife()
		{
			return this.FFamily.Wife.Value as TGEDCOMIndividualRecord;
		}

		private void SetFamily(TGEDCOMFamilyRecord Value)
		{
			this.FFamily = Value;
			try
			{
				if (this.FFamily == null)
				{
					this.btnHusbandSel.Enabled = false;
					this.btnWifeSel.Enabled = false;
					this.EditMarriageStatus.Enabled = false;
					this.EditMarriageStatus.SelectedIndex = 0;
					this.cbRestriction.SelectedIndex = 0;
				}
				else
				{
					string stat = this.FFamily.GetTagStringValue("_STAT");
					int stat_idx = GKUtils.GetMarriageStatusIndex(stat);
					this.EditMarriageStatus.Enabled = true;
					this.EditMarriageStatus.SelectedIndex = stat_idx;
					this.cbRestriction.SelectedIndex = (int)((sbyte)this.FFamily.Restriction);
					this.ControlsRefresh();
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("FamilyEdit.SetFamily(): " + E.Message);
			}
		}

		private void ControlsRefresh()
		{
			TGEDCOMIndividualRecord spouse = this.GetHusband();
			if (spouse != null)
			{
				this.EditHusband.Text = spouse.aux_GetNameStr(true, false);
			}
			else
			{
				this.EditHusband.Text = LangMan.LSList[64];
			}
			this.btnHusbandAdd.Enabled = (spouse == null);
			this.btnHusbandDelete.Enabled = (spouse != null);
			this.btnHusbandSel.Enabled = (spouse != null);

			spouse = this.GetWife();
			if (spouse != null)
			{
				this.EditWife.Text = spouse.aux_GetNameStr(true, false);
			}
			else
			{
				this.EditWife.Text = LangMan.LSList[63];
			}
			this.btnWifeAdd.Enabled = (spouse == null);
			this.btnWifeDelete.Enabled = (spouse != null);
			this.btnWifeSel.Enabled = (spouse != null);

			this.Base.RecListFamilyEventsRefresh(this.FFamily, this.FEventsList.List, null);
			this.Base.RecListNotesRefresh(this.FFamily, this.FNotesList.List, null);
			this.Base.RecListMediaRefresh(this.FFamily, this.FMediaList.List, null);
			this.Base.RecListSourcesRefresh(this.FFamily, this.FSourcesList.List, null);

			GKListView list = this.FChildsList.List;
			list.SwitchSorter();
			list.BeginUpdate();
			list.Items.Clear();
			int num = this.FFamily.Childrens.Count;
			for (int i = 1; i <= num; i++)
			{
				TGEDCOMIndividualRecord child = this.FFamily.Childrens[i - 1].Value as TGEDCOMIndividualRecord;
				ListViewItem item = list.AddItem(i.ToString(), child);
				item.SubItems.Add(child.aux_GetNameStr(true, false));
				item.SubItems.Add(GKUtils.GetBirthDate(child, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat, false));
			}
			list.EndUpdate();
			list.SwitchSorter();

			bool locked = (this.FFamily.Restriction == TGEDCOMRestriction.rnLocked);
			
			this.btnHusbandAdd.Enabled = (this.btnHusbandAdd.Enabled && !locked);
			this.btnHusbandDelete.Enabled = (this.btnHusbandDelete.Enabled && !locked);
			this.btnWifeAdd.Enabled = (this.btnWifeAdd.Enabled && !locked);
			this.btnWifeDelete.Enabled = (this.btnWifeDelete.Enabled && !locked);
			this.EditMarriageStatus.Enabled = (this.EditMarriageStatus.Enabled && !locked);
			this.FChildsList.ReadOnly = locked;
			this.FEventsList.ReadOnly = locked;
			this.FNotesList.ReadOnly = locked;
			this.FMediaList.ReadOnly = locked;
			this.FSourcesList.ReadOnly = locked;
		}

		private void SetTitle()
		{
			this.Text = LangMan.LSList[114] + " \"" + this.EditHusband.Text + " - " + this.EditWife.Text + "\"";
		}

		private void ListModify(object sender, ModifyEventArgs eArgs)
		{
			bool res = false;

			if (sender == this.FChildsList)
			{
				TGEDCOMIndividualRecord child = eArgs.ItemData as TGEDCOMIndividualRecord;

				switch (eArgs.Action)
				{
					case TRecAction.raAdd:
						child = this.Base.SelectPerson(this.GetHusband(), TTargetMode.tmParent, TGEDCOMSex.svNone);
						res = (child != null && this.FFamily.aux_AddChild(child));
						break;

					case TRecAction.raEdit:
						res = (this.Base.ModifyPerson(ref child));
						break;

					case TRecAction.raDelete:
						res = (child != null && GKUtils.ShowQuestion(LangMan.LSList[121]) != DialogResult.No && this.FFamily.aux_RemoveChild(child));
						break;

					case TRecAction.raJump:
						if (child != null)
						{
							this.AcceptChanges();
							this.Base.SelectRecordByXRef(child.XRef);
							base.Close();
						}
						break;
				}
			}
			else if (sender == this.FEventsList)
			{
				TGEDCOMCustomEvent evt = eArgs.ItemData as TGEDCOMCustomEvent;
				res = (this.Base.ModifyRecEvent(this, this.FFamily, ref evt, eArgs.Action));
				if (res && eArgs.Action == TRecAction.raAdd) eArgs.ItemData = evt;
			}
			else if (sender == this.FNotesList)
			{
				res = (this.Base.ModifyRecNote(this, this.FFamily, eArgs.ItemData as TGEDCOMNotes, eArgs.Action));
			}
			else if (sender == this.FMediaList)
			{
				res = (this.Base.ModifyRecMultimedia(this, this.FFamily, eArgs.ItemData as TGEDCOMMultimediaLink, eArgs.Action));
			}
			else if (sender == this.FSourcesList)
			{
				res = (this.Base.ModifyRecSource(this, this.FFamily, eArgs.ItemData as TGEDCOMSourceCitation, eArgs.Action));
			}

			if (res) this.ControlsRefresh();
		}

		private void btnHusbandAddClick(object sender, EventArgs e)
		{
			TGEDCOMIndividualRecord husband = this.Base.SelectPerson(null, TTargetMode.tmNone, TGEDCOMSex.svMale);
			if (husband != null && this.FFamily.Husband.StringValue == "")
			{
				this.FFamily.aux_AddSpouse(husband);
				this.ControlsRefresh();
			}
		}

		private void btnHusbandDeleteClick(object sender, EventArgs e)
		{
			if (GKUtils.ShowQuestion(LangMan.LSList[119]) != DialogResult.No)
			{
				this.FFamily.aux_RemoveSpouse(this.GetHusband());
				this.ControlsRefresh();
			}
		}

		private void btnHusbandSelClick(object sender, EventArgs e)
		{
			TGEDCOMIndividualRecord spouse = this.GetHusband();
			if (spouse != null)
			{
				this.AcceptChanges();
				this.Base.SelectRecordByXRef(spouse.XRef);
				base.Close();
			}
		}

		private void btnWifeAddClick(object sender, EventArgs e)
		{
			TGEDCOMIndividualRecord wife = this.Base.SelectPerson(null, TTargetMode.tmNone, TGEDCOMSex.svFemale);
			if (wife != null && this.FFamily.Wife.StringValue == "")
			{
				this.FFamily.aux_AddSpouse(wife);
				this.ControlsRefresh();
			}
		}

		private void btnWifeDeleteClick(object sender, EventArgs e)
		{
			if (GKUtils.ShowQuestion(LangMan.LSList[120]) != DialogResult.No)
			{
				this.FFamily.aux_RemoveSpouse(this.GetWife());
				this.ControlsRefresh();
			}
		}

		private void btnWifeSelClick(object sender, EventArgs e)
		{
			TGEDCOMIndividualRecord spouse = this.GetWife();
			if (spouse != null)
			{
				this.AcceptChanges();
				this.Base.SelectRecordByXRef(spouse.XRef);
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
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmFamilyEdit.Accept(): " + E.Message);
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

		public TfmFamilyEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;

			for (TGEDCOMRestriction res = TGEDCOMRestriction.rnNone; res <= TGEDCOMRestriction.rnLast; res++)
			{
				this.cbRestriction.Items.Add(GKData.Restrictions[(int)res]);
			}

			for (int i = 0; i < GKData.MarriageStatus.Length; i++)
			{
				this.EditMarriageStatus.Items.Add(LangMan.LSList[(int)GKData.MarriageStatus[i].Name - 1]);
			}

			this.FChildsList = new GKSheetList(this.SheetChilds);
			this.FChildsList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.FChildsList.Buttons = EnumSet.Create(new Enum[]
			{
				GKSheetList.TListButton.lbAdd, 
				GKSheetList.TListButton.lbEdit, 
				GKSheetList.TListButton.lbDelete, 
				GKSheetList.TListButton.lbJump
			});
			this.FChildsList.List.AddListColumn("№", 25, false);
			this.FChildsList.List.AddListColumn(LangMan.LSList[85], 300, false);
			this.FChildsList.List.AddListColumn(LangMan.LSList[122], 100, false);

			this.FEventsList = new GKSheetList(this.SheetEvents);
			this.FEventsList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.Base.SetupRecEventsList(this.FEventsList, false);

			this.FNotesList = new GKSheetList(this.SheetNotes);
			this.FNotesList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.Base.SetupRecNotesList(this.FNotesList);

			this.FMediaList = new GKSheetList(this.SheetMultimedia);
			this.FMediaList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.Base.SetupRecMediaList(this.FMediaList);

			this.FSourcesList = new GKSheetList(this.SheetSources);
			this.FSourcesList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.Base.SetupRecSourcesList(this.FSourcesList);

			this.SetLang();
		}

		public void SetLang()
		{
			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.GroupBox1.Text = LangMan.LSList[114];
			this.Label1.Text = LangMan.LSList[115];
			this.Label2.Text = LangMan.LSList[116];
			this.Label6.Text = LangMan.LSList[117];
			this.SheetChilds.Text = LangMan.LSList[118];
			this.SheetEvents.Text = LangMan.LSList[83];
			this.SheetNotes.Text = LangMan.LSList[54];
			this.SheetMultimedia.Text = LangMan.LSList[55];
			this.SheetSources.Text = LangMan.LSList[56];
			this.Label5.Text = LangMan.LSList[124];
		}
	}
}
