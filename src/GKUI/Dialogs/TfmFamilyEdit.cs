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
	public partial class TfmFamilyEdit : Form
	{
		private TfmBase FBase;
		private TGEDCOMFamilyRecord FFamily;
		private TSheetList FChildsList;
		private TSheetList FEventsList;
		private TSheetList FNotesList;
		private TSheetList FMediaList;
		private TSheetList FSourcesList;

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
			string stat = TGenEngine.MarriageStatus[this.EditMarriageStatus.SelectedIndex].StatSign;
			this.FFamily.SetTagStringValue("_STAT", stat);
			this.FFamily.Restriction = (TGEDCOMRestriction)this.cbRestriction.SelectedIndex;
			this.FFamily.SortChilds();
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

		private void SetFamily([In] TGEDCOMFamilyRecord Value)
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
					int stat_idx = TGenEngine.GetMarriageStatusIndex(stat);
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
				this.EditHusband.Text = TGenEngine.GetNameStr(spouse, true, false);
			}
			else
			{
				this.EditHusband.Text = GKL.LSList[64];
			}
			this.btnHusbandAdd.Enabled = (spouse == null);
			this.btnHusbandDelete.Enabled = (spouse != null);
			this.btnHusbandSel.Enabled = (spouse != null);
			spouse = this.GetWife();
			if (spouse != null)
			{
				this.EditWife.Text = TGenEngine.GetNameStr(spouse, true, false);
			}
			else
			{
				this.EditWife.Text = GKL.LSList[63];
			}
			this.btnWifeAdd.Enabled = (spouse == null);
			this.btnWifeDelete.Enabled = (spouse != null);
			this.btnWifeSel.Enabled = (spouse != null);
			this.Base.RecListFamilyEventsRefresh(this.FFamily, this.FEventsList.List, null);
			this.Base.RecListNotesRefresh(this.FFamily, this.FNotesList.List, null);
			this.Base.RecListMediaRefresh(this.FFamily, this.FMediaList.List, null);
			this.Base.RecListSourcesRefresh(this.FFamily, this.FSourcesList.List, null);
			TGKListView list = this.FChildsList.List;
			list.BeginUpdate();
			list.Items.Clear();

			int num = this.FFamily.Childrens.Count;
			for (int i = 1; i <= num; i++)
			{
				TGEDCOMIndividualRecord child = this.FFamily.Childrens[i - 1].Value as TGEDCOMIndividualRecord;
				ListViewItem item = list.AddItem(i.ToString(), child);
				item.SubItems.Add(TGenEngine.GetNameStr(child, true, false));
				item.SubItems.Add(TGenEngine.GetBirthDate(child, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat, false));
			}
			list.EndUpdate();

			this.btnHusbandAdd.Enabled = (this.btnHusbandAdd.Enabled && this.FFamily.Restriction != TGEDCOMRestriction.rnLocked);
			this.btnHusbandDelete.Enabled = (this.btnHusbandDelete.Enabled && this.FFamily.Restriction != TGEDCOMRestriction.rnLocked);
			this.btnWifeAdd.Enabled = (this.btnWifeAdd.Enabled && this.FFamily.Restriction != TGEDCOMRestriction.rnLocked);
			this.btnWifeDelete.Enabled = (this.btnWifeDelete.Enabled && this.FFamily.Restriction != TGEDCOMRestriction.rnLocked);
			this.EditMarriageStatus.Enabled = (this.EditMarriageStatus.Enabled && this.FFamily.Restriction != TGEDCOMRestriction.rnLocked);
			this.FChildsList.ReadOnly = (this.FFamily.Restriction == TGEDCOMRestriction.rnLocked);
			this.FEventsList.ReadOnly = (this.FFamily.Restriction == TGEDCOMRestriction.rnLocked);
			this.FNotesList.ReadOnly = (this.FFamily.Restriction == TGEDCOMRestriction.rnLocked);
			this.FMediaList.ReadOnly = (this.FFamily.Restriction == TGEDCOMRestriction.rnLocked);
			this.FSourcesList.ReadOnly = (this.FFamily.Restriction == TGEDCOMRestriction.rnLocked);
		}
		private void SetTitle()
		{
			this.Text = string.Concat(new string[]
			{
				GKL.LSList[114], 
				" \"", 
				this.EditHusband.Text, 
				" - ", 
				this.EditWife.Text, 
				"\""
			});
		}
		private void ListModify(object Sender, object ItemData, TGenEngine.TRecAction Action)
		{
			if (object.Equals(Sender, this.FChildsList))
			{
				if (Action != TGenEngine.TRecAction.raAdd)
				{
					if (Action != TGenEngine.TRecAction.raEdit)
					{
						if (Action != TGenEngine.TRecAction.raDelete)
						{
							if (Action == TGenEngine.TRecAction.raJump)
							{
								TGEDCOMIndividualRecord child = ItemData as TGEDCOMIndividualRecord;
								if (child != null)
								{
									this.AcceptChanges();
									this.Base.SelectRecordByXRef(child.XRef);
									base.Close();
								}
							}
						}
						else
						{
							TGEDCOMIndividualRecord child = ItemData as TGEDCOMIndividualRecord;
							if (child != null && SysUtils.ShowQuestion(GKL.LSList[121]) != DialogResult.No && this.Base.Engine.RemoveFamilyChild(this.FFamily, child))
							{
								this.ControlsRefresh();
							}
						}
					}
					else
					{
						TGEDCOMIndividualRecord child = ItemData as TGEDCOMIndividualRecord;
						if (this.Base.ModifyPerson(ref child))
						{
							this.ControlsRefresh();
						}
					}
				}
				else
				{
					TGEDCOMIndividualRecord child = this.Base.SelectPerson(this.GetHusband(), TGenEngine.TTargetMode.tmParent, TGEDCOMSex.svNone);
					if (child != null && this.Base.Engine.AddFamilyChild(this.FFamily, child))
					{
						this.ControlsRefresh();
					}
				}
			}
			else
			{
				if (object.Equals(Sender, this.FEventsList))
				{
					if (this.Base.ModifyRecEvent(this, this.FFamily, ItemData as TGEDCOMCustomEvent, Action))
					{
						this.ControlsRefresh();
					}
				}
				else
				{
					if (object.Equals(Sender, this.FNotesList))
					{
						if (this.Base.ModifyRecNote(this, this.FFamily, ItemData as TGEDCOMNotes, Action))
						{
							this.ControlsRefresh();
						}
					}
					else
					{
						if (object.Equals(Sender, this.FMediaList))
						{
							if (this.Base.ModifyRecMultimedia(this, this.FFamily, ItemData as TGEDCOMMultimediaLink, Action))
							{
								this.ControlsRefresh();
							}
						}
						else
						{
							if (object.Equals(Sender, this.FSourcesList) && this.Base.ModifyRecSource(this, this.FFamily, ItemData as TGEDCOMSourceCitation, Action))
							{
								this.ControlsRefresh();
							}
						}
					}
				}
			}
		}

		private void btnHusbandAddClick(object sender, EventArgs e)
		{
			TGEDCOMIndividualRecord husband = this.Base.SelectPerson(null, TGenEngine.TTargetMode.tmNone, TGEDCOMSex.svMale);
			if (husband != null && this.FFamily.Husband.StringValue == "")
			{
				this.Base.Engine.AddFamilySpouse(this.FFamily, husband);
				this.ControlsRefresh();
			}
		}

		private void btnHusbandDeleteClick(object sender, EventArgs e)
		{
			if (SysUtils.ShowQuestion(GKL.LSList[119]) != DialogResult.No)
			{
				this.Base.Engine.RemoveFamilySpouse(this.FFamily, this.GetHusband());
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
			TGEDCOMIndividualRecord wife = this.Base.SelectPerson(null, TGenEngine.TTargetMode.tmNone, TGEDCOMSex.svFemale);
			if (wife != null && this.FFamily.Wife.StringValue == "")
			{
				this.Base.Engine.AddFamilySpouse(this.FFamily, wife);
				this.ControlsRefresh();
			}
		}

		private void btnWifeDeleteClick(object sender, EventArgs e)
		{
			if (SysUtils.ShowQuestion(GKL.LSList[120]) != DialogResult.No)
			{
				this.Base.Engine.RemoveFamilySpouse(this.FFamily, this.GetWife());
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
			TGEDCOMRestriction res = TGEDCOMRestriction.rnNone;
			do
			{
				this.cbRestriction.Items.Add(TGenEngine.Restrictions[(int)res]);
				res++;
			}
			while (res != (TGEDCOMRestriction)4);
			int i = 0;
			do
			{
				this.EditMarriageStatus.Items.Add(GKL.LSList[(int)TGenEngine.MarriageStatus[i].Name - 1]);
				i++;
			}
			while (i != 4);

			this.FChildsList = new TSheetList(this.SheetChilds);
			this.FChildsList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.FChildsList.Buttons = TEnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbJump
			});
			this.FChildsList.List.AddListColumn("№", 25, false);
			this.FChildsList.List.AddListColumn(GKL.LSList[85], 300, false);
			this.FChildsList.List.AddListColumn(GKL.LSList[122], 100, false);

			this.FEventsList = new TSheetList(this.SheetEvents);
			this.FEventsList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecEventsList(this.FEventsList, false);

			this.FNotesList = new TSheetList(this.SheetNotes);
			this.FNotesList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecNotesList(this.FNotesList);

			this.FMediaList = new TSheetList(this.SheetMultimedia);
			this.FMediaList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecMediaList(this.FMediaList);

			this.FSourcesList = new TSheetList(this.SheetSources);
			this.FSourcesList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecSourcesList(this.FSourcesList);

			this.SetLang();
		}

		public void SetLang()
		{
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.GroupBox1.Text = GKL.LSList[114];
			this.Label1.Text = GKL.LSList[115];
			this.Label2.Text = GKL.LSList[116];
			this.Label6.Text = GKL.LSList[117];
			this.SheetChilds.Text = GKL.LSList[118];
			this.SheetEvents.Text = GKL.LSList[83];
			this.SheetNotes.Text = GKL.LSList[54];
			this.SheetMultimedia.Text = GKL.LSList[55];
			this.SheetSources.Text = GKL.LSList[56];
			this.Label5.Text = GKL.LSList[124];
		}
	}
}
