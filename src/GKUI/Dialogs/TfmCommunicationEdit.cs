using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKSys;
using GKUI.Lists;

/// <summary>
/// Localization: unknown
/// </summary>

namespace GKUI
{
	public partial class TfmCommunicationEdit : Form
	{
		private TfmBase FBase;
		private TGEDCOMCommunicationRecord FCommunication;
		private TGEDCOMIndividualRecord FTempInd;
		private TSheetList FNotesList;
		private TSheetList FMediaList;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		public TGEDCOMCommunicationRecord Communication
		{
			get { return this.FCommunication; }
			set { this.SetCommunication(value); }
		}

		private void ListModify(object Sender, object ItemData, TGenEngine.TRecAction Action)
		{
			if (object.Equals(Sender, this.FNotesList))
			{
				if (this.Base.ModifyRecNote(this, this.FCommunication, ItemData as TGEDCOMNotes, Action))
				{
					this.ListsRefresh();
				}
			}
			else
			{
				if (object.Equals(Sender, this.FMediaList) && this.Base.ModifyRecMultimedia(this, this.FCommunication, ItemData as TGEDCOMMultimediaLink, Action))
				{
					this.ListsRefresh();
				}
			}
		}

		private void ListsRefresh()
		{
			this.Base.RecListNotesRefresh(this.FCommunication, this.FNotesList.List, null);
			this.Base.RecListMediaRefresh(this.FCommunication, this.FMediaList.List, null);
		}

		private void SetCommunication([In] TGEDCOMCommunicationRecord Value)
		{
			this.FCommunication = Value;
			try
			{
				if (this.FCommunication == null)
				{
					this.EditName.Text = "";
					this.EditCorrType.SelectedIndex = -1;
					this.EditDate.Text = "";
					this.EditDir.SelectedIndex = 0;
					this.EditCorresponder.Text = "";
				}
				else
				{
					this.EditName.Text = this.FCommunication.CommName;
					this.EditCorrType.SelectedIndex = (int)this.FCommunication.CommunicationType;
					this.EditDate.Text = TGenEngine.GEDCOMDateToStr(this.FCommunication.Date, TGenEngine.TDateFormat.dfDD_MM_YYYY);
					TCommunicationDir dir = TCommunicationDir.cdFrom;
					this.FCommunication.GetCorresponder(ref dir, ref this.FTempInd);
					if (this.FTempInd != null)
					{
						this.EditDir.SelectedIndex = (int)dir;
						this.EditCorresponder.Text = TGenEngine.GetNameStr(this.FTempInd, true, false);
					}
					else
					{
						this.EditDir.SelectedIndex = 0;
						this.EditCorresponder.Text = "";
					}
				}
				this.ListsRefresh();
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("CommunicationEdit.SetCommunication(): " + E.Message);
			}
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.FCommunication.CommName = this.EditName.Text;
				this.FCommunication.CommunicationType = (TCommunicationType)this.EditCorrType.SelectedIndex;
				this.FCommunication.Date.ParseString(TGenEngine.StrToGEDCOMDate(this.EditDate.Text, true));
				this.FCommunication.SetCorresponder((TCommunicationDir)this.EditDir.SelectedIndex, this.FTempInd);
				this.Base.ChangeRecord(this.FCommunication);
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmCommunicationEdit.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void btnPersonAdd_Click(object sender, EventArgs e)
		{
			this.FTempInd = this.Base.SelectPerson(null, TGenEngine.TTargetMode.tmNone, TGEDCOMSex.svNone);
			this.EditCorresponder.Text = TGenEngine.GetNameStr(this.FTempInd, true, false);
		}

		public TfmCommunicationEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			TCommunicationType ct = TCommunicationType.ctCall;
			do
			{
				this.EditCorrType.Items.Add(LangMan.LSList[(int)TGenEngine.CommunicationNames[(int)ct] - 1]);
				ct++;
			}
			while (ct != (TCommunicationType)6);

			this.FNotesList = new TSheetList(this.SheetNotes);
			this.FNotesList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecNotesList(this.FNotesList);

			this.FMediaList = new TSheetList(this.SheetMultimedia);
			this.FMediaList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecMediaList(this.FMediaList);

			this.FTempInd = null;
			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.Text = LangMan.LSList[191];
			this.SheetNotes.Text = LangMan.LSList[54];
			this.SheetMultimedia.Text = LangMan.LSList[55];
			this.Label1.Text = LangMan.LSList[183];
			this.Label5.Text = LangMan.LSList[184];
			this.Label2.Text = LangMan.LSList[113];
			this.Label4.Text = LangMan.LSList[139];
		}
	}
}
