using System;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKUI.Sheets;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Dialogs
{
	public partial class TfmCommunicationEdit : Form, IBaseEditor
	{
		private readonly IBase fBase;
        private readonly GKNotesSheet fNotesList;
        private readonly GKMediaSheet fMediaList;
        
        private TGEDCOMCommunicationRecord fCommunication;
		private TGEDCOMIndividualRecord fTempInd;

		public IBase Base
		{
			get { return this.fBase; }
		}

		public TGEDCOMCommunicationRecord Communication
		{
			get { return this.fCommunication; }
			set { this.SetCommunication(value); }
		}

		private void SetCommunication(TGEDCOMCommunicationRecord value)
		{
			this.fCommunication = value;
			try
			{
				if (this.fCommunication == null)
				{
					this.EditName.Text = "";
					this.EditCorrType.SelectedIndex = -1;
					this.EditDate.Text = "";
					this.EditDir.SelectedIndex = 0;
					this.EditCorresponder.Text = "";
				}
				else
				{
					this.EditName.Text = this.fCommunication.CommName;
					this.EditCorrType.SelectedIndex = (int)this.fCommunication.CommunicationType;
					this.EditDate.Text = GKUtils.GEDCOMDateToStr(this.fCommunication.Date, DateFormat.dfDD_MM_YYYY);
					TCommunicationDir dir = TCommunicationDir.cdFrom;
					this.fCommunication.GetCorresponder(ref dir, ref this.fTempInd);
					if (this.fTempInd != null)
					{
						this.EditDir.SelectedIndex = (int)dir;
						this.EditCorresponder.Text = this.fTempInd.aux_GetNameStr(true, false);
					}
					else
					{
						this.EditDir.SelectedIndex = 0;
						this.EditCorresponder.Text = "";
					}

                    this.fNotesList.DataList = this.fCommunication.Notes.GetEnumerator();
                    this.fMediaList.DataList = this.fCommunication.MultimediaLinks.GetEnumerator();
                }
            }
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmCommunicationEdit.SetCommunication(): " + ex.Message);
			}
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.fCommunication.CommName = this.EditName.Text;
				this.fCommunication.CommunicationType = (TCommunicationType)this.EditCorrType.SelectedIndex;
				this.fCommunication.Date.ParseString(GEDCOMUtils.StrToGEDCOMDate(this.EditDate.Text, true));
				this.fCommunication.SetCorresponder((TCommunicationDir)this.EditDir.SelectedIndex, this.fTempInd);
				this.fBase.ChangeRecord(this.fCommunication);
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmCommunicationEdit.Accept(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void btnPersonAdd_Click(object sender, EventArgs e)
		{
			this.fTempInd = this.fBase.SelectPerson(null, TargetMode.tmNone, TGEDCOMSex.svNone);
			this.EditCorresponder.Text = ((this.fTempInd == null) ? "" : this.fTempInd.aux_GetNameStr(true, false));
		}

		public TfmCommunicationEdit(IBase aBase)
		{
			this.InitializeComponent();
			this.fBase = aBase;

			for (TCommunicationType ct = TCommunicationType.ctCall; ct <= TCommunicationType.ctLast; ct++)
			{
				this.EditCorrType.Items.Add(LangMan.LS(GKData.CommunicationNames[(int)ct]));
			}

			this.fNotesList = new GKNotesSheet(this, this.SheetNotes);
            this.fMediaList = new GKMediaSheet(this, this.SheetMultimedia);

			this.fTempInd = null;
			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.Text = LangMan.LS(LSID.LSID_WinCommunicationEdit);
			this.SheetNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
			this.SheetMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
			this.Label1.Text = LangMan.LS(LSID.LSID_Theme);
			this.Label5.Text = LangMan.LS(LSID.LSID_Corresponder);
			this.Label2.Text = LangMan.LS(LSID.LSID_Type);
			this.Label4.Text = LangMan.LS(LSID.LSID_Date);
		}
	}
}
