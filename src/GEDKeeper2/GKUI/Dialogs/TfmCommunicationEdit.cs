using System;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Sheets;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TfmCommunicationEdit : Form, IBaseEditor
	{
		private readonly IBaseWindow fBase;
        private readonly GKNotesSheet fNotesList;
        private readonly GKMediaSheet fMediaList;
        
        private GEDCOMCommunicationRecord fCommunication;
		private GEDCOMIndividualRecord fTempInd;

		public IBaseWindow Base
		{
			get { return this.fBase; }
		}

		public GEDCOMCommunicationRecord Communication
		{
			get { return this.fCommunication; }
			set { this.SetCommunication(value); }
		}

		private void SetCommunication(GEDCOMCommunicationRecord value)
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
					GKCommunicationDir dir = GKCommunicationDir.cdFrom;
					this.fCommunication.GetCorresponder(ref dir, ref this.fTempInd);
					if (this.fTempInd != null)
					{
						this.EditDir.SelectedIndex = (int)dir;
						this.EditCorresponder.Text = this.fTempInd.GetNameString(true, false);
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
				this.fCommunication.CommunicationType = (GKCommunicationType)this.EditCorrType.SelectedIndex;
				this.fCommunication.Date.ParseString(GEDCOMUtils.StrToGEDCOMDate(this.EditDate.Text, true));
				this.fCommunication.SetCorresponder((GKCommunicationDir)this.EditDir.SelectedIndex, this.fTempInd);
				this.fBase.ChangeRecord(this.fCommunication);
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmCommunicationEdit.btnAccept_Click(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void btnPersonAdd_Click(object sender, EventArgs e)
		{
			this.fTempInd = this.fBase.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svNone);
			this.EditCorresponder.Text = ((this.fTempInd == null) ? "" : this.fTempInd.GetNameString(true, false));
		}

		public TfmCommunicationEdit(IBaseWindow aBase)
		{
			this.InitializeComponent();

			this.fBase = aBase;
			this.fTempInd = null;

			for (GKCommunicationType ct = GKCommunicationType.ctCall; ct <= GKCommunicationType.ctLast; ct++)
			{
				this.EditCorrType.Items.Add(LangMan.LS(GKData.CommunicationNames[(int)ct]));
			}

			this.fNotesList = new GKNotesSheet(this, this.SheetNotes);
            this.fMediaList = new GKMediaSheet(this, this.SheetMultimedia);

			// SetLang()
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
