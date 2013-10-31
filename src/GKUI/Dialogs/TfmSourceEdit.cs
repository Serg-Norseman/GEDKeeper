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
	public partial class TfmSourceEdit : Form
	{
		private TfmBase FBase;
		private TGEDCOMSourceRecord FSourceRecord;
		private GKSheetList FNotesList;
		private GKSheetList FMediaList;
		private GKSheetList FRepositoriesList;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		public TGEDCOMSourceRecord SourceRecord
		{
			get { return this.FSourceRecord; }
			set { this.SetSourceRecord(value); }
		}

		private void AcceptChanges()
		{
			this.FSourceRecord.FiledByEntry = this.EditShortTitle.Text;
			this.FSourceRecord.Originator.Clear();
			this.FSourceRecord.SetOriginatorArray(this.EditAuthor.Lines);
			this.FSourceRecord.Title.Clear();
			this.FSourceRecord.SetTitleArray(this.EditTitle.Lines);
			this.FSourceRecord.Publication.Clear();
			this.FSourceRecord.SetPublicationArray(this.EditPublication.Lines);
			this.FSourceRecord.Text.Clear();
			this.FSourceRecord.SetTextArray(this.EditText.Lines);
			this.Base.ChangeRecord(this.FSourceRecord);
		}

		private void ControlsRefresh()
		{
			this.Base.RecListNotesRefresh(this.FSourceRecord, this.FNotesList.List, null);
			this.Base.RecListMediaRefresh(this.FSourceRecord, this.FMediaList.List, null);

			GKListView list = this.FRepositoriesList.List;
			list.BeginUpdate();
			list.Items.Clear();

			int num = this.FSourceRecord.RepositoryCitations.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMRepositoryRecord rep = this.FSourceRecord.RepositoryCitations[i].Value as TGEDCOMRepositoryRecord;
				list.AddItem(rep.RepositoryName, this.FSourceRecord.RepositoryCitations[i]);
			}
			list.EndUpdate();
		}

		private void ListModify(object sender, ModifyEventArgs eArgs)
		{
            bool res = false;

            if (sender == this.FNotesList)
            {
            	res = (this.Base.ModifyRecNote(this, this.FSourceRecord, eArgs.ItemData as TGEDCOMNotes, eArgs.Action));
            }
            else if (sender == this.FMediaList)
            {
            	res = (this.Base.ModifyRecMultimedia(this, this.FSourceRecord, eArgs.ItemData as TGEDCOMMultimediaLink, eArgs.Action));
            }
            else if (sender == this.FRepositoriesList)
            {
            	TGEDCOMRepositoryCitation cit = eArgs.ItemData as TGEDCOMRepositoryCitation;

            	switch (eArgs.Action)
            	{
            		case TRecAction.raAdd:
            			TGEDCOMRepositoryRecord rep = FBase.SelectRecord(TGEDCOMRecordType.rtRepository, null) as TGEDCOMRepositoryRecord;
            			if (rep != null)
            			{
            				this.FSourceRecord.aux_AddRepository(rep);
            				res = true;
            			}
            			break;

            		case TRecAction.raDelete:
            			if (cit != null && GKUtils.ShowQuestion(LangMan.LSList[145]) != DialogResult.No)
            			{
            				this.FSourceRecord.RepositoryCitations.DeleteObject(cit);
            				res = true;
            			}
            			break;

            		case TRecAction.raJump:
            			if (cit != null)
            			{
            				this.AcceptChanges();
            				this.Base.SelectRecordByXRef((cit.Value as TGEDCOMRepositoryRecord).XRef);
            				base.Close();
            			}
            			break;
            	}
            }

            if (res) this.ControlsRefresh();
		}

		private void SetSourceRecord(TGEDCOMSourceRecord Value)
		{
			this.FSourceRecord = Value;
			this.EditShortTitle.Text = this.FSourceRecord.FiledByEntry;
			this.EditAuthor.Text = this.FSourceRecord.Originator.Text.Trim();
			this.EditTitle.Text = this.FSourceRecord.Title.Text.Trim();
			this.EditPublication.Text = this.FSourceRecord.Publication.Text.Trim();
			this.EditText.Text = this.FSourceRecord.Text.Text.Trim();
			this.ControlsRefresh();
			this.ActiveControl = this.EditShortTitle;
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
				SysUtils.LogWrite("TfmSourceEdit.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void EditShortTitle_TextChanged(object sender, EventArgs e)
		{
			this.Text = LangMan.LSList[109] + " \"" + this.EditShortTitle.Text + "\"";
		}

		public TfmSourceEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;

			this.FNotesList = new GKSheetList(this.SheetNotes);
			this.FNotesList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.Base.SetupRecNotesList(this.FNotesList);

			this.FMediaList = new GKSheetList(this.SheetMultimedia);
			this.FMediaList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.Base.SetupRecMediaList(this.FMediaList);

			this.FRepositoriesList = new GKSheetList(this.SheetRepositories);
			this.FRepositoriesList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.FRepositoriesList.Buttons = EnumSet.Create(new Enum[]
			{
				GKSheetList.TListButton.lbAdd, 
				GKSheetList.TListButton.lbEdit, 
				GKSheetList.TListButton.lbDelete, 
				GKSheetList.TListButton.lbJump
			});
			this.FRepositoriesList.List.AddListColumn(LangMan.LSList[134], 300, false);
			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.Label1.Text = LangMan.LSList[141];
			this.Label3.Text = LangMan.LSList[142];
			this.Label2.Text = LangMan.LSList[125];
			this.Label4.Text = LangMan.LSList[143];
			this.SheetCommon.Text = LangMan.LSList[144];
			this.SheetText.Text = LangMan.LSList[140];
			this.SheetRepositories.Text = LangMan.LSList[57];
			this.SheetNotes.Text = LangMan.LSList[54];
			this.SheetMultimedia.Text = LangMan.LSList[55];
		}
	}
}
