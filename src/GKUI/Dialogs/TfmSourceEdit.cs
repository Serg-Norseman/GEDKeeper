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
	public partial class TfmSourceEdit : Form
	{
		private TfmBase FBase;
		private TGEDCOMSourceRecord FSourceRecord;
		private TSheetList FNotesList;
		private TSheetList FMediaList;
		private TSheetList FRepositoriesList;

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

			TGKListView list = this.FRepositoriesList.List;
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

		private void ListModify(object Sender, object ItemData, TGenEngine.TRecAction Action)
		{
			if (object.Equals(Sender, this.FNotesList))
			{
				if (this.Base.ModifyRecNote(this, this.FSourceRecord, ItemData as TGEDCOMNotes, Action))
				{
					this.ControlsRefresh();
				}
			}
			else
			{
				if (object.Equals(Sender, this.FMediaList))
				{
					if (this.Base.ModifyRecMultimedia(this, this.FSourceRecord, ItemData as TGEDCOMMultimediaLink, Action))
					{
						this.ControlsRefresh();
					}
				}
				else
				{
					if (object.Equals(Sender, this.FRepositoriesList))
					{
						if (Action != TGenEngine.TRecAction.raAdd)
						{
							if (Action != TGenEngine.TRecAction.raDelete)
							{
								if (Action == TGenEngine.TRecAction.raJump)
								{
									TGEDCOMRepositoryCitation cit = ItemData as TGEDCOMRepositoryCitation;
									if (cit != null)
									{
										this.AcceptChanges();
										this.Base.SelectRecordByXRef((cit.Value as TGEDCOMRepositoryRecord).XRef);
										base.Close();
									}
								}
							}
							else
							{
								TGEDCOMRepositoryCitation cit = ItemData as TGEDCOMRepositoryCitation;
								if (cit != null && SysUtils.ShowQuestion(GKL.LSList[145]) != DialogResult.No)
								{
									this.FSourceRecord.RepositoryCitations.DeleteObject(cit);
									this.ControlsRefresh();
								}
							}
						}
						else
						{
							object[] anArgs = new object[0];
							TGEDCOMRepositoryRecord rep = FBase.SelectRecord(TGEDCOMRecordType.rtRepository, anArgs) as TGEDCOMRepositoryRecord;
							if (rep != null)
							{
								TGenEngine.BindSourceRepository(this.Base.Tree, this.FSourceRecord, rep);
								this.ControlsRefresh();
							}
						}
					}
				}
			}
		}

		private void SetSourceRecord([In] TGEDCOMSourceRecord Value)
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
			this.Text = GKL.LSList[109] + " \"" + this.EditShortTitle.Text + "\"";
		}

		public TfmSourceEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;

			this.FNotesList = new TSheetList(this.SheetNotes);
			this.FNotesList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecNotesList(this.FNotesList);

			this.FMediaList = new TSheetList(this.SheetMultimedia);
			this.FMediaList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecMediaList(this.FMediaList);

			this.FRepositoriesList = new TSheetList(this.SheetRepositories);
			this.FRepositoriesList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.FRepositoriesList.Buttons = TEnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete, 
				TSheetList.TListButton.lbJump
			});
			this.FRepositoriesList.List.AddListColumn(GKL.LSList[134], 300, false);
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.Label1.Text = GKL.LSList[141];
			this.Label3.Text = GKL.LSList[142];
			this.Label2.Text = GKL.LSList[125];
			this.Label4.Text = GKL.LSList[143];
			this.SheetCommon.Text = GKL.LSList[144];
			this.SheetText.Text = GKL.LSList[140];
			this.SheetRepositories.Text = GKL.LSList[57];
			this.SheetNotes.Text = GKL.LSList[54];
			this.SheetMultimedia.Text = GKL.LSList[55];
		}
	}
}
