using GedCom551;
using GKCore;
using GKUI.Controls;
using GKUI.Lists;
using GKSys;
using System;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmSourceEdit : Form
	{
		private Button btnAccept;
		private Button btnCancel;
		private TabControl PagesData;
		private TabPage SheetNotes;
		private TabPage SheetMultimedia;
		private TabPage SheetRepositories;
		private TabPage SheetText;
		private TextBox EditText;
		private TabPage SheetCommon;
		private Label Label1;
		private TextBox EditShortTitle;
		private Label Label3;
		private TextBox EditAuthor;
		private Label Label2;
		private TextBox EditTitle;
		private Label Label4;
		private TextBox EditPublication;
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
			int arg_66_0 = 0;
			int num = this.FSourceRecord.GetRepositoryCitationsCount() - 1;
			int i = arg_66_0;
			if (num >= i)
			{
				num++;
				do
				{
					TGEDCOMRepositoryRecord rep = this.FSourceRecord.GetRepositoryCitation(i).Value as TGEDCOMRepositoryRecord;
					list.AddItem(rep.RepositoryName, this.FSourceRecord.GetRepositoryCitation(i));
					i++;
				}
				while (i != num);
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
								if (cit != null && TGKSys.ShowQuestion(GKL.LSList[145]) != DialogResult.No)
								{
									this.FSourceRecord.DeleteRepositoryCitation(cit);
									this.ControlsRefresh();
								}
							}
						}
						else
						{
							TfmBase arg_9E_0 = this.Base;
							TGEDCOMRecord.TGEDCOMRecordType arg_9E_1 = TGEDCOMRecord.TGEDCOMRecordType.rtRepository;
							object[] anArgs = new object[0];
							TGEDCOMRepositoryRecord rep = arg_9E_0.SelectRecord(arg_9E_1, anArgs) as TGEDCOMRepositoryRecord;
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
		}
		private void InitializeComponent()
		{
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.PagesData = new TabControl();
			this.SheetCommon = new TabPage();
			this.Label1 = new Label();
			this.Label3 = new Label();
			this.Label2 = new Label();
			this.Label4 = new Label();
			this.EditShortTitle = new TextBox();
			this.EditAuthor = new TextBox();
			this.EditTitle = new TextBox();
			this.EditPublication = new TextBox();
			this.SheetText = new TabPage();
			this.EditText = new TextBox();
			this.SheetRepositories = new TabPage();
			this.SheetNotes = new TabPage();
			this.SheetMultimedia = new TabPage();
			this.PagesData.SuspendLayout();
			this.SheetCommon.SuspendLayout();
			this.SheetText.SuspendLayout();
			base.SuspendLayout();
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(360, 416);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 1;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(448, 416);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 2;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			this.PagesData.Controls.Add(this.SheetCommon);
			this.PagesData.Controls.Add(this.SheetText);
			this.PagesData.Controls.Add(this.SheetRepositories);
			this.PagesData.Controls.Add(this.SheetNotes);
			this.PagesData.Controls.Add(this.SheetMultimedia);
			this.PagesData.Dock = DockStyle.Top;
			this.PagesData.Location = new Point(0, 0);
			this.PagesData.Name = "PagesData";
			this.PagesData.SelectedIndex = 0;
			this.PagesData.Size = new Size(537, 401);
			this.PagesData.TabIndex = 0;
			this.SheetCommon.Controls.Add(this.Label1);
			this.SheetCommon.Controls.Add(this.Label3);
			this.SheetCommon.Controls.Add(this.Label2);
			this.SheetCommon.Controls.Add(this.Label4);
			this.SheetCommon.Controls.Add(this.EditShortTitle);
			this.SheetCommon.Controls.Add(this.EditAuthor);
			this.SheetCommon.Controls.Add(this.EditTitle);
			this.SheetCommon.Controls.Add(this.EditPublication);
			this.SheetCommon.Location = new Point(4, 22);
			this.SheetCommon.Name = "SheetCommon";
			this.SheetCommon.Size = new Size(529, 375);
			this.SheetCommon.TabIndex = 0;
			this.SheetCommon.Text = "Основное";
			this.Label1.Location = new Point(8, 8);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(100, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Краткое название";
			this.Label3.Location = new Point(8, 32);
			this.Label3.Name = "Label3";
			this.Label3.Size = new Size(60, 13);
			this.Label3.TabIndex = 1;
			this.Label3.Text = "Автор";
			this.Label2.Location = new Point(8, 144);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(60, 13);
			this.Label2.TabIndex = 2;
			this.Label2.Text = "Название";
			this.Label4.Location = new Point(8, 256);
			this.Label4.Name = "Label4";
			this.Label4.Size = new Size(80, 13);
			this.Label4.TabIndex = 3;
			this.Label4.Text = "Опубликовано";
			this.EditShortTitle.Location = new Point(112, 8);
			this.EditShortTitle.Name = "EditShortTitle";
			this.EditShortTitle.Size = new Size(233, 21);
			this.EditShortTitle.TabIndex = 0;
			this.EditShortTitle.Text = "";
			this.EditShortTitle.TextChanged += new EventHandler(this.EditShortTitle_TextChanged);
			this.EditAuthor.Location = new Point(112, 32);
			this.EditAuthor.Multiline = true;
			this.EditAuthor.Name = "EditAuthor";
			this.EditAuthor.ScrollBars = ScrollBars.Vertical;
			this.EditAuthor.Size = new Size(409, 105);
			this.EditAuthor.TabIndex = 1;
			this.EditAuthor.Text = "";
			this.EditTitle.Location = new Point(112, 144);
			this.EditTitle.Multiline = true;
			this.EditTitle.Name = "EditTitle";
			this.EditTitle.ScrollBars = ScrollBars.Vertical;
			this.EditTitle.Size = new Size(409, 105);
			this.EditTitle.TabIndex = 2;
			this.EditTitle.Text = "";
			this.EditPublication.Location = new Point(112, 256);
			this.EditPublication.Multiline = true;
			this.EditPublication.Name = "EditPublication";
			this.EditPublication.ScrollBars = ScrollBars.Vertical;
			this.EditPublication.Size = new Size(409, 105);
			this.EditPublication.TabIndex = 3;
			this.EditPublication.Text = "";
			this.SheetText.Controls.Add(this.EditText);
			this.SheetText.Location = new Point(4, 22);
			this.SheetText.Name = "SheetText";
			this.SheetText.Size = new Size(529, 375);
			this.SheetText.TabIndex = 1;
			this.SheetText.Text = "Текст";
			this.EditText.Dock = DockStyle.Fill;
			this.EditText.Location = new Point(0, 0);
			this.EditText.Multiline = true;
			this.EditText.Name = "EditText";
			this.EditText.ScrollBars = ScrollBars.Both;
			this.EditText.Size = new Size(529, 375);
			this.EditText.TabIndex = 0;
			this.EditText.Text = "";
			this.SheetRepositories.Location = new Point(4, 22);
			this.SheetRepositories.Name = "SheetRepositories";
			this.SheetRepositories.Size = new Size(529, 375);
			this.SheetRepositories.TabIndex = 2;
			this.SheetRepositories.Text = "Архивы";
			this.SheetNotes.Location = new Point(4, 22);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new Size(529, 375);
			this.SheetNotes.TabIndex = 3;
			this.SheetNotes.Text = "Заметки";
			this.SheetMultimedia.Location = new Point(4, 22);
			this.SheetMultimedia.Name = "SheetMultimedia";
			this.SheetMultimedia.Size = new Size(529, 375);
			this.SheetMultimedia.TabIndex = 4;
			this.SheetMultimedia.Text = "Мультимедиа";
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(537, 449);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			base.Controls.Add(this.PagesData);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmSourceEdit";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Источник";
			this.PagesData.ResumeLayout(false);
			this.SheetCommon.ResumeLayout(false);
			this.SheetText.ResumeLayout(false);
			base.ResumeLayout(false);
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
			this.FNotesList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecNotesList(this.FNotesList);
			this.FMediaList = new TSheetList(this.SheetMultimedia);
			this.FMediaList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecMediaList(this.FMediaList);
			this.FRepositoriesList = new TSheetList(this.SheetRepositories);
			this.FRepositoriesList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
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
