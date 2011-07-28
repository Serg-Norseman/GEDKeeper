using GedCom551;
using GKCore;
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
	public class TfmMediaEdit : Form
	{
		private TabControl PagesData;
		private TabPage SheetNotes;
		private TabPage SheetSources;
		private Button btnAccept;
		private Button btnCancel;
		private OpenFileDialog OpenDialog1;
		private Button btnView;
		private TabPage SheetCommon;
		private Label Label1;
		private TextBox edName;
		private Label Label2;
		private ComboBox cbMediaType;
		private Label Label4;
		private ComboBox cbStoreType;
		private Label Label3;
		private TextBox edFile;
		private Button btnFileSelect;
		private bool FIsNew;
		private TGEDCOMMultimediaRecord FMediaRec;
		private TfmBase FBase;
		private TSheetList FNotesList;
		private TSheetList FSourcesList;

		[Browsable(false)]
		public TfmBase Base
		{
			get
			{
				return this.FBase;
			}
		}
		[Browsable(false)]
		public TGEDCOMMultimediaRecord MediaRec
		{
			get
			{
				return this.FMediaRec;
			}
			set
			{
				this.SetMediaRec(value);
			}
		}
		private bool AcceptChanges()
		{
			TGEDCOMFileReferenceWithTitle file_ref = this.FMediaRec.GetFileReference(0);
			file_ref.Title = this.edName.Text;
			bool Result;
			if (this.FIsNew)
			{
				TGenEngine.TGKStoreType gst = (TGenEngine.TGKStoreType)this.cbStoreType.SelectedIndex;
				if (gst >= TGenEngine.TGKStoreType.gstArchive && gst < (TGenEngine.TGKStoreType)3)
				{
					if (!this.Base.IsAdvanced())
					{
						TGKSys.ShowError(GKL.LSList[149]);
						if (this.Base.FileProperties(TfmBase.TFilePropertiesMode.fpmAdvanced) == DialogResult.Cancel || !this.Base.IsAdvanced())
						{
							Result = false;
							return Result;
						}
					}
					if (!this.Base.Engine.CheckPath())
					{
						Result = false;
						return Result;
					}
				}
				string source_fn = this.edFile.Text;
				string ref_fn = "";
				this.Base.Engine.MediaSave(source_fn, gst, ref ref_fn);
				file_ref.LinkFile(ref_fn, (TGEDCOMFileReference.TGEDCOMMediaType)this.cbMediaType.SelectedIndex, TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfUnknown);
			}
			else
			{
				file_ref.MediaType = (TGEDCOMFileReference.TGEDCOMMediaType)this.cbMediaType.SelectedIndex;
			}
			this.ControlsRefresh();
			this.Base.ChangeRecord(this.FMediaRec);
			Result = true;
			return Result;
		}
		private void ControlsRefresh()
		{
			TGEDCOMFileReferenceWithTitle file_ref = this.FMediaRec.GetFileReference(0);
			this.FIsNew = (BDSSystem.WStrCmp(file_ref.StringValue, "") == 0);
			this.edName.Text = file_ref.Title;
			this.cbMediaType.SelectedIndex = (int)((sbyte)file_ref.MediaType);
			this.edFile.Text = file_ref.StringValue;
			string dummy = "";
			TGenEngine.TGKStoreType gst = this.Base.Engine.GetStoreType(file_ref.StringValue, ref dummy);
			this.cbStoreType.SelectedIndex = (int)((sbyte)gst);
			this.edFile.Enabled = this.FIsNew;
			this.btnFileSelect.Enabled = this.FIsNew;
			this.cbStoreType.Enabled = this.FIsNew;
			this.Base.RecListNotesRefresh(this.FMediaRec, this.FNotesList.List, null);
			this.Base.RecListSourcesRefresh(this.FMediaRec, this.FSourcesList.List, null);
		}
		private void ListModify(object Sender, object ItemData, TGenEngine.TRecAction Action)
		{
			if (object.Equals(Sender, this.FNotesList))
			{
				if (this.Base.ModifyRecNote(this, this.FMediaRec, ItemData as TGEDCOMNotes, Action))
				{
					this.ControlsRefresh();
				}
			}
			else
			{
				if (object.Equals(Sender, this.FSourcesList) && this.Base.ModifyRecSource(this, this.FMediaRec, ItemData as TGEDCOMSourceCitation, Action))
				{
					this.ControlsRefresh();
				}
			}
		}
		private void SetMediaRec([In] TGEDCOMMultimediaRecord Value)
		{
			this.FMediaRec = Value;
			try
			{
				this.ControlsRefresh();
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("MediaEdit.SetMediaRec(): " + E.Message);
			}
		}
		private void InitializeComponent()
		{
			this.PagesData = new TabControl();
			this.SheetCommon = new TabPage();
			this.Label1 = new Label();
			this.Label2 = new Label();
			this.Label4 = new Label();
			this.Label3 = new Label();
			this.edName = new TextBox();
			this.cbMediaType = new ComboBox();
			this.cbStoreType = new ComboBox();
			this.edFile = new TextBox();
			this.btnFileSelect = new Button();
			this.SheetNotes = new TabPage();
			this.SheetSources = new TabPage();
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.OpenDialog1 = new OpenFileDialog();
			this.btnView = new Button();
			this.PagesData.SuspendLayout();
			this.SheetCommon.SuspendLayout();
			base.SuspendLayout();
			this.PagesData.Controls.Add(this.SheetCommon);
			this.PagesData.Controls.Add(this.SheetNotes);
			this.PagesData.Controls.Add(this.SheetSources);
			this.PagesData.Location = new Point(0, 0);
			this.PagesData.Name = "PagesData";
			this.PagesData.SelectedIndex = 0;
			this.PagesData.Size = new Size(522, 249);
			this.PagesData.TabIndex = 0;
			this.SheetCommon.Controls.Add(this.Label1);
			this.SheetCommon.Controls.Add(this.Label2);
			this.SheetCommon.Controls.Add(this.Label4);
			this.SheetCommon.Controls.Add(this.Label3);
			this.SheetCommon.Controls.Add(this.edName);
			this.SheetCommon.Controls.Add(this.cbMediaType);
			this.SheetCommon.Controls.Add(this.cbStoreType);
			this.SheetCommon.Controls.Add(this.edFile);
			this.SheetCommon.Controls.Add(this.btnFileSelect);
			this.SheetCommon.Location = new Point(4, 22);
			this.SheetCommon.Name = "SheetCommon";
			this.SheetCommon.Size = new Size(514, 223);
			this.SheetCommon.TabIndex = 0;
			this.SheetCommon.Text = "Общие данные";
			this.Label1.Location = new Point(8, 8);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(55, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Название";
			this.Label2.Location = new Point(8, 56);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(25, 13);
			this.Label2.TabIndex = 1;
			this.Label2.Text = "Тип";
			this.Label4.Location = new Point(192, 56);
			this.Label4.Name = "Label4";
			this.Label4.Size = new Size(95, 13);
			this.Label4.TabIndex = 2;
			this.Label4.Text = "Способ хранения";
			this.Label3.Location = new Point(8, 104);
			this.Label3.Name = "Label3";
			this.Label3.Size = new Size(35, 13);
			this.Label3.TabIndex = 3;
			this.Label3.Text = "Файл";
			this.edName.Location = new Point(8, 24);
			this.edName.Name = "edName";
			this.edName.Size = new Size(497, 21);
			this.edName.TabIndex = 0;
			this.edName.Text = "";
			this.edName.TextChanged += new EventHandler(this.edName_TextChanged);
			this.cbMediaType.DropDownStyle = ComboBoxStyle.DropDownList;
			this.cbMediaType.DropDownWidth = 15;
			this.cbMediaType.Location = new Point(8, 72);
			this.cbMediaType.Name = "cbMediaType";
			this.cbMediaType.Size = new Size(169, 21);
			this.cbMediaType.TabIndex = 1;
			this.cbStoreType.DropDownStyle = ComboBoxStyle.DropDownList;
			this.cbStoreType.Location = new Point(192, 72);
			this.cbStoreType.Name = "cbStoreType";
			this.cbStoreType.Size = new Size(201, 21);
			this.cbStoreType.TabIndex = 2;
			this.edFile.Location = new Point(8, 120);
			this.edFile.Name = "edFile";
			this.edFile.Size = new Size(449, 21);
			this.edFile.TabIndex = 3;
			this.edFile.Text = "";
			this.btnFileSelect.Location = new Point(464, 120);
			this.btnFileSelect.Name = "btnFileSelect";
			this.btnFileSelect.Size = new Size(43, 21);
			this.btnFileSelect.TabIndex = 4;
			this.btnFileSelect.Text = "...";
			this.btnFileSelect.Click += new EventHandler(this.btnFileSelect_Click);
			this.SheetNotes.Location = new Point(4, 22);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new Size(514, 223);
			this.SheetNotes.TabIndex = 1;
			this.SheetNotes.Text = "Заметки";
			this.SheetSources.Location = new Point(4, 22);
			this.SheetSources.Name = "SheetSources";
			this.SheetSources.Size = new Size(514, 223);
			this.SheetSources.TabIndex = 2;
			this.SheetSources.Text = "Источники";
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(344, 264);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 1;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(432, 264);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 2;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			this.OpenDialog1.Filter = "Все файлы (*.*)|*.*";
			this.btnView.Location = new Point(8, 264);
			this.btnView.Name = "btnView";
			this.btnView.Size = new Size(81, 25);
			this.btnView.TabIndex = 3;
			this.btnView.Text = "Просмотр...";
			this.btnView.Click += new EventHandler(this.btnView_Click);
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(522, 298);
			base.Controls.Add(this.PagesData);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			base.Controls.Add(this.btnView);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmMediaEdit";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Редактирование мультимедиа объекта";
			this.PagesData.ResumeLayout(false);
			this.SheetCommon.ResumeLayout(false);
			base.ResumeLayout(false);
		}
		private void btnAccept_Click(object sender, EventArgs e)
		{
			if (this.AcceptChanges())
			{
				base.DialogResult = DialogResult.OK;
			}
			else
			{
				base.DialogResult = DialogResult.None;
			}
		}
		private void btnFileSelect_Click(object sender, EventArgs e)
		{
			if (this.OpenDialog1.ShowDialog() == DialogResult.OK)
			{
				this.edFile.Text = this.OpenDialog1.FileName;
			}
		}
		private void btnView_Click(object sender, EventArgs e)
		{
			this.AcceptChanges();
			this.Base.ShowMedia(this.FMediaRec);
		}
		private void edName_TextChanged(object sender, EventArgs e)
		{
			this.Text = GKL.LSList[55] + " \"" + this.edName.Text + "\"";
		}
		public TfmMediaEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			TGEDCOMFileReference.TGEDCOMMediaType mt = TGEDCOMFileReference.TGEDCOMMediaType.mtNone;
			do
			{
				this.cbMediaType.Items.Add(GKL.LSList[(int)TGenEngine.MediaTypes[(int)mt] - 1]);
				mt++;
			}
			while (mt != (TGEDCOMFileReference.TGEDCOMMediaType)15);
			TGenEngine.TGKStoreType gst = TGenEngine.TGKStoreType.gstReference;
			do
			{
				this.cbStoreType.Items.Add(GKL.LSList[(int)TGenEngine.GKStoreType[(int)gst].Name - 1]);
				gst++;
			}
			while (gst != (TGenEngine.TGKStoreType)3);
			this.cbStoreType.SelectedIndex = 0;
			this.FNotesList = new TSheetList(this.SheetNotes);
			this.FNotesList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecNotesList(this.FNotesList);
			this.FSourcesList = new TSheetList(this.SheetSources);
			this.FSourcesList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecSourcesList(this.FSourcesList);
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.SheetCommon.Text = GKL.LSList[144];
			this.SheetNotes.Text = GKL.LSList[54];
			this.SheetSources.Text = GKL.LSList[56];
			this.Label1.Text = GKL.LSList[125];
			this.Label2.Text = GKL.LSList[113];
			this.Label4.Text = GKL.LSList[146];
			this.Label3.Text = GKL.LSList[147];
			this.btnView.Text = GKL.LSList[148] + "...";
		}
	}
}
