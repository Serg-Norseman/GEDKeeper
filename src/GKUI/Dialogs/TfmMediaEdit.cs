using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Lists;

namespace GKUI
{
	public partial class TfmMediaEdit : Form
	{
		private bool FIsNew;
		private TGEDCOMMultimediaRecord FMediaRec;
		private TfmBase FBase;
		private TSheetList FNotesList;
		private TSheetList FSourcesList;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		public TGEDCOMMultimediaRecord MediaRec
		{
			get { return this.FMediaRec; }
			set { this.SetMediaRec(value); }
		}

		private bool AcceptChanges()
		{
			TGEDCOMFileReferenceWithTitle file_ref = this.FMediaRec.FileReferences[0];
			file_ref.Title = this.edName.Text;
			bool Result;
			if (this.FIsNew)
			{
				TGenEngine.TGKStoreType gst = (TGenEngine.TGKStoreType)this.cbStoreType.SelectedIndex;
				if (gst >= TGenEngine.TGKStoreType.gstArchive && gst < (TGenEngine.TGKStoreType)3)
				{
					if (!this.Base.IsAdvanced())
					{
						SysUtils.ShowError(GKL.LSList[149]);
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
				file_ref.LinkFile(ref_fn, (TGEDCOMMediaType)this.cbMediaType.SelectedIndex, TGEDCOMMultimediaFormat.mfUnknown);
			}
			else
			{
				file_ref.MediaType = (TGEDCOMMediaType)this.cbMediaType.SelectedIndex;
			}
			this.ControlsRefresh();
			this.Base.ChangeRecord(this.FMediaRec);
			Result = true;
			return Result;
		}

		private void ControlsRefresh()
		{
			TGEDCOMFileReferenceWithTitle file_ref = this.FMediaRec.FileReferences[0];
			this.FIsNew = (file_ref.StringValue == "");
			this.edName.Text = file_ref.Title;
			this.cbMediaType.SelectedIndex = (int)file_ref.MediaType;
			this.edFile.Text = file_ref.StringValue;
			string dummy = "";
			TGenEngine.TGKStoreType gst = this.Base.Engine.GetStoreType(file_ref.StringValue, ref dummy);
			this.cbStoreType.SelectedIndex = (int)gst;
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
				this.ActiveControl = this.edName;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("MediaEdit.SetMediaRec(): " + E.Message);
			}
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				if (this.AcceptChanges()) {
					base.DialogResult = DialogResult.OK;
				} else {
					base.DialogResult = DialogResult.None;
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmMediaEdit.Accept(): " + E.Message);
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
			TGEDCOMMediaType mt = TGEDCOMMediaType.mtNone;
			do
			{
				this.cbMediaType.Items.Add(GKL.LSList[(int)TGenEngine.MediaTypes[(int)mt] - 1]);
				mt++;
			}
			while (mt != (TGEDCOMMediaType)15);
			TGenEngine.TGKStoreType gst = TGenEngine.TGKStoreType.gstReference;
			do
			{
				this.cbStoreType.Items.Add(GKL.LSList[(int)TGenEngine.GKStoreType[(int)gst].Name - 1]);
				gst++;
			}
			while (gst != (TGenEngine.TGKStoreType)3);
			this.cbStoreType.SelectedIndex = 0;

			this.FNotesList = new TSheetList(this.SheetNotes);
			this.FNotesList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecNotesList(this.FNotesList);

			this.FSourcesList = new TSheetList(this.SheetSources);
			this.FSourcesList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
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
