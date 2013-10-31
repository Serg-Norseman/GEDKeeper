using System;
using System.IO;
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
	public partial class TfmMediaEdit : Form
	{
		private bool FIsNew;
		private TGEDCOMMultimediaRecord FMediaRec;
		private TfmBase FBase;
		private GKSheetList FNotesList;
		private GKSheetList FSourcesList;

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
			bool result;
			TGEDCOMFileReferenceWithTitle file_ref = this.FMediaRec.FileReferences[0];

			if (this.FIsNew)
			{
				TGKStoreType gst = (TGKStoreType)this.cbStoreType.SelectedIndex;
				if ((gst == TGKStoreType.gstArchive || gst == TGKStoreType.gstStorage) && !this.Base.Media.CheckPath())
				{
					result = false;
					return result;
				}

				string ref_fn = "";
				result = this.Base.Media.MediaSave(this.edFile.Text, gst, ref ref_fn);

				if (result) {
					file_ref.LinkFile(ref_fn, (TGEDCOMMediaType)this.cbMediaType.SelectedIndex, TGEDCOMMultimediaFormat.mfUnknown);
				} else {
					return result;
				}
			}
			else
			{
				file_ref.MediaType = (TGEDCOMMediaType)this.cbMediaType.SelectedIndex;
			}

			file_ref.Title = this.edName.Text;

			this.ControlsRefresh();
			this.Base.ChangeRecord(this.FMediaRec);

			result = true;
			return result;
		}

		private void ControlsRefresh()
		{
			TGEDCOMFileReferenceWithTitle file_ref = this.FMediaRec.FileReferences[0];

			this.FIsNew = (file_ref.StringValue == "");

			this.edName.Text = file_ref.Title;
			this.cbMediaType.SelectedIndex = (int)file_ref.MediaType;
			this.edFile.Text = file_ref.StringValue;

			if (this.FIsNew) {
				this.StoreTypesRefresh(true, TGKStoreType.gstReference);
			} else {
				string dummy = "";
				TGKStoreType gst = MediaManager.GetStoreType(file_ref.StringValue, ref dummy);
				this.StoreTypesRefresh((gst == TGKStoreType.gstArchive), gst);
			}

			this.btnFileSelect.Enabled = this.FIsNew;
			this.cbStoreType.Enabled = this.FIsNew;

			this.Base.RecListNotesRefresh(this.FMediaRec, this.FNotesList.List, null);
			this.Base.RecListSourcesRefresh(this.FMediaRec, this.FSourcesList.List, null);
		}

		private void ListModify(object sender, ModifyEventArgs eArgs)
		{
			bool res = false;

			if (sender == this.FNotesList) {
				res = (this.Base.ModifyRecNote(this, this.FMediaRec, eArgs.ItemData as TGEDCOMNotes, eArgs.Action));
			}
			else if (sender == this.FSourcesList) {
				res = this.Base.ModifyRecSource(this, this.FMediaRec, eArgs.ItemData as TGEDCOMSourceCitation, eArgs.Action);
			}

			if (res) this.ControlsRefresh();
		}

		private void SetMediaRec(TGEDCOMMultimediaRecord Value)
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
				string file = this.OpenDialog1.FileName;

				this.edFile.Text = file;
				TGEDCOMMultimediaFormat file_fmt = TGEDCOMFileReference.RecognizeFormat(file);

				FileInfo info = new FileInfo(file);
				double file_size = (((double)info.Length / 1024) / 1024); // mb

				bool can_arc = true;
				if (file_fmt == TGEDCOMMultimediaFormat.mfWAV || file_fmt == TGEDCOMMultimediaFormat.mfAVI || file_fmt == TGEDCOMMultimediaFormat.mfMPG || file_size > 1)
				{
					can_arc = false;
				}
				this.StoreTypesRefresh(can_arc, TGKStoreType.gstReference);
				this.cbStoreType.Enabled = true;
			}
		}

		private void btnView_Click(object sender, EventArgs e)
		{
			this.AcceptChanges();
			this.Base.ShowMedia(this.FMediaRec, true);
		}

		private void edName_TextChanged(object sender, EventArgs e)
		{
			this.Text = LangMan.LSList[55] + " \"" + this.edName.Text + "\"";
		}

		private void StoreTypesRefresh(bool allow_arc, TGKStoreType select)
		{
			this.cbStoreType.Items.Clear();
			this.cbStoreType.Items.Add(LangMan.LSList[(int)GKData.GKStoreTypes[(int)TGKStoreType.gstReference].Name - 1]);
			this.cbStoreType.Items.Add(LangMan.LSList[(int)GKData.GKStoreTypes[(int)TGKStoreType.gstStorage].Name - 1]);
			if (allow_arc) {
				this.cbStoreType.Items.Add(LangMan.LSList[(int)GKData.GKStoreTypes[(int)TGKStoreType.gstArchive].Name - 1]);
			}
			this.cbStoreType.SelectedIndex = (int)select;
		}

		public TfmMediaEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;

			for (TGEDCOMMediaType mt = TGEDCOMMediaType.mtNone; mt <= TGEDCOMMediaType.mtLast; mt++)
			{
				this.cbMediaType.Items.Add(LangMan.LSList[(int)GKData.MediaTypes[(int)mt] - 1]);
			}

			this.FNotesList = new GKSheetList(this.SheetNotes);
			this.FNotesList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.Base.SetupRecNotesList(this.FNotesList);

			this.FSourcesList = new GKSheetList(this.SheetSources);
			this.FSourcesList.OnModify += new GKSheetList.ModifyEventHandler(this.ListModify);
			this.Base.SetupRecSourcesList(this.FSourcesList);

			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.SheetCommon.Text = LangMan.LSList[144];
			this.SheetNotes.Text = LangMan.LSList[54];
			this.SheetSources.Text = LangMan.LSList[56];
			this.Label1.Text = LangMan.LSList[125];
			this.Label2.Text = LangMan.LSList[113];
			this.Label4.Text = LangMan.LSList[146];
			this.Label3.Text = LangMan.LSList[147];
			this.btnView.Text = LangMan.LSList[148] + "...";
		}
	}
}
