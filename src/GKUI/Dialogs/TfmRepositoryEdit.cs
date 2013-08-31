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
	public partial class TfmRepositoryEdit : Form
	{
		private TfmBase FBase;
		private TGEDCOMRepositoryRecord FRepository;
		private GKSheetList FNotesList;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		public TGEDCOMRepositoryRecord Repository
		{
			get { return this.FRepository; }
			set { this.SetRepository(value); }
		}

		private void ControlsRefresh()
		{
			this.Base.RecListNotesRefresh(this.FRepository, this.FNotesList.List, null);
		}

		private void ListModify(object Sender, object ItemData, TRecAction Action)
		{
			if (object.Equals(Sender, this.FNotesList) && this.Base.ModifyRecNote(this, this.FRepository, ItemData as TGEDCOMNotes, Action))
			{
				this.ControlsRefresh();
			}
		}

		private void SetRepository(TGEDCOMRepositoryRecord Value)
		{
			this.FRepository = Value;
			this.edName.Text = this.FRepository.RepositoryName;
			this.ControlsRefresh();
		}

		private void btnAddress_Click(object sender, EventArgs e)
		{
			this.Base.ModifyAddress(this, this.FRepository.Address);
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.FRepository.RepositoryName = this.edName.Text;
				this.Base.ChangeRecord(this.FRepository);
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmRepositoryEdit.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		public TfmRepositoryEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;

			this.FNotesList = new GKSheetList(this.SheetNotes);
			this.FNotesList.OnModify += new GKSheetList.TModifyEvent(this.ListModify);
			this.Base.SetupRecNotesList(this.FNotesList);

			this.Text = LangMan.LSList[134];
			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.Label1.Text = LangMan.LSList[125];
			this.SheetNotes.Text = LangMan.LSList[54];
			this.btnAddress.Text = LangMan.LSList[82] + "...";
		}
	}
}
