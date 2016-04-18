using System;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKUI.Controls;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TfmSourceCitEdit : Form, IBaseEditor
	{
		private readonly IBaseWindow fBase;
        private readonly StringList fSourcesList;

        private GEDCOMSourceCitation fSourceCitation;

		public GEDCOMSourceCitation SourceCitation
		{
			get { return this.fSourceCitation; }
			set { this.SetSourceCitation(value); }
		}

        public IBaseWindow Base
		{
			get { return this.fBase; }
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				int idx = this.fSourcesList.IndexOf(this.cbSource.Text);
				GEDCOMSourceRecord src = ((idx < 0) ? null : (this.fSourcesList.GetObject(idx) as GEDCOMSourceRecord));

				if (src == null) {
					GKUtils.ShowError(LangMan.LS(LSID.LSID_DoNotSetSource));
					base.DialogResult = DialogResult.None;
				} else {
					this.fSourceCitation.Value = src;
					this.fSourceCitation.Page = this.EditPage.Text;
					this.fSourceCitation.CertaintyAssessment = this.EditCertainty.SelectedIndex;
					base.DialogResult = DialogResult.OK;
				}
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmSourceCitEdit.btnAccept_Click(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void btnSourceAdd_Click(object sender, EventArgs e)
		{
			object[] anArgs = new object[0];
			GEDCOMSourceRecord src = fBase.SelectRecord(GEDCOMRecordType.rtSource, anArgs) as GEDCOMSourceRecord;
		    if (src == null) return;
		    
            this.fBase.Context.GetSourcesList(this.fSourcesList);
		    this.RefreshSourcesList("");
		    this.cbSource.Text = src.FiledByEntry;
		}

        // FIXME
		private void cbSource_KeyDown(object sender, KeyEventArgs e)
		{
			//
		}

		private void cbSource_KeyUp(object sender, KeyEventArgs e)
		{
			this.RefreshSourcesList(this.cbSource.Text);
			this.cbSource.SelectionStart = this.cbSource.Text.Length;
		}

		private void RefreshSourcesList(string filter)
		{
			this.cbSource.BeginUpdate();
			try
			{
				this.cbSource.Items.Clear();

				string flt = "*" + filter + "*";

				int num = this.fSourcesList.Count;
				for (int i = 0; i < num; i++) {
					string st = this.fSourcesList[i];

					if (filter == "" || GKUtils.MatchesMask(st, flt))
					{
						this.cbSource.Items.Add(new GKComboItem(st, this.fSourcesList.GetObject(i)));
					}
				}
			}
			finally
			{
				this.cbSource.EndUpdate();
			}
		}

		private void SetSourceCitation(GEDCOMSourceCitation value)
		{
			this.fSourceCitation = value;

			GEDCOMSourceRecord src = (this.fSourceCitation.Value as GEDCOMSourceRecord);
			if (src != null) this.cbSource.Text = src.FiledByEntry;

			this.EditPage.Text = this.fSourceCitation.Page;
			this.EditCertainty.SelectedIndex = this.fSourceCitation.CertaintyAssessment;
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
                this.fSourcesList.Dispose();
			}
			base.Dispose(disposing);
		}

		public TfmSourceCitEdit(IBaseWindow aBase)
		{
			this.InitializeComponent();

			this.fBase = aBase;

			for (int i = 0; i < GKData.CertaintyAssessments.Length; i++)
			{
				this.EditCertainty.Items.Add(LangMan.LS(GKData.CertaintyAssessments[i]));
			}

			this.fSourcesList = new StringList();
			this.fBase.Context.GetSourcesList(this.fSourcesList);
			this.RefreshSourcesList("");

			// SetLang()
			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.Text = LangMan.LS(LSID.LSID_WinSourceCitEdit);
			this.Label2.Text = LangMan.LS(LSID.LSID_Source);
			this.Label1.Text = LangMan.LS(LSID.LSID_Page);
			this.Label3.Text = LangMan.LS(LSID.LSID_Certainty);
		}
	}
}
