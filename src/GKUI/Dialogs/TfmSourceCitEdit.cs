using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Controls;

namespace GKUI
{
	public partial class TfmSourceCitEdit : Form
	{
		private TfmBase FBase;
		private TGEDCOMSourceCitation FSourceCitation;
		private TGEDCOMSourceRecord FTempSrc;
		private TStringList FSourcesList;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		public TGEDCOMSourceCitation SourceCitation
		{
			get { return this.FSourceCitation; }
			set { this.SetSourceCitation(value); }
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				if (this.FTempSrc == null) {
					SysUtils.ShowError("Не задан источник");
					base.DialogResult = DialogResult.None;
				} else {
					this.FSourceCitation.Value = this.FTempSrc;
					this.FSourceCitation.Page = this.EditPage.Text;
					this.FSourceCitation.CertaintyAssessment = this.EditCertainty.SelectedIndex;
					base.DialogResult = DialogResult.OK;
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmSourceCitEdit.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void btnSourceAdd_Click(object sender, EventArgs e)
		{
			object[] anArgs = new object[0];
			TGEDCOMSourceRecord src = FBase.SelectRecord(TGEDCOMRecordType.rtSource, anArgs) as TGEDCOMSourceRecord;
			if (src != null)
			{
				this.Base.Engine.GetSourcesList(this.FSourcesList);
				this.RefreshSourcesList("");
				this.cbSource.SelectedIndex = this.cbSource.Items.IndexOf(src.FiledByEntry);
			}
		}

		private void cbSource_KeyUp(object sender, KeyEventArgs e)
		{
			int s = this.cbSource.SelectionStart;
			this.RefreshSourcesList(this.cbSource.Text);
			this.cbSource.SelectionStart = s;
		}

		private void cbSource_SelectedIndexChanged(object sender, EventArgs e)
		{
			int idx = this.cbSource.SelectedIndex;
			if (idx < 0) {
				this.FTempSrc = null;
			} else {
				this.FTempSrc = ((this.cbSource.Items[idx] as TComboItem).Data as TGEDCOMSourceRecord);
			}
		}

		private void RefreshSourcesList(string aFilter)
		{
			string flt = "*" + aFilter + "*";
			this.cbSource.BeginUpdate();
			try
			{
				this.FTempSrc = null;
				this.cbSource.Items.Clear();

				int num = this.FSourcesList.Count - 1;
				for (int i = 0; i <= num; i++) {
					string st = this.FSourcesList[i];
					if (aFilter == "" || TGenEngine.IsMatchesMask(st, flt))
					{
						this.cbSource.Items.Add(new TComboItem(st, this.FSourcesList.GetObject(i)));
					}
				}
			}
			finally
			{
				this.cbSource.EndUpdate();
			}
		}

		private void SetSourceCitation([In] TGEDCOMSourceCitation Value)
		{
			this.FSourceCitation = Value;
			this.FTempSrc = (this.FSourceCitation.Value as TGEDCOMSourceRecord);
			if (this.FTempSrc != null)
			{
				this.cbSource.SelectedIndex = this.cbSource.Items.IndexOf(this.FTempSrc.FiledByEntry);
			}
			this.EditPage.Text = this.FSourceCitation.Page;
			this.EditCertainty.SelectedIndex = this.FSourceCitation.CertaintyAssessment;
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.FSourcesList.Free();
			}
			base.Dispose(Disposing);
		}

		public TfmSourceCitEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			int i = 0;
			do
			{
				this.EditCertainty.Items.Add(GKL.LSList[(int)TGenEngine.CertaintyAssessments[i] - 1]);
				i++;
			}
			while (i != 4);
			this.FSourcesList = new TStringList();
			this.Base.Engine.GetSourcesList(this.FSourcesList);
			this.RefreshSourcesList("");
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.Text = GKL.LSList[106];
			this.Label2.Text = GKL.LSList[109];
			this.Label1.Text = GKL.LSList[110];
			this.Label3.Text = GKL.LSList[111];
		}
	}
}
