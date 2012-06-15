using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKCore;
using GKUI.Controls;

/// <summary>
/// Localization: dirty
/// </summary>

namespace GKUI
{
	public partial class TfmSourceCitEdit : Form
	{
		private TfmBase FBase;
		private TGEDCOMSourceCitation FSourceCitation;
		private StringList FSourcesList;

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
				int idx = this.FSourcesList.IndexOf(this.cbSource.Text);
				TGEDCOMSourceRecord src = ((idx < 0) ? null : (this.FSourcesList.GetObject(idx) as TGEDCOMSourceRecord));

				if (src == null) {
					TGenEngine.ShowError("Не задан источник");
					base.DialogResult = DialogResult.None;
				} else {
					this.FSourceCitation.Value = src;
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
				this.cbSource.Text = src.FiledByEntry;
			}
		}

		void cbSource_KeyDown(object sender, KeyEventArgs e)
		{
			//
		}

		void cbSource_KeyUp(object sender, KeyEventArgs e)
		{
			this.RefreshSourcesList(this.cbSource.Text);
			this.cbSource.SelectionStart = this.cbSource.Text.Length;
		}

		private void RefreshSourcesList(string aFilter)
		{
			this.cbSource.BeginUpdate();
			try
			{
				this.cbSource.Items.Clear();

				string flt = "*" + aFilter + "*";
				int num = this.FSourcesList.Count - 1;
				for (int i = 0; i <= num; i++) {
					string st = this.FSourcesList[i];
					if (aFilter == "" || TGenEngine.MatchesMask(st, flt))
					{
						this.cbSource.Items.Add(new GKComboItem(st, this.FSourcesList.GetObject(i)));
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

			TGEDCOMSourceRecord src = (this.FSourceCitation.Value as TGEDCOMSourceRecord);
			if (src != null) this.cbSource.Text = src.FiledByEntry;

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

			for (int i = 0; i <= TGenEngine.CertaintyAssessments.Length - 1; i++)
			{
				this.EditCertainty.Items.Add(LangMan.LSList[(int)TGenEngine.CertaintyAssessments[i] - 1]);
			}

			this.FSourcesList = new StringList();

			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.Text = LangMan.LSList[106];
			this.Label2.Text = LangMan.LSList[109];
			this.Label1.Text = LangMan.LSList[110];
			this.Label3.Text = LangMan.LSList[111];

			this.Base.Engine.GetSourcesList(this.FSourcesList);
			this.RefreshSourcesList("");
		}
		
	}
}
