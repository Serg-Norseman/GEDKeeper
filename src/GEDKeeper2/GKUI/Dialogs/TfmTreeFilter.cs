using System;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKUI.Charts;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Dialogs
{
	public partial class TfmTreeFilter : Form
	{
		private readonly IBase fBase;
        private readonly GKSheetList fPersonsList;
        
        private TChartFilter fFilter;
		private string fTemp;

		public IBase Base
		{
			get	{ return this.fBase; }
		}

		public TChartFilter Filter
		{
			get	{ return this.fFilter; }
			set	{ this.fFilter = value;	}
		}

		private void ListModify(object sender, ModifyEventArgs eArgs)
		{
			if (sender == this.fPersonsList) {
                TGEDCOMIndividualRecord i_rec = eArgs.ItemData as TGEDCOMIndividualRecord;

                switch (eArgs.Action)
                {
                    case RecordAction.raAdd:
					    i_rec = this.Base.SelectPerson(null, TargetMode.tmNone, TGEDCOMSex.svNone);
					    if (i_rec != null) {
						    this.fTemp = this.fTemp + i_rec.XRef + ";";
					    }
                        break;

                    case RecordAction.raDelete:
						if (i_rec != null) {
							this.fTemp = this.fTemp.Replace(i_rec.XRef + ";", "");
						}
                        break;
                }
			}

			this.UpdateControls();
		}

		private void UpdateControls()
		{
			switch (this.fFilter.BranchCut) {
				case TChartFilter.TBranchCut.bcPersons: {
					this.rbCutPersons.Checked = true;
					break;
				}
				case TChartFilter.TBranchCut.bcYears: {
					this.rbCutYears.Checked = true;
					break;
				}
				case TChartFilter.TBranchCut.bcNone: {
					this.rbCutNone.Checked = true;
					break;
				}
			}

			this.edYear.Enabled = (this.fFilter.BranchCut == TChartFilter.TBranchCut.bcYears);
			this.fPersonsList.Enabled = (this.fFilter.BranchCut == TChartFilter.TBranchCut.bcPersons);
			this.edYear.Text = this.fFilter.BranchYear.ToString();
			this.fPersonsList.List.Items.Clear();

			if (!string.IsNullOrEmpty(this.fTemp)) {
				string[] tmp_refs = this.fTemp.Split(';');
				int num = tmp_refs.Length - 1;
				for (int i = 0; i <= num; i++)
				{
					string xref = tmp_refs[i];
					TGEDCOMIndividualRecord p = this.Base.Tree.XRefIndex_Find(xref) as TGEDCOMIndividualRecord;
					if (p != null) this.fPersonsList.List.AddItem(p.aux_GetNameStr(true, false), p);
				}
			}

			if (this.fFilter.SourceMode != TGroupMode.gmSelected) {
				this.cbSource.SelectedIndex = (sbyte)this.fFilter.SourceMode;
			} else {
				TGEDCOMSourceRecord src_rec = this.Base.Tree.XRefIndex_Find(this.fFilter.SourceRef) as TGEDCOMSourceRecord;
				this.cbSource.Text = src_rec.FiledByEntry;
			}
		}

		private void rbCutNoneClick(object sender, EventArgs e)
		{
			if (this.rbCutNone.Checked)
			{
				this.fFilter.BranchCut = TChartFilter.TBranchCut.bcNone;
			}
			else
			{
				if (this.rbCutYears.Checked)
				{
					this.fFilter.BranchCut = TChartFilter.TBranchCut.bcYears;
				}
				else
				{
					if (this.rbCutPersons.Checked)
					{
						this.fFilter.BranchCut = TChartFilter.TBranchCut.bcPersons;
					}
				}
			}
			this.UpdateControls();
		}

		private void AcceptChanges()
		{
			if (this.rbCutNone.Checked)
			{
				this.fFilter.BranchCut = TChartFilter.TBranchCut.bcNone;
			}
			else
			{
				if (this.rbCutYears.Checked)
				{
					this.fFilter.BranchCut = TChartFilter.TBranchCut.bcYears;
					this.fFilter.BranchYear = int.Parse(this.edYear.Text);
				}
				else
				{
					if (this.rbCutPersons.Checked)
					{
						this.fFilter.BranchCut = TChartFilter.TBranchCut.bcPersons;
						this.fFilter.BranchPersons = this.fTemp;
					}
				}
			}

            int selectedIndex = this.cbSource.SelectedIndex;
			if (selectedIndex >= 0 && selectedIndex < 3)
			{
				this.fFilter.SourceMode = (TGroupMode)this.cbSource.SelectedIndex;
				this.fFilter.SourceRef = "";
			}
			else
			{
			    GKComboItem item = this.cbSource.Items[this.cbSource.SelectedIndex] as GKComboItem;
				TGEDCOMRecord rec = item.Data as TGEDCOMRecord;
				if (rec != null)
				{
					this.fFilter.SourceMode = TGroupMode.gmSelected;
					this.fFilter.SourceRef = rec.XRef;
				}
				else
				{
					this.fFilter.SourceMode = TGroupMode.gmAll;
					this.fFilter.SourceRef = "";
				}
			}
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				AcceptChanges();
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmTreeFilter.Accept(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void btnCancel_Click(object sender, EventArgs e)
		{
			this.fFilter.Reset();
		}

		private void TfmTreeFilter_Load(object sender, EventArgs e)
		{
			TGEDCOMTree tree = this.Base.Tree;
			this.fTemp = this.fFilter.BranchPersons;

			this.cbSource.Sorted = true;
			int num = tree.RecordsCount - 1;
			for (int i = 0; i <= num; i++) {
				TGEDCOMRecord rec = tree[i];
				if (rec is TGEDCOMSourceRecord) {
					this.cbSource.Items.Add(new GKComboItem((rec as TGEDCOMSourceRecord).FiledByEntry, rec));
				}
			}
			this.cbSource.Sorted = false;

			this.cbSource.Items.Insert(0, LangMan.LS(LSID.LSID_SrcAll));
			this.cbSource.Items.Insert(1, LangMan.LS(LSID.LSID_SrcNot));
			this.cbSource.Items.Insert(2, LangMan.LS(LSID.LSID_SrcAny));

			this.UpdateControls();
		}

		public TfmTreeFilter(IBase aBase)
		{
			this.InitializeComponent();

            this.fBase = aBase;
			this.fPersonsList = new GKSheetList(this.Panel1);
			this.fPersonsList.Buttons = EnumSet<GKSheetList.SheetButton>.Create(
				GKSheetList.SheetButton.lbAdd, 
				GKSheetList.SheetButton.lbDelete
			);
			this.fPersonsList.OnModify += this.ListModify;
			this.fPersonsList.List.AddListColumn(LangMan.LS(LSID.LSID_RPIndividuals), 350, false);

			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.Text = LangMan.LS(LSID.LSID_MIFilter);
			this.rgBranchCut.Text = LangMan.LS(LSID.LSID_BranchCut);
			this.rbCutNone.Text = LangMan.LS(LSID.LSID_Not);
			this.rbCutYears.Text = LangMan.LS(LSID.LSID_BCut_Years);
			this.Label1.Text = LangMan.LS(LSID.LSID_Year);
			this.rbCutPersons.Text = LangMan.LS(LSID.LSID_BCut_Persons);
			this.Label5.Text = LangMan.LS(LSID.LSID_RPSources);
		}

	}
}
