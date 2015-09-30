using System;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Charts;
using GKUI.Controls;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TfmTreeFilter : Form
	{
		private readonly IBase fBase;
        private readonly GKSheetList fPersonsList;
        
        private ChartFilter fFilter;
		private string fTemp;

		public IBase Base
		{
			get	{ return this.fBase; }
		}

		public ChartFilter Filter
		{
			get	{ return this.fFilter; }
			set	{ this.fFilter = value;	}
		}

		private void ListModify(object sender, ModifyEventArgs eArgs)
		{
			if (sender == this.fPersonsList) {
                GEDCOMIndividualRecord iRec = eArgs.ItemData as GEDCOMIndividualRecord;

                switch (eArgs.Action)
                {
                    case RecordAction.raAdd:
					    iRec = this.Base.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svNone);
					    if (iRec != null) {
						    this.fTemp = this.fTemp + iRec.XRef + ";";
					    }
                        break;

                    case RecordAction.raDelete:
						if (iRec != null) {
							this.fTemp = this.fTemp.Replace(iRec.XRef + ";", "");
						}
                        break;
                }
			}

			this.UpdateControls();
		}

		private void UpdateControls()
		{
			switch (this.fFilter.BranchCut) {
				case ChartFilter.TBranchCut.bcPersons: {
					this.rbCutPersons.Checked = true;
					break;
				}
				case ChartFilter.TBranchCut.bcYears: {
					this.rbCutYears.Checked = true;
					break;
				}
				case ChartFilter.TBranchCut.bcNone: {
					this.rbCutNone.Checked = true;
					break;
				}
			}

			this.edYear.Enabled = (this.fFilter.BranchCut == ChartFilter.TBranchCut.bcYears);
			this.fPersonsList.Enabled = (this.fFilter.BranchCut == ChartFilter.TBranchCut.bcPersons);
			this.edYear.Text = this.fFilter.BranchYear.ToString();
			this.fPersonsList.List.Items.Clear();

			if (!string.IsNullOrEmpty(this.fTemp)) {
				string[] tmp_refs = this.fTemp.Split(';');

				int num = tmp_refs.Length;
				for (int i = 0; i < num; i++)
				{
					string xref = tmp_refs[i];
					GEDCOMIndividualRecord p = this.Base.Tree.XRefIndex_Find(xref) as GEDCOMIndividualRecord;
					if (p != null) this.fPersonsList.List.AddItem(p.GetNameString(true, false), p);
				}
			}

			if (this.fFilter.SourceMode != FilterGroupMode.gmSelected) {
				this.cbSource.SelectedIndex = (sbyte)this.fFilter.SourceMode;
			} else {
				GEDCOMSourceRecord srcRec = this.Base.Tree.XRefIndex_Find(this.fFilter.SourceRef) as GEDCOMSourceRecord;
				this.cbSource.Text = srcRec.FiledByEntry;
			}
		}

		private void rbCutNoneClick(object sender, EventArgs e)
		{
			if (this.rbCutNone.Checked)
			{
				this.fFilter.BranchCut = ChartFilter.TBranchCut.bcNone;
			}
			else
			{
				if (this.rbCutYears.Checked)
				{
					this.fFilter.BranchCut = ChartFilter.TBranchCut.bcYears;
				}
				else
				{
					if (this.rbCutPersons.Checked)
					{
						this.fFilter.BranchCut = ChartFilter.TBranchCut.bcPersons;
					}
				}
			}
			this.UpdateControls();
		}

		private void AcceptChanges()
		{
			if (this.rbCutNone.Checked)
			{
				this.fFilter.BranchCut = ChartFilter.TBranchCut.bcNone;
			}
			else
			{
				if (this.rbCutYears.Checked)
				{
					this.fFilter.BranchCut = ChartFilter.TBranchCut.bcYears;
					this.fFilter.BranchYear = int.Parse(this.edYear.Text);
				}
				else
				{
					if (this.rbCutPersons.Checked)
					{
						this.fFilter.BranchCut = ChartFilter.TBranchCut.bcPersons;
						this.fFilter.BranchPersons = this.fTemp;
					}
				}
			}

            int selectedIndex = this.cbSource.SelectedIndex;
			if (selectedIndex >= 0 && selectedIndex < 3)
			{
				this.fFilter.SourceMode = (FilterGroupMode)this.cbSource.SelectedIndex;
				this.fFilter.SourceRef = "";
			}
			else
			{
			    GKComboItem item = this.cbSource.Items[this.cbSource.SelectedIndex] as GKComboItem;
				GEDCOMRecord rec = item.Data as GEDCOMRecord;
				if (rec != null)
				{
					this.fFilter.SourceMode = FilterGroupMode.gmSelected;
					this.fFilter.SourceRef = rec.XRef;
				}
				else
				{
					this.fFilter.SourceMode = FilterGroupMode.gmAll;
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
			GEDCOMTree tree = this.Base.Tree;
			this.fTemp = this.fFilter.BranchPersons;

			this.cbSource.Sorted = true;
			int num = tree.RecordsCount - 1;
			for (int i = 0; i <= num; i++) {
				GEDCOMRecord rec = tree[i];
				if (rec is GEDCOMSourceRecord) {
					this.cbSource.Items.Add(new GKComboItem((rec as GEDCOMSourceRecord).FiledByEntry, rec));
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
