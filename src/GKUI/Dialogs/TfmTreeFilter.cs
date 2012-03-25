using System;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKCore;
using GKUI.Charts;
using GKUI.Controls;
using GKUI.Lists;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI
{
	public partial class TfmTreeFilter : Form
	{
		private TfmBase FBase;
		private TChartFilter FFilter;
		private TSheetList FPersonsList;
		private string FTemp;

		public TfmBase Base
		{
			get	{ return this.FBase; }
		}

		public TChartFilter Filter
		{
			get	{ return this.FFilter; }
			set	{ this.FFilter = value;	}
		}

		private void ListModify(object Sender, object ItemData, TGenEngine.TRecAction Action)
		{
			if (object.Equals(Sender, this.FPersonsList))
			{
				if (Action != TGenEngine.TRecAction.raAdd)
				{
					if (Action == TGenEngine.TRecAction.raDelete)
					{
						TGEDCOMIndividualRecord i_rec = ItemData as TGEDCOMIndividualRecord;
						if (i_rec != null)
						{
							this.FTemp = this.FTemp.Replace(i_rec.XRef + ";", "");
						}
					}
				}
				else
				{
					TGEDCOMIndividualRecord i_rec = this.Base.SelectPerson(null, TGenEngine.TTargetMode.tmNone, TGEDCOMSex.svNone);
					if (i_rec != null)
					{
						this.FTemp = this.FTemp + i_rec.XRef + ";";
					}
				}
			}
			this.UpdateControls();
		}

		private void UpdateControls()
		{
			switch (this.FFilter.BranchCut) {
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

			this.edYear.Enabled = (this.FFilter.BranchCut == TChartFilter.TBranchCut.bcYears);
			this.FPersonsList.Enabled = (this.FFilter.BranchCut == TChartFilter.TBranchCut.bcPersons);
			this.edYear.Text = this.FFilter.BranchYear.ToString();
			this.FPersonsList.List.Items.Clear();

			if (!string.IsNullOrEmpty(this.FTemp)) {
				string[] tmp_refs = this.FTemp.Split(';');
				int num = tmp_refs.Length - 1;
				for (int i = 0; i <= num; i++)
				{
					string xref = tmp_refs[i];
					TGEDCOMIndividualRecord p = this.Base.Tree.XRefIndex_Find(xref) as TGEDCOMIndividualRecord;
					if (p != null) this.FPersonsList.List.AddItem(p.aux_GetNameStr(true, false), p);
				}
			}
		}

		private void rbCutNoneClick(object sender, EventArgs e)
		{
			if (this.rbCutNone.Checked)
			{
				this.FFilter.BranchCut = TChartFilter.TBranchCut.bcNone;
			}
			else
			{
				if (this.rbCutYears.Checked)
				{
					this.FFilter.BranchCut = TChartFilter.TBranchCut.bcYears;
				}
				else
				{
					if (this.rbCutPersons.Checked)
					{
						this.FFilter.BranchCut = TChartFilter.TBranchCut.bcPersons;
					}
				}
			}
			this.UpdateControls();
		}

		private void AcceptChanges()
		{
			if (this.rbCutNone.Checked)
			{
				this.FFilter.BranchCut = TChartFilter.TBranchCut.bcNone;
			}
			else
			{
				if (this.rbCutYears.Checked)
				{
					this.FFilter.BranchCut = TChartFilter.TBranchCut.bcYears;
					this.FFilter.BranchYear = int.Parse(this.edYear.Text);
				}
				else
				{
					if (this.rbCutPersons.Checked)
					{
						this.FFilter.BranchCut = TChartFilter.TBranchCut.bcPersons;
						this.FFilter.BranchPersons = this.FTemp;
					}
				}
			}
			int selectedIndex = this.cbSource.SelectedIndex;
			if (selectedIndex >= 0 && selectedIndex < 3)
			{
				this.FFilter.SourceMode = (CustomFilter.TGroupMode)this.cbSource.SelectedIndex;
				this.FFilter.SourceRef = "";
			}
			else
			{
				TGEDCOMRecord rec = (this.cbSource.Items[this.cbSource.SelectedIndex] as GKComboItem).Data as TGEDCOMRecord;
				if (rec != null)
				{
					this.FFilter.SourceMode = CustomFilter.TGroupMode.gmSelected;
					this.FFilter.SourceRef = rec.XRef;
				}
				else
				{
					this.FFilter.SourceMode = CustomFilter.TGroupMode.gmAll;
					this.FFilter.SourceRef = "";
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
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmTreeFilter.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void btnCancel_Click(object sender, EventArgs e)
		{
			this.FFilter.Clear();
		}

		private void TfmTreeFilter_Load(object sender, EventArgs e)
		{
			TGEDCOMTree tree = this.Base.Tree;
			this.FTemp = this.FFilter.BranchPersons;
			this.UpdateControls();
			this.cbSource.Sorted = true;

			int num = tree.RecordsCount - 1;
			for (int i = 0; i <= num; i++) {
				TGEDCOMRecord rec = tree[i];
				if (rec is TGEDCOMSourceRecord) {
					this.cbSource.Items.Add(new GKComboItem((rec as TGEDCOMSourceRecord).FiledByEntry, rec));
				}
			}

			this.cbSource.Sorted = false;
			this.cbSource.Items.Insert(0, LangMan.LSList[500]);
			this.cbSource.Items.Insert(1, LangMan.LSList[501]);
			this.cbSource.Items.Insert(2, LangMan.LSList[502]);
			if (this.FFilter.SourceMode != CustomFilter.TGroupMode.gmSelected) {
				this.cbSource.SelectedIndex = (int)((sbyte)this.FFilter.SourceMode);
			} else {
				this.cbSource.SelectedIndex = this.cbSource.Items.IndexOf(tree.XRefIndex_Find(this.FFilter.SourceRef));
			}
		}

		public TfmTreeFilter(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.FPersonsList = new TSheetList(this.Panel1);
			this.FPersonsList.Buttons = EnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbDelete
			});
			this.FPersonsList.OnModify += new TSheetList.TModifyEvent(this.ListModify);
			this.FPersonsList.List.AddListColumn(LangMan.LSList[52], 350, false);

			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.Text = LangMan.LSList[38];
			this.rgBranchCut.Text = LangMan.LSList[496];
			this.rbCutNone.Text = LangMan.LSList[497];
			this.rbCutYears.Text = LangMan.LSList[498];
			this.Label1.Text = LangMan.LSList[490];
			this.rbCutPersons.Text = LangMan.LSList[499];
			this.Label5.Text = LangMan.LSList[56];
		}

	}
}
