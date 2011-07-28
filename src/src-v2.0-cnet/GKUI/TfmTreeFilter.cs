using GedCom551;
using GKCore;
using GKUI.Charts;
using GKUI.Controls;
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
	public class TfmTreeFilter : Form
	{
		private Button btnAccept;
		private Button btnCancel;
		private Label Label5;
		private ComboBox cbSource;
		private GroupBox rgBranchCut;
		private RadioButton rbCutNone;
		private RadioButton rbCutYears;
		private RadioButton rbCutPersons;
		private Label Label1;
		private NumericUpDown edYear;
		private Panel Panel1;
		private TfmBase FBase;
		private TChartFilter FFilter;
		private TSheetList FPersonsList;
		private string FTemp;

		[Browsable(false)]
		public TfmBase Base
		{
			get	{ return this.FBase; }
		}

		[Browsable(false)]
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
					TGEDCOMIndividualRecord i_rec = this.Base.SelectPerson(null, TGenEngine.TTargetMode.tmNone, TGEDCOMObject.TGEDCOMSex.svNone);
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
			TChartFilter.TBranchCut branchCut = this.FFilter.BranchCut;
			if (branchCut != TChartFilter.TBranchCut.bcNone)
			{
				if (branchCut != TChartFilter.TBranchCut.bcYears)
				{
					if (branchCut == TChartFilter.TBranchCut.bcPersons)
					{
						this.rbCutPersons.Checked = true;
					}
				}
				else
				{
					this.rbCutYears.Checked = true;
				}
			}
			else
			{
				this.rbCutNone.Checked = true;
			}
			this.edYear.Enabled = (this.FFilter.BranchCut == TChartFilter.TBranchCut.bcYears);
			this.FPersonsList.Enabled = (this.FFilter.BranchCut == TChartFilter.TBranchCut.bcPersons);
			this.edYear.Text = this.FFilter.BranchYear.ToString();
			this.FPersonsList.List.Items.Clear();
			int arg_BB_0 = 1;
			int num = TGKSys.GetTokensCount(this.FTemp, ';');
			int i = arg_BB_0;
			if (num >= i)
			{
				num++;
				do
				{
					string xref = TGKSys.GetToken(this.FTemp, ';', i);
					TGEDCOMIndividualRecord p = this.Base.Tree.XRefIndex_Find(xref) as TGEDCOMIndividualRecord;
					this.FPersonsList.List.AddItem(TGenEngine.GetNameStr(p, true, false), p);
					i++;
				}
				while (i != num);
			}
		}
		private void InitializeComponent()
		{
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.Label5 = new Label();
			this.cbSource = new ComboBox();
			this.rgBranchCut = new GroupBox();
			this.Label1 = new Label();
			this.rbCutNone = new RadioButton();
			this.rbCutYears = new RadioButton();
			this.rbCutPersons = new RadioButton();
			this.edYear = new NumericUpDown();
			this.Panel1 = new Panel();
			this.rgBranchCut.SuspendLayout();
			((ISupportInitialize)this.edYear).BeginInit();
			base.SuspendLayout();
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(216, 328);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 1;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.AccessibleName = "";
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(304, 328);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 2;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			this.btnCancel.Click += new EventHandler(this.btnCancel_Click);
			this.Label5.Location = new Point(8, 280);
			this.Label5.Name = "Label5";
			this.Label5.Size = new Size(54, 13);
			this.Label5.TabIndex = 0;
			this.Label5.Text = "Источники";
			this.cbSource.DropDownStyle = ComboBoxStyle.DropDownList;
			this.cbSource.Location = new Point(8, 296);
			this.cbSource.Name = "cbSource";
			this.cbSource.Size = new Size(377, 21);
			this.cbSource.TabIndex = 0;
			this.rgBranchCut.Controls.Add(this.Label1);
			this.rgBranchCut.Controls.Add(this.rbCutNone);
			this.rgBranchCut.Controls.Add(this.rbCutYears);
			this.rgBranchCut.Controls.Add(this.rbCutPersons);
			this.rgBranchCut.Controls.Add(this.edYear);
			this.rgBranchCut.Location = new Point(8, 8);
			this.rgBranchCut.Name = "rgBranchCut";
			this.rgBranchCut.Size = new Size(377, 265);
			this.rgBranchCut.TabIndex = 3;
			this.rgBranchCut.TabStop = false;
			this.rgBranchCut.Text = "Отсечение ветвей";
			this.Label1.Location = new Point(32, 80);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(23, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Год";
			this.rbCutNone.Checked = true;
			this.rbCutNone.Location = new Point(16, 24);
			this.rbCutNone.Name = "rbCutNone";
			this.rbCutNone.Size = new Size(249, 17);
			this.rbCutNone.TabIndex = 0;
			this.rbCutNone.TabStop = true;
			this.rbCutNone.Text = "нет";
			this.rbCutNone.Click += new EventHandler(this.rbCutNoneClick);
			this.rbCutYears.Location = new Point(16, 48);
			this.rbCutYears.Name = "rbCutYears";
			this.rbCutYears.Size = new Size(249, 17);
			this.rbCutYears.TabIndex = 1;
			this.rbCutYears.Text = "по границе лет";
			this.rbCutYears.Click += new EventHandler(this.rbCutNoneClick);
			this.rbCutPersons.Location = new Point(16, 104);
			this.rbCutPersons.Name = "rbCutPersons";
			this.rbCutPersons.Size = new Size(249, 17);
			this.rbCutPersons.TabIndex = 2;
			this.rbCutPersons.Text = "по заданным лицам";
			this.rbCutPersons.Click += new EventHandler(this.rbCutNoneClick);
			NumericUpDown arg_540_0 = this.edYear;
			int[] array = null;
			int[] array2 = array;
			int[] array3;
			int[] expr_50A = array3 = new int[4];
			if (array2 != null)
			{
				int num;
				if ((num = array2.Length) > 4)
				{
					num = 4;
				}
				if (num > 0)
				{
					Array.Copy(array2, array3, num);
				}
			}
			array = expr_50A;
			array[0] = 10;
			array[1] = 0;
			array[2] = 0;
			array[3] = 0;
			arg_540_0.Increment = new decimal(array);
			this.edYear.Location = new Point(64, 72);
			NumericUpDown arg_5A8_0 = this.edYear;
			int[] array4 = null;
			int[] array5 = array4;
			int[] array6;
			int[] expr_56A = array6 = new int[4];
			if (array5 != null)
			{
				int num2;
				if ((num2 = array5.Length) > 4)
				{
					num2 = 4;
				}
				if (num2 > 0)
				{
					Array.Copy(array5, array6, num2);
				}
			}
			array4 = expr_56A;
			array4[0] = 3000;
			array4[1] = 0;
			array4[2] = 0;
			array4[3] = 0;
			arg_5A8_0.Maximum = new decimal(array4);
			this.edYear.Name = "edYear";
			this.edYear.Size = new Size(121, 21);
			this.edYear.TabIndex = 3;
			this.Panel1.BorderStyle = BorderStyle.Fixed3D;
			this.Panel1.Location = new Point(16, 128);
			this.Panel1.Name = "Panel1";
			this.Panel1.Size = new Size(360, 136);
			this.Panel1.TabIndex = 5;
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(393, 361);
			base.Controls.Add(this.Label5);
			base.Controls.Add(this.cbSource);
			base.Controls.Add(this.Panel1);
			base.Controls.Add(this.rgBranchCut);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmTreeFilter";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Фильтр";
			base.Load += new EventHandler(this.TfmTreeFilter_Load);
			this.rgBranchCut.ResumeLayout(false);
			((ISupportInitialize)this.edYear).EndInit();
			base.ResumeLayout(false);
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
		private void btnAccept_Click(object sender, EventArgs e)
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
				this.FFilter.SourceMode = (TFilter.TGroupMode)this.cbSource.SelectedIndex;
				this.FFilter.SourceRef = "";
			}
			else
			{
				TGEDCOMRecord rec = (this.cbSource.Items[this.cbSource.SelectedIndex] as TComboItem).Data as TGEDCOMRecord;
				if (rec != null)
				{
					this.FFilter.SourceMode = TFilter.TGroupMode.gmSelected;
					this.FFilter.SourceRef = rec.XRef;
				}
				else
				{
					this.FFilter.SourceMode = TFilter.TGroupMode.gmAll;
					this.FFilter.SourceRef = "";
				}
			}
			base.DialogResult = DialogResult.OK;
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
			int arg_39_0 = 0;
			int num = tree.RecordsCount - 1;
			int i = arg_39_0;
			if (num >= i)
			{
				num++;
				do
				{
					if (tree.GetRecord(i) is TGEDCOMSourceRecord)
					{
						this.cbSource.Items.Add(new TComboItem((tree.GetRecord(i) as TGEDCOMSourceRecord).FiledByEntry, tree.GetRecord(i)));
					}
					i++;
				}
				while (i != num);
			}
			this.cbSource.Sorted = false;
			this.cbSource.Items.Insert(0, GKL.LSList[500]);
			this.cbSource.Items.Insert(1, GKL.LSList[501]);
			this.cbSource.Items.Insert(2, GKL.LSList[502]);
			if (this.FFilter.SourceMode != TFilter.TGroupMode.gmSelected)
			{
				this.cbSource.SelectedIndex = (int)((sbyte)this.FFilter.SourceMode);
			}
			else
			{
				this.cbSource.SelectedIndex = this.cbSource.Items.IndexOf(tree.XRefIndex_Find(this.FFilter.SourceRef));
			}
		}
		public TfmTreeFilter(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.FPersonsList = new TSheetList(this.Panel1);
			this.FPersonsList.Buttons = TEnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbDelete
			});
			this.FPersonsList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.FPersonsList.List.AddListColumn(GKL.LSList[52], 350, false);
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.Text = GKL.LSList[38];
			this.rgBranchCut.Text = GKL.LSList[496];
			this.rbCutNone.Text = GKL.LSList[497];
			this.rbCutYears.Text = GKL.LSList[498];
			this.Label1.Text = GKL.LSList[490];
			this.rbCutPersons.Text = GKL.LSList[499];
			this.Label5.Text = GKL.LSList[56];
		}

	}
}
