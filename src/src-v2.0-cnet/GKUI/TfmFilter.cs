using GedCom551;
using GKCore;
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
	public class TfmFilter : Form
	{
		private Button btnAccept;
		private Button btnCancel;
		private GroupBox rgLife;
		private Label Label1;
		private ComboBox edName;
		private GroupBox rgSex;
		private Label Label2;
		private MaskedTextBox edAliveBeforeDate;
		private GroupBox GroupBox1;
		private CheckBox CheckPatriarch;
		private Label Label3;
		private ComboBox cbResidence;
		private Label Label4;
		private ComboBox cbGroup;
		private Label Label5;
		private ComboBox cbSource;
		private Label Label6;
		private ComboBox cbEventVal;
		private RadioButton RadioButton1;
		private RadioButton RadioButton2;
		private RadioButton RadioButton3;
		private RadioButton RadioButton4;
		private RadioButton RadioButton5;
		private RadioButton RadioButton6;
		private RadioButton RadioButton7;
		private TfmBase FBase;

		[Browsable(false)]
		public TfmBase Base
		{
			get { return this.FBase; }
		}

		private object[] StringsToArray(TStrings aStrings)
		{
			object[] Result = null;
			object[] arg_08_0 = Result;
			int count = aStrings.Count;
			object[] array = arg_08_0;
			int arg_11_0;
			if ((arg_11_0 = count) < 0)
			{
				arg_11_0 = 0;
			}
			object[] array2;
			object[] expr_16 = array2 = new object[arg_11_0];
			if (count > 0 && array != null)
			{
				int num;
				if ((num = array.Length) > count)
				{
					num = count;
				}
				if (num > 0)
				{
					Array.Copy(array, array2, num);
				}
			}
			Result = expr_16;
			int arg_48_0 = 0;
			int num2 = aStrings.Count - 1;
			int i = arg_48_0;
			if (num2 >= i)
			{
				num2++;
				do
				{
					Result[i] = aStrings[i];
					i++;
				}
				while (i != num2);
			}
			return Result;
		}
		private void InitializeComponent()
		{
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.rgLife = new GroupBox();
			this.RadioButton4 = new RadioButton();
			this.RadioButton3 = new RadioButton();
			this.RadioButton2 = new RadioButton();
			this.RadioButton1 = new RadioButton();
			this.Label1 = new Label();
			this.edName = new ComboBox();
			this.rgSex = new GroupBox();
			this.RadioButton7 = new RadioButton();
			this.RadioButton5 = new RadioButton();
			this.RadioButton6 = new RadioButton();
			this.Label2 = new Label();
			this.edAliveBeforeDate = new MaskedTextBox();
			this.GroupBox1 = new GroupBox();
			this.CheckPatriarch = new CheckBox();
			this.Label3 = new Label();
			this.cbResidence = new ComboBox();
			this.Label4 = new Label();
			this.cbGroup = new ComboBox();
			this.Label5 = new Label();
			this.cbSource = new ComboBox();
			this.Label6 = new Label();
			this.cbEventVal = new ComboBox();
			this.rgLife.SuspendLayout();
			this.rgSex.SuspendLayout();
			this.GroupBox1.SuspendLayout();
			base.SuspendLayout();
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(120, 464);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 8;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(208, 464);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 9;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			this.btnCancel.Click += new EventHandler(this.btnCancel_Click);
			this.rgLife.Controls.Add(this.RadioButton4);
			this.rgLife.Controls.Add(this.RadioButton3);
			this.rgLife.Controls.Add(this.RadioButton2);
			this.rgLife.Controls.Add(this.RadioButton1);
			this.rgLife.Location = new Point(8, 8);
			this.rgLife.Name = "rgLife";
			this.rgLife.Size = new Size(137, 104);
			this.rgLife.TabIndex = 0;
			this.rgLife.TabStop = false;
			this.RadioButton4.Location = new Point(8, 72);
			this.RadioButton4.Name = "RadioButton4";
			this.RadioButton4.Size = new Size(114, 24);
			this.RadioButton4.TabIndex = 6;
			this.RadioButton4.Text = "в живых до";
			this.RadioButton4.Click += new EventHandler(this.rgLifeClick);
			this.RadioButton3.Location = new Point(8, 56);
			this.RadioButton3.Name = "RadioButton3";
			this.RadioButton3.Size = new Size(114, 24);
			this.RadioButton3.TabIndex = 2;
			this.RadioButton3.Text = "только умершие";
			this.RadioButton3.Click += new EventHandler(this.rgLifeClick);
			this.RadioButton2.Location = new Point(8, 40);
			this.RadioButton2.Name = "RadioButton2";
			this.RadioButton2.Size = new Size(114, 24);
			this.RadioButton2.TabIndex = 1;
			this.RadioButton2.Text = "только живые";
			this.RadioButton2.Click += new EventHandler(this.rgLifeClick);
			this.RadioButton1.Location = new Point(8, 24);
			this.RadioButton1.Name = "RadioButton1";
			this.RadioButton1.TabIndex = 0;
			this.RadioButton1.Text = "все";
			this.RadioButton1.Click += new EventHandler(this.rgLifeClick);
			this.Label1.Location = new Point(8, 168);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(75, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Маска имени";
			this.edName.Location = new Point(8, 184);
			this.edName.Name = "edName";
			this.edName.Size = new Size(281, 21);
			this.edName.Sorted = true;
			this.edName.TabIndex = 3;
			this.edName.Text = "*";
			this.rgSex.Controls.Add(this.RadioButton7);
			this.rgSex.Controls.Add(this.RadioButton5);
			this.rgSex.Controls.Add(this.RadioButton6);
			this.rgSex.Location = new Point(152, 8);
			this.rgSex.Name = "rgSex";
			this.rgSex.Size = new Size(137, 104);
			this.rgSex.TabIndex = 1;
			this.rgSex.TabStop = false;
			this.RadioButton7.Location = new Point(8, 72);
			this.RadioButton7.Name = "RadioButton7";
			this.RadioButton7.Size = new Size(114, 24);
			this.RadioButton7.TabIndex = 5;
			this.RadioButton7.Text = "только женщины";
			this.RadioButton5.Location = new Point(8, 24);
			this.RadioButton5.Name = "RadioButton5";
			this.RadioButton5.TabIndex = 4;
			this.RadioButton5.Text = "все";
			this.RadioButton6.Location = new Point(8, 48);
			this.RadioButton6.Name = "RadioButton6";
			this.RadioButton6.Size = new Size(114, 24);
			this.RadioButton6.TabIndex = 3;
			this.RadioButton6.Text = "только мужчины";
			this.Label2.Location = new Point(8, 120);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(70, 13);
			this.Label2.TabIndex = 1;
			this.Label2.Text = "В живых до:";
			this.edAliveBeforeDate.Enabled = false;
			this.edAliveBeforeDate.Location = new Point(8, 136);
			this.edAliveBeforeDate.Mask = "00/00/0000";
			this.edAliveBeforeDate.MaxLength = 10;
			this.edAliveBeforeDate.Name = "edAliveBeforeDate";
			this.edAliveBeforeDate.Size = new Size(137, 21);
			this.edAliveBeforeDate.TabIndex = 2;
			this.edAliveBeforeDate.Text = "  .  .    ";
			this.GroupBox1.Controls.Add(this.CheckPatriarch);
			this.GroupBox1.Location = new Point(8, 408);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new Size(281, 41);
			this.GroupBox1.TabIndex = 7;
			this.GroupBox1.TabStop = false;
			this.CheckPatriarch.Location = new Point(8, 16);
			this.CheckPatriarch.Name = "CheckPatriarch";
			this.CheckPatriarch.Size = new Size(185, 17);
			this.CheckPatriarch.TabIndex = 0;
			this.CheckPatriarch.Text = "Только главы семей";
			this.Label3.Location = new Point(8, 216);
			this.Label3.Name = "Label3";
			this.Label3.Size = new Size(130, 13);
			this.Label3.TabIndex = 2;
			this.Label3.Text = "Маска местожительства";
			this.cbResidence.Location = new Point(8, 232);
			this.cbResidence.Name = "cbResidence";
			this.cbResidence.Size = new Size(281, 21);
			this.cbResidence.Sorted = true;
			this.cbResidence.TabIndex = 4;
			this.cbResidence.Text = "*";
			this.Label4.Location = new Point(8, 312);
			this.Label4.Name = "Label4";
			this.Label4.Size = new Size(45, 13);
			this.Label4.TabIndex = 3;
			this.Label4.Text = "Группы";
			this.cbGroup.DropDownStyle = ComboBoxStyle.DropDownList;
			this.cbGroup.Location = new Point(8, 328);
			this.cbGroup.Name = "cbGroup";
			this.cbGroup.Size = new Size(281, 21);
			this.cbGroup.TabIndex = 5;
			this.Label5.Location = new Point(8, 360);
			this.Label5.Name = "Label5";
			this.Label5.Size = new Size(60, 13);
			this.Label5.TabIndex = 4;
			this.Label5.Text = "Источники";
			this.cbSource.DropDownStyle = ComboBoxStyle.DropDownList;
			this.cbSource.Location = new Point(8, 376);
			this.cbSource.Name = "cbSource";
			this.cbSource.Size = new Size(281, 21);
			this.cbSource.TabIndex = 6;
			this.Label6.Location = new Point(8, 264);
			this.Label6.Name = "Label6";
			this.Label6.Size = new Size(85, 13);
			this.Label6.TabIndex = 5;
			this.Label6.Text = "Маска фактов";
			this.cbEventVal.Location = new Point(8, 280);
			this.cbEventVal.Name = "cbEventVal";
			this.cbEventVal.Size = new Size(281, 21);
			this.cbEventVal.Sorted = true;
			this.cbEventVal.TabIndex = 10;
			this.cbEventVal.Text = "*";
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(297, 497);
			base.Controls.Add(this.Label1);
			base.Controls.Add(this.Label2);
			base.Controls.Add(this.Label3);
			base.Controls.Add(this.Label4);
			base.Controls.Add(this.Label5);
			base.Controls.Add(this.Label6);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			base.Controls.Add(this.rgLife);
			base.Controls.Add(this.edName);
			base.Controls.Add(this.rgSex);
			base.Controls.Add(this.edAliveBeforeDate);
			base.Controls.Add(this.GroupBox1);
			base.Controls.Add(this.cbResidence);
			base.Controls.Add(this.cbGroup);
			base.Controls.Add(this.cbSource);
			base.Controls.Add(this.cbEventVal);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmFilter";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Фильтр";
			base.Load += new EventHandler(this.TfmFilter_Load);
			this.rgLife.ResumeLayout(false);
			this.rgSex.ResumeLayout(false);
			this.GroupBox1.ResumeLayout(false);
			base.ResumeLayout(false);
		}
		private void rgLifeClick(object sender, EventArgs e)
		{
			this.edAliveBeforeDate.Enabled = this.RadioButton4.Checked;
		}
		private void btnCancel_Click(object sender, EventArgs e)
		{
			this.Base.Filter.Clear();
			this.Base.ApplyFilter();
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			string fs = this.edName.Text.Trim();
			if (fs != "" && fs != "*" && GKL.fmGEDKeeper.Options.NameFilters.IndexOf(fs) < 0)
			{
				GKL.fmGEDKeeper.Options.NameFilters.Add(fs);
			}

			fs = this.cbResidence.Text.Trim();
			if (fs != "" && fs != "*" && GKL.fmGEDKeeper.Options.ResidenceFilters.IndexOf(fs) < 0)
			{
				GKL.fmGEDKeeper.Options.ResidenceFilters.Add(fs);
			}

			fs = this.cbEventVal.Text.Trim();
			if (fs != "" && fs != "*" && GKL.fmGEDKeeper.Options.EventFilters.IndexOf(fs) < 0)
			{
				GKL.fmGEDKeeper.Options.EventFilters.Add(fs);
			}

			this.Base.Filter.PatriarchOnly = this.CheckPatriarch.Checked;

			int life_sel = 0;
			if (this.RadioButton1.Checked) life_sel = 0;
			if (this.RadioButton2.Checked) life_sel = 1;
			if (this.RadioButton3.Checked) life_sel = 2;
			if (this.RadioButton4.Checked) life_sel = 3;

			if (this.Base.Filter.LifeMode != TGenEngine.TLifeMode.lmTimeLine)
			{
				this.Base.Filter.AliveBeforeDate = this.edAliveBeforeDate.Text;
				if (life_sel == 3)
				{
					try
					{
						DateTime dt = DateTime.Parse(this.edAliveBeforeDate.Text);
					}
					catch (Exception E)
					{
						TGKSys.ShowError(GKL.LSList[532]);
						base.DialogResult = DialogResult.None;
					}
				}
				this.Base.Filter.LifeMode = (TGenEngine.TLifeMode)life_sel;
			}

			int sex_sel = 0;
			if (this.RadioButton5.Checked) sex_sel = 0;
			if (this.RadioButton6.Checked) sex_sel = 1;
			if (this.RadioButton7.Checked) sex_sel = 2;
			this.Base.Filter.Sex = (TGEDCOMObject.TGEDCOMSex)sex_sel;

			if (this.edName.Text == "") this.edName.Text = "*";
			this.Base.Filter.Name = this.edName.Text;

			if (this.cbResidence.Text == "") this.cbResidence.Text = "*";
			this.Base.Filter.Residence = this.cbResidence.Text;

			if (this.cbEventVal.Text == "") this.cbEventVal.Text = "*";
			this.Base.Filter.EventVal = this.cbEventVal.Text;

			int selectedIndex = this.cbGroup.SelectedIndex;
			if (selectedIndex >= 0 && selectedIndex < 3) {
				this.Base.Filter.GroupMode = (TFilter.TGroupMode)this.cbGroup.SelectedIndex;
				this.Base.Filter.GroupRef = "";
			} else {
				TGEDCOMRecord rec = (this.cbGroup.Items[this.cbGroup.SelectedIndex] as TComboItem).Data as TGEDCOMRecord;
				if (rec != null) {
					this.Base.Filter.GroupMode = TFilter.TGroupMode.gmSelected;
					this.Base.Filter.GroupRef = rec.XRef;
				} else {
					this.Base.Filter.GroupMode = TFilter.TGroupMode.gmAll;
					this.Base.Filter.GroupRef = "";
				}
			}

			int selectedIndex2 = this.cbSource.SelectedIndex;
			if (selectedIndex2 >= 0 && selectedIndex2 < 3) {
				this.Base.Filter.SourceMode = (TFilter.TGroupMode)this.cbSource.SelectedIndex;
				this.Base.Filter.SourceRef = "";
			} else {
				TGEDCOMRecord rec = (this.cbSource.Items[this.cbSource.SelectedIndex] as TComboItem).Data as TGEDCOMRecord;
				if (rec != null) {
					this.Base.Filter.SourceMode = TFilter.TGroupMode.gmSelected;
					this.Base.Filter.SourceRef = rec.XRef;
				} else {
					this.Base.Filter.SourceMode = TFilter.TGroupMode.gmAll;
					this.Base.Filter.SourceRef = "";
				}
			}

			this.Base.ApplyFilter();
			base.DialogResult = DialogResult.OK;
		}

		private void TfmFilter_Load(object sender, EventArgs e)
		{
			this.edName.Items.AddRange(this.StringsToArray(GKL.fmGEDKeeper.Options.NameFilters));
			this.cbResidence.Items.AddRange(this.StringsToArray(GKL.fmGEDKeeper.Options.ResidenceFilters));
			this.cbEventVal.Items.AddRange(this.StringsToArray(GKL.fmGEDKeeper.Options.EventFilters));
			int life_sel;
			if (this.Base.Filter.LifeMode != TGenEngine.TLifeMode.lmTimeLine)
			{
				life_sel = (int)((sbyte)this.Base.Filter.LifeMode);
				this.rgLife.Enabled = true;
				this.edAliveBeforeDate.Text = this.Base.Filter.AliveBeforeDate;
			}
			else
			{
				life_sel = -1;
				this.rgLife.Enabled = false;
				this.edAliveBeforeDate.Text = "";
			}
			if (life_sel == 0)
			{
				this.RadioButton1.Checked = true;
			}
			else
			{
				if (life_sel == 1)
				{
					this.RadioButton2.Checked = true;
				}
				else
				{
					if (life_sel == 2)
					{
						this.RadioButton3.Checked = true;
					}
					else
					{
						if (life_sel == 3)
						{
							this.RadioButton4.Checked = true;
						}
					}
				}
			}
			int sex_sel = (int)((sbyte)this.Base.Filter.Sex);
			if (sex_sel == 0)
			{
				this.RadioButton5.Checked = true;
			}
			else
			{
				if (sex_sel == 1)
				{
					this.RadioButton6.Checked = true;
				}
				else
				{
					if (sex_sel == 2)
					{
						this.RadioButton7.Checked = true;
					}
				}
			}
			this.edName.Text = this.Base.Filter.Name;
			this.cbResidence.Text = this.Base.Filter.Residence;
			this.cbEventVal.Text = this.Base.Filter.EventVal;
			this.CheckPatriarch.Checked = this.Base.Filter.PatriarchOnly;
			TGEDCOMTree tree = this.Base.Tree;
			this.cbGroup.Sorted = true;
			int arg_1FF_0 = 0;
			int num = tree.RecordsCount - 1;
			int i = arg_1FF_0;
			if (num >= i)
			{
				num++;
				do
				{
					if (tree.GetRecord(i) is TGEDCOMGroupRecord)
					{
						this.cbGroup.Items.Add(new TComboItem((tree.GetRecord(i) as TGEDCOMGroupRecord).Name, tree.GetRecord(i)));
					}
					i++;
				}
				while (i != num);
			}
			this.cbGroup.Sorted = false;
			this.cbGroup.Items.Insert(0, new TComboItem(GKL.LSList[500], null));
			this.cbGroup.Items.Insert(1, new TComboItem(GKL.LSList[501], null));
			this.cbGroup.Items.Insert(2, new TComboItem(GKL.LSList[502], null));
			if (this.Base.Filter.GroupMode != TFilter.TGroupMode.gmSelected)
			{
				this.cbGroup.SelectedIndex = (int)((sbyte)this.Base.Filter.GroupMode);
			}
			else
			{
				this.cbGroup.SelectedIndex = this.cbGroup.Items.IndexOf(tree.XRefIndex_Find(this.Base.Filter.GroupRef));
			}
			this.cbSource.Sorted = true;
			int arg_337_0 = 0;
			int num2 = tree.RecordsCount - 1;
			i = arg_337_0;
			if (num2 >= i)
			{
				num2++;
				do
				{
					if (tree.GetRecord(i) is TGEDCOMSourceRecord)
					{
						this.cbSource.Items.Add(new TComboItem((tree.GetRecord(i) as TGEDCOMSourceRecord).FiledByEntry, tree.GetRecord(i)));
					}
					i++;
				}
				while (i != num2);
			}
			this.cbSource.Sorted = false;
			this.cbSource.Items.Insert(0, new TComboItem(GKL.LSList[500], null));
			this.cbSource.Items.Insert(1, new TComboItem(GKL.LSList[501], null));
			this.cbSource.Items.Insert(2, new TComboItem(GKL.LSList[502], null));
			if (this.Base.Filter.SourceMode != TFilter.TGroupMode.gmSelected)
			{
				this.cbSource.SelectedIndex = (int)((sbyte)this.Base.Filter.SourceMode);
			}
			else
			{
				this.cbSource.SelectedIndex = this.cbSource.Items.IndexOf(tree.XRefIndex_Find(this.Base.Filter.SourceRef));
			}
		}
		public TfmFilter(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.SetLang();
		}
		public void SetLang()
		{
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.RadioButton1.Text = GKL.LSList[522];
			this.RadioButton2.Text = GKL.LSList[523];
			this.RadioButton3.Text = GKL.LSList[524];
			this.RadioButton4.Text = GKL.LSList[525].ToLower();
			this.RadioButton5.Text = GKL.LSList[522];
			this.RadioButton6.Text = GKL.LSList[526];
			this.RadioButton7.Text = GKL.LSList[527];
			this.Label2.Text = GKL.LSList[525] + ":";
			this.Label1.Text = GKL.LSList[528];
			this.Label3.Text = GKL.LSList[529];
			this.Label6.Text = GKL.LSList[530];
			this.Label4.Text = GKL.LSList[58];
			this.Label5.Text = GKL.LSList[56];
			this.CheckPatriarch.Text = GKL.LSList[531];
		}

	}
}
