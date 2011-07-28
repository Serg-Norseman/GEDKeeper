using GedCom551;
using GKCore;
using GKUI.Controls;
using GKSys;
using System;
using System.ComponentModel;
using System.Drawing;
using System.Resources;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmSourceCitEdit : Form
	{

		private Button btnAccept;
		private Button btnCancel;
		private Label Label1;
		private TextBox EditPage;
		private Label Label2;
		private Button btnSourceAdd;
		private Label Label3;
		private ComboBox EditCertainty;
		private ComboBox cbSource;
		internal TfmBase FBase;
		internal TGEDCOMSourceCitation FSourceCitation;
		internal TGEDCOMSourceRecord FTempSrc;
		internal TStringList FSourcesList;

		[Browsable(false)]
		public TfmBase Base
		{
			get
			{
				return this.FBase;
			}
		}
		[Browsable(false)]
		public TGEDCOMSourceCitation SourceCitation
		{
			get
			{
				return this.FSourceCitation;
			}
			set
			{
				this.SetSourceCitation(value);
			}
		}
		private void InitializeComponent()
		{
			ResourceManager resources = new ResourceManager(typeof(TfmSourceCitEdit));
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.Label1 = new Label();
			this.EditPage = new TextBox();
			this.Label2 = new Label();
			this.btnSourceAdd = new Button();
			this.Label3 = new Label();
			this.EditCertainty = new ComboBox();
			this.cbSource = new ComboBox();
			base.SuspendLayout();
			this.btnAccept.Image = (resources.GetObject("btnAccept.Image") as Image);
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(176, 160);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 4;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.Image = (resources.GetObject("btnCancel.Image") as Image);
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(264, 160);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 5;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			this.Label1.Location = new Point(8, 56);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(90, 13);
			this.Label1.TabIndex = 6;
			this.Label1.Text = "Лист/Страница";
			this.EditPage.Location = new Point(8, 72);
			this.EditPage.Name = "EditPage";
			this.EditPage.Size = new Size(337, 21);
			this.EditPage.TabIndex = 2;
			this.EditPage.Text = "";
			this.Label2.Location = new Point(8, 8);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(60, 13);
			this.Label2.TabIndex = 7;
			this.Label2.Text = "Источник";
			this.btnSourceAdd.AccessibleDescription = "Выбрать персональную запись";
			this.btnSourceAdd.Location = new Point(320, 21);
			this.btnSourceAdd.Name = "btnSourceAdd";
			this.btnSourceAdd.Size = new Size(26, 26);
			this.btnSourceAdd.TabIndex = 8;
			this.btnSourceAdd.Click += new EventHandler(this.btnSourceAdd_Click);
			this.Label3.Location = new Point(8, 104);
			this.Label3.Name = "Label3";
			this.Label3.Size = new Size(100, 13);
			this.Label3.TabIndex = 9;
			this.Label3.Text = "Оценка качества";
			this.EditCertainty.DropDownStyle = ComboBoxStyle.DropDownList;
			this.EditCertainty.Location = new Point(8, 120);
			this.EditCertainty.Name = "EditCertainty";
			this.EditCertainty.Size = new Size(337, 21);
			this.EditCertainty.TabIndex = 3;
			this.cbSource.Location = new Point(8, 24);
			this.cbSource.Name = "cbSource";
			this.cbSource.Size = new Size(306, 21);
			this.cbSource.Sorted = true;
			this.cbSource.TabIndex = 1;
			this.cbSource.KeyUp += new KeyEventHandler(this.cbSource_KeyUp);
			this.cbSource.SelectedIndexChanged += new EventHandler(this.cbSource_SelectedIndexChanged);
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(353, 193);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			base.Controls.Add(this.Label1);
			base.Controls.Add(this.EditPage);
			base.Controls.Add(this.Label2);
			base.Controls.Add(this.btnSourceAdd);
			base.Controls.Add(this.Label3);
			base.Controls.Add(this.EditCertainty);
			base.Controls.Add(this.cbSource);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmSourceCitEdit";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "SourceCitEdit";
			base.ResumeLayout(false);
		}
		private void btnAccept_Click(object sender, EventArgs e)
		{
			if (this.FTempSrc == null)
			{
				TGKSys.ShowError("Не задан источник");
				base.DialogResult = DialogResult.None;
			}
			else
			{
				this.FSourceCitation.Value = this.FTempSrc;
				this.FSourceCitation.Page = this.EditPage.Text;
				this.FSourceCitation.CertaintyAssessment = this.EditCertainty.SelectedIndex;
				base.DialogResult = DialogResult.OK;
			}
		}
		private void btnSourceAdd_Click(object sender, EventArgs e)
		{
			TfmBase arg_0F_0 = this.Base;
			TGEDCOMRecord.TGEDCOMRecordType arg_0F_1 = TGEDCOMRecord.TGEDCOMRecordType.rtSource;
			object[] anArgs = new object[0];
			TGEDCOMSourceRecord src = arg_0F_0.SelectRecord(arg_0F_1, anArgs) as TGEDCOMSourceRecord;
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
			if (idx < 0)
			{
				this.FTempSrc = null;
			}
			else
			{
				this.FTempSrc = ((this.cbSource.Items[idx] as TComboItem).Data as TGEDCOMSourceRecord);
			}
		}

		internal void RefreshSourcesList(string aFilter)
		{
			string flt = "*" + aFilter + "*";
			this.cbSource.BeginUpdate();
			try
			{
				this.FTempSrc = null;
				this.cbSource.Items.Clear();
				int arg_42_0 = 0;
				int num = this.FSourcesList.Count - 1;
				int i = arg_42_0;
				if (num >= i)
				{
					num++;
					do
					{
						string st = this.FSourcesList[i];
						if (BDSSystem.WStrCmp(aFilter, "") == 0 || TGenEngine.IsMatchesMask(st, flt))
						{
							this.cbSource.Items.Add(new TComboItem(st, this.FSourcesList.GetObject(i)));
						}
						i++;
					}
					while (i != num);
				}
			}
			finally
			{
				this.cbSource.EndUpdate();
			}
		}

		internal void SetSourceCitation([In] TGEDCOMSourceCitation Value)
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
