using GedCom551;
using GKCore;
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
	public class TfmNameEdit : Form
	{
		private Label Label2;
		private TextBox edName;
		private Label Label4;
		private ComboBox edSex;
		private Button btnAccept;
		private Button btnCancel;
		private GroupBox GroupBox1;
		private Label Label3;
		private TextBox edFPatr;
		private Label Label1;
		private TextBox edMPatr;
		private TNamesTable.TName FIName;

		[Browsable(false)]
		public TNamesTable.TName IName
		{
			get
			{
				return this.FIName;
			}
			set
			{
				this.SetIName(value);
			}
		}
		private void SetIName([In] TNamesTable.TName Value)
		{
			this.FIName = Value;
			if (this.FIName == null)
			{
				this.edName.Text = "";
				this.edSex.SelectedIndex = 0;
				this.edFPatr.Text = "";
				this.edMPatr.Text = "";
			}
			else
			{
				this.edName.Text = this.FIName.Name;
				this.edSex.SelectedIndex = (int)((sbyte)this.FIName.Sex);
				this.edFPatr.Text = this.FIName.F_Patronymic;
				this.edMPatr.Text = this.FIName.M_Patronymic;
			}
		}
		private void InitializeComponent()
		{
			ResourceManager resources = new ResourceManager(typeof(TfmNameEdit));
			this.Label2 = new Label();
			this.edName = new TextBox();
			this.Label4 = new Label();
			this.edSex = new ComboBox();
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.GroupBox1 = new GroupBox();
			this.Label3 = new Label();
			this.Label1 = new Label();
			this.edFPatr = new TextBox();
			this.edMPatr = new TextBox();
			this.GroupBox1.SuspendLayout();
			base.SuspendLayout();
			this.Label2.Location = new Point(8, 16);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(25, 13);
			this.Label2.TabIndex = 0;
			this.Label2.Text = "Имя";
			this.edName.Location = new Point(72, 8);
			this.edName.Name = "edName";
			this.edName.Size = new Size(193, 21);
			this.edName.TabIndex = 0;
			this.edName.Text = "";
			this.edName.KeyPress += new KeyPressEventHandler(this.edName_KeyPress);
			this.Label4.Location = new Point(8, 48);
			this.Label4.Name = "Label4";
			this.Label4.Size = new Size(25, 13);
			this.Label4.TabIndex = 1;
			this.Label4.Text = "Пол";
			this.edSex.DropDownStyle = ComboBoxStyle.DropDownList;
			this.edSex.Location = new Point(72, 40);
			this.edSex.Name = "edSex";
			this.edSex.TabIndex = 1;
			this.btnAccept.Image = (resources.GetObject("btnAccept.Image") as Image);
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(96, 168);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 3;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.Image = (resources.GetObject("btnCancel.Image") as Image);
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(184, 168);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 4;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			this.GroupBox1.Controls.Add(this.Label3);
			this.GroupBox1.Controls.Add(this.Label1);
			this.GroupBox1.Controls.Add(this.edFPatr);
			this.GroupBox1.Controls.Add(this.edMPatr);
			this.GroupBox1.Location = new Point(8, 72);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new Size(257, 78);
			this.GroupBox1.TabIndex = 2;
			this.GroupBox1.TabStop = false;
			this.GroupBox1.Text = "Отчества";
			this.Label3.Location = new Point(8, 24);
			this.Label3.Name = "Label3";
			this.Label3.Size = new Size(50, 13);
			this.Label3.TabIndex = 0;
			this.Label3.Text = "Женское";
			this.Label1.Location = new Point(8, 56);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(50, 13);
			this.Label1.TabIndex = 1;
			this.Label1.Text = "Мужское";
			this.edFPatr.Location = new Point(64, 16);
			this.edFPatr.Name = "edFPatr";
			this.edFPatr.Size = new Size(185, 21);
			this.edFPatr.TabIndex = 0;
			this.edFPatr.Text = "";
			this.edFPatr.KeyPress += new KeyPressEventHandler(this.edName_KeyPress);
			this.edMPatr.Location = new Point(64, 48);
			this.edMPatr.Name = "edMPatr";
			this.edMPatr.Size = new Size(185, 21);
			this.edMPatr.TabIndex = 1;
			this.edMPatr.Text = "";
			this.edMPatr.KeyPress += new KeyPressEventHandler(this.edName_KeyPress);
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(273, 201);
			base.Controls.Add(this.Label2);
			base.Controls.Add(this.edName);
			base.Controls.Add(this.Label4);
			base.Controls.Add(this.edSex);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			base.Controls.Add(this.GroupBox1);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.Name = "TfmNameEdit";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Имя";
			this.GroupBox1.ResumeLayout(false);
			base.ResumeLayout(false);
		}
		private void btnAccept_Click(object sender, EventArgs e)
		{
			this.FIName.Name = this.edName.Text;
			this.FIName.Sex = (TGEDCOMObject.TGEDCOMSex)this.edSex.SelectedIndex;
			this.FIName.F_Patronymic = this.edFPatr.Text;
			this.FIName.M_Patronymic = this.edMPatr.Text;
			base.DialogResult = DialogResult.OK;
		}
		private void edName_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (e.KeyChar == '/')
			{
				e.Handled = true;
			}
		}

		public TfmNameEdit()
		{
			this.InitializeComponent();
			TGEDCOMObject.TGEDCOMSex sx = TGEDCOMObject.TGEDCOMSex.svNone;
			do
			{
				this.edSex.Items.Add(TGenEngine.SexStr(sx));
				sx++;
			}
			while (sx != (TGEDCOMObject.TGEDCOMSex)4);
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.Text = GKL.LSList[85];
			this.Label2.Text = GKL.LSList[85];
			this.Label4.Text = GKL.LSList[87];
			this.GroupBox1.Text = GKL.LSList[86];
			this.Label3.Text = GKL.LSList[207];
			this.Label1.Text = GKL.LSList[208];
		}
	}
}
