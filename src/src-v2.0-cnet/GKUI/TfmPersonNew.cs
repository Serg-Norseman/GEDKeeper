using GedCom551;
using GKCore;
using GKSys;
using System;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmPersonNew : Form
	{
		public TextBox edFamily;
		public TextBox edName;
		public ComboBox edPatronymic;
		public ComboBox EditSex;
		private Label Label1;
		private Label Label2;
		private Label Label3;
		private Label Label4;
		private Button btnAccept;
		private Button btnCancel;
		private TGEDCOMIndividualRecord FTarget;
		private TGenEngine.TTargetMode FTargetMode;

		[Browsable(false)]
		public TGEDCOMIndividualRecord Target
		{
			get
			{
				return this.FTarget;
			}
			set
			{
				this.SetTarget(value);
			}
		}
		[Browsable(false)]
		public TGenEngine.TTargetMode TargetMode
		{
			get
			{
				return this.FTargetMode;
			}
			set
			{
				this.FTargetMode = value;
			}
		}
		private void SetTarget([In] TGEDCOMIndividualRecord Value)
		{
			this.FTarget = Value;
			TNamesTable names = GKL.fmGEDKeeper.NamesTable;
			if (this.FTarget != null)
			{
				string iFamily = "";
				string iName = "";
				string iPatronymic = "";
				TGenEngine.GetNameParts(this.FTarget, ref iFamily, ref iName, ref iPatronymic);
				this.edFamily.Text = iFamily;
				TGenEngine.TTargetMode fTargetMode = this.FTargetMode;
				if (fTargetMode != TGenEngine.TTargetMode.tmAncestor)
				{
					if (fTargetMode == TGenEngine.TTargetMode.tmDescendant)
					{
						TGEDCOMObject.TGEDCOMSex sx = (TGEDCOMObject.TGEDCOMSex)this.EditSex.SelectedIndex;
						if (sx != TGEDCOMObject.TGEDCOMSex.svMale)
						{
							if (sx == TGEDCOMObject.TGEDCOMSex.svFemale)
							{
								this.edFamily.Text = "(" + this.edFamily.Text + ")";
							}
						}
						else
						{
							this.edName.Text = names.GetNameByPatronymic(iPatronymic, TGEDCOMObject.TGEDCOMSex.svMale);
						}
					}
				}
				else
				{
					this.edPatronymic.Items.Add(names.GetPatronymicByName(iName, TGEDCOMObject.TGEDCOMSex.svMale));
					this.edPatronymic.Items.Add(names.GetPatronymicByName(iName, TGEDCOMObject.TGEDCOMSex.svFemale));
				}
			}
		}
		private void InitializeComponent()
		{
			this.Label1 = new Label();
			this.edFamily = new TextBox();
			this.Label2 = new Label();
			this.edName = new TextBox();
			this.Label3 = new Label();
			this.edPatronymic = new ComboBox();
			this.Label4 = new Label();
			this.EditSex = new ComboBox();
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			base.SuspendLayout();
			this.Label1.Location = new Point(8, 16);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(55, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Фамилия";
			this.edFamily.Location = new Point(72, 8);
			this.edFamily.Name = "edFamily";
			this.edFamily.Size = new Size(185, 21);
			this.edFamily.TabIndex = 0;
			this.edFamily.Text = "";
			this.edFamily.KeyDown += new KeyEventHandler(this.edFamily_KeyDown);
			this.edFamily.KeyPress += new KeyPressEventHandler(this.edFamily_KeyPress);
			this.Label2.Location = new Point(8, 40);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(55, 13);
			this.Label2.TabIndex = 1;
			this.Label2.Text = "Имя";
			this.edName.Location = new Point(72, 32);
			this.edName.Name = "edName";
			this.edName.Size = new Size(185, 21);
			this.edName.TabIndex = 1;
			this.edName.Text = "";
			this.edName.KeyDown += new KeyEventHandler(this.edFamily_KeyDown);
			this.edName.KeyPress += new KeyPressEventHandler(this.edFamily_KeyPress);
			this.Label3.Location = new Point(8, 64);
			this.Label3.Name = "Label3";
			this.Label3.Size = new Size(55, 13);
			this.Label3.TabIndex = 2;
			this.Label3.Text = "Отчество";
			this.edPatronymic.Location = new Point(72, 56);
			this.edPatronymic.Name = "edPatronymic";
			this.edPatronymic.Size = new Size(185, 21);
			this.edPatronymic.TabIndex = 2;
			this.edPatronymic.KeyDown += new KeyEventHandler(this.edFamily_KeyDown);
			this.edPatronymic.KeyPress += new KeyPressEventHandler(this.edFamily_KeyPress);
			this.Label4.Location = new Point(8, 88);
			this.Label4.Name = "Label4";
			this.Label4.Size = new Size(55, 13);
			this.Label4.TabIndex = 3;
			this.Label4.Text = "Пол";
			this.EditSex.DropDownStyle = ComboBoxStyle.DropDownList;
			this.EditSex.Location = new Point(72, 80);
			this.EditSex.Name = "EditSex";
			this.EditSex.TabIndex = 3;
			this.btnAccept.Location = new Point(48, 120);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 4;
			this.btnAccept.Text = "Принять";
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.Location = new Point(136, 120);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 5;
			this.btnCancel.Text = "Отменить";
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(266, 153);
			base.Controls.Add(this.Label1);
			base.Controls.Add(this.edFamily);
			base.Controls.Add(this.edName);
			base.Controls.Add(this.Label2);
			base.Controls.Add(this.Label3);
			base.Controls.Add(this.edPatronymic);
			base.Controls.Add(this.Label4);
			base.Controls.Add(this.EditSex);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmPersonNew";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Новая персональная запись";
			base.ResumeLayout(false);
		}
		private void btnAccept_Click(object sender, EventArgs e)
		{
			base.DialogResult = DialogResult.OK;
		}

		private void edFamily_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Down && e.Control)
			{
				string ss = (sender as TextBox).Text;
				(sender as TextBox).Text = TGKSys.SetAsName(ss);
			}
		}

		private void edFamily_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (e.KeyChar == '/')
			{
				e.Handled = true;
			}
		}

		public TfmPersonNew()
		{
			this.InitializeComponent();

			for (TGEDCOMObject.TGEDCOMSex sx = TGEDCOMObject.TGEDCOMSex.svNone; sx <= TGEDCOMObject.TGEDCOMSex.svUndetermined; sx++)
			{
				this.EditSex.Items.Add(TGenEngine.SexStr(sx));
			}

			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.Text = GKL.LSList[102];
			this.Label1.Text = GKL.LSList[84];
			this.Label2.Text = GKL.LSList[85];
			this.Label3.Text = GKL.LSList[86];
			this.Label4.Text = GKL.LSList[87];
		}
	}
}
