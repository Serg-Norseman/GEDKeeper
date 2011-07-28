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
	public class TfmUserRefEdit : Form
	{
		private Button btnAccept;
		private Button btnCancel;
		private Label Label1;
		private ComboBox EditRef;
		private Label Label2;
		private ComboBox EditType;
		private TfmBase FBase;
		private TGEDCOMUserReference FUserRef;
		[Browsable(false)]
		public TfmBase Base
		{
			get
			{
				return this.FBase;
			}
		}

		[Browsable(false)]
		public TGEDCOMUserReference UserRef
		{
			get
			{
				return this.FUserRef;
			}
			set
			{
				this.SetUserRef(value);
			}
		}

		private void SetUserRef([In] TGEDCOMUserReference Value)
		{
			this.FUserRef = Value;
			this.EditRef.Text = this.FUserRef.StringValue;
			this.EditType.Text = this.FUserRef.ReferenceType;
		}
		private void InitializeComponent()
		{
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.Label1 = new Label();
			this.EditRef = new ComboBox();
			this.Label2 = new Label();
			this.EditType = new ComboBox();
			base.SuspendLayout();
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(176, 112);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 2;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(264, 112);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			this.Label1.Location = new Point(8, 8);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(205, 13);
			this.Label1.TabIndex = 4;
			this.Label1.Text = "Сноска/ссылка/пометка/комментарий";
			this.EditRef.Location = new Point(8, 24);
			this.EditRef.Name = "EditRef";
			this.EditRef.Size = new Size(337, 21);
			this.EditRef.TabIndex = 0;
			this.Label2.Location = new Point(8, 56);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(25, 13);
			this.Label2.TabIndex = 5;
			this.Label2.Text = "Тип";
			this.EditType.Location = new Point(8, 72);
			this.EditType.Name = "EditType";
			this.EditType.Size = new Size(337, 21);
			this.EditType.TabIndex = 1;
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(353, 145);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			base.Controls.Add(this.Label1);
			base.Controls.Add(this.EditRef);
			base.Controls.Add(this.Label2);
			base.Controls.Add(this.EditType);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmUserRefEdit";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Пользовательская сноска";
			base.ResumeLayout(false);
		}
		private void btnAccept_Click(object sender, EventArgs e)
		{
			this.FUserRef.StringValue = this.EditRef.Text;
			this.FUserRef.ReferenceType = this.EditType.Text;
			base.DialogResult = DialogResult.OK;
		}
		public TfmUserRefEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;

			for (TGenEngine.TUserRef ur = TGenEngine.TUserRef.urCustom; ur <= TGenEngine.TUserRef.urUSSR_RearVeteran; ur++)
			{
				this.EditRef.Items.Add(TGenEngine.UserRefs[(int)ur].Name);
			}

			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.Text = GKL.LSList[107];
			this.Label1.Text = GKL.LSList[112];
			this.Label2.Text = GKL.LSList[113];
		}
	}
}
