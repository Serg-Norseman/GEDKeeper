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
	public class TfmAssociationEdit : Form
	{
		private Container Components;
		private Button btnAccept;
		private Button btnCancel;
		private Label Label1;
		private ComboBox EditRelation;
		private Label Label2;
		private TextBox EditPerson;
		private Button btnPersonAdd;
		private TfmBase FBase;
		private TGEDCOMAssociation FAssociation;
		private TGEDCOMIndividualRecord FTempInd;

		[Browsable(false)]
		public TGEDCOMAssociation Association
		{
			get
			{
				return this.FAssociation;
			}
			set
			{
				this.SetAssociation(value);
			}
		}
		[Browsable(false)]
		public TfmBase Base
		{
			get
			{
				return this.FBase;
			}
		}
		private void SetAssociation([In] TGEDCOMAssociation Value)
		{
			this.FAssociation = Value;
			this.EditRelation.Text = this.FAssociation.Relation;
			this.EditPerson.Text = TGenEngine.GetNameStr(this.FAssociation.Individual, true, false);
		}
		private void InitializeComponent()
		{
			ResourceManager resources = new ResourceManager(typeof(TfmAssociationEdit));
			this.Label1 = new Label();
			this.Label2 = new Label();
			this.btnPersonAdd = new Button();
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.EditRelation = new ComboBox();
			this.EditPerson = new TextBox();
			base.SuspendLayout();
			this.Label1.Location = new Point(8, 8);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(65, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Отношение";
			this.Label2.Location = new Point(8, 56);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(50, 13);
			this.Label2.TabIndex = 1;
			this.Label2.Text = "Персона";
			this.btnPersonAdd.AccessibleDescription = "Выбрать персональную запись";
			this.btnPersonAdd.AccessibleName = "";
			this.btnPersonAdd.AccessibleRole = AccessibleRole.ToolTip;
			this.btnPersonAdd.FlatStyle = FlatStyle.Flat;
			this.btnPersonAdd.Location = new Point(320, 69);
			this.btnPersonAdd.Name = "btnPersonAdd";
			this.btnPersonAdd.Size = new Size(26, 26);
			this.btnPersonAdd.TabIndex = 2;
			this.btnPersonAdd.Click += new EventHandler(this.btnPersonAdd_Click);
			this.btnAccept.Image = (resources.GetObject("btnAccept.Image") as Image);
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(176, 112);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 3;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.Image = (resources.GetObject("btnCancel.Image") as Image);
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(264, 112);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 4;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			this.EditRelation.Location = new Point(8, 24);
			this.EditRelation.Name = "EditRelation";
			this.EditRelation.Size = new Size(337, 21);
			this.EditRelation.Sorted = true;
			this.EditRelation.TabIndex = 5;
			this.EditPerson.BackColor = SystemColors.Control;
			this.EditPerson.Location = new Point(8, 72);
			this.EditPerson.Name = "EditPerson";
			this.EditPerson.ReadOnly = true;
			this.EditPerson.Size = new Size(306, 21);
			this.EditPerson.TabIndex = 6;
			this.EditPerson.Text = "";
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(353, 145);
			base.Controls.Add(this.Label1);
			base.Controls.Add(this.Label2);
			base.Controls.Add(this.btnPersonAdd);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			base.Controls.Add(this.EditRelation);
			base.Controls.Add(this.EditPerson);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmAssociationEdit";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Ассоциация";
			base.ResumeLayout(false);
		}
		private void btnAccept_Click(object sender, EventArgs e)
		{
			string rel = this.EditRelation.Text.Trim();
			if (BDSSystem.WStrCmp(rel, "") != 0 && GKL.fmGEDKeeper.Options.Relations.IndexOf(rel) < 0)
			{
				GKL.fmGEDKeeper.Options.Relations.Add(rel);
			}
			this.FAssociation.Relation = this.EditRelation.Text;
			this.FAssociation.Individual = this.FTempInd;
			base.DialogResult = DialogResult.OK;
		}
		private void btnPersonAdd_Click(object sender, EventArgs e)
		{
			this.FTempInd = this.Base.SelectPerson(null, TGenEngine.TTargetMode.tmNone, TGEDCOMObject.TGEDCOMSex.svNone);
			this.EditPerson.Text = TGenEngine.GetNameStr(this.FTempInd, true, false);
		}
		public TfmAssociationEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			int arg_2B_0 = 0;
			int num = GKL.fmGEDKeeper.Options.Relations.Count - 1;
			int i = arg_2B_0;
			if (num >= i)
			{
				num++;
				do
				{
					this.EditRelation.Items.Add(GKL.fmGEDKeeper.Options.Relations[i]);
					i++;
				}
				while (i != num);
			}
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.Text = GKL.LSList[94];
			this.Label1.Text = GKL.LSList[95];
			this.Label2.Text = GKL.LSList[96];
		}
	}
}
