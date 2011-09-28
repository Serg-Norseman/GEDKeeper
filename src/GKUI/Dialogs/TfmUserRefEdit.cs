using System;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;

namespace GKUI
{
	public partial class TfmUserRefEdit : Form
	{
		private TfmBase FBase;
		private TGEDCOMUserReference FUserRef;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		public TGEDCOMUserReference UserRef
		{
			get { return this.FUserRef; }
			set { this.SetUserRef(value); }
		}

		private void SetUserRef([In] TGEDCOMUserReference Value)
		{
			this.FUserRef = Value;
			this.EditRef.Text = this.FUserRef.StringValue;
			this.EditType.Text = this.FUserRef.ReferenceType;
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.FUserRef.StringValue = this.EditRef.Text;
				this.FUserRef.ReferenceType = this.EditType.Text;
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmUserRefEdit.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
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
