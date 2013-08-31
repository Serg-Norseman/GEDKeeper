using System;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKCore;

/// <summary>
/// Localization: clean
/// </summary>

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

		private void SetUserRef(TGEDCOMUserReference Value)
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

			for (TUserRef ur = TUserRef.urCustom; ur <= TUserRef.urUSSR_RearVeteran; ur++)
			{
				this.EditRef.Items.Add(GKData.UserRefs[(int)ur]);
			}

			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.Text = LangMan.LSList[107];
			this.Label1.Text = LangMan.LSList[112];
			this.Label2.Text = LangMan.LSList[113];
		}
	}
}
