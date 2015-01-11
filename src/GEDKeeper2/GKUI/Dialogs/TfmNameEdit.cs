using System;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Interfaces;

/// <summary>
/// 
/// </summary>

namespace GKUI.Dialogs
{
	public partial class TfmNameEdit : Form
	{
		private readonly IBase fBase;
		private NamesTable.NameEntry fNameEntry;

		public IBase Base
		{
			get { return this.fBase; }
		}

		public NamesTable.NameEntry IName
		{
			get { return this.fNameEntry; }
			set { this.SetIName(value); }
		}

		private void SetIName(NamesTable.NameEntry value)
		{
			this.fNameEntry = value;
			if (this.fNameEntry == null)
			{
				this.edName.Text = "";
				this.edSex.SelectedIndex = 0;
				this.edFPatr.Text = "";
				this.edMPatr.Text = "";
			}
			else
			{
				this.edName.Text = this.fNameEntry.Name;
				this.edSex.SelectedIndex = (sbyte)this.fNameEntry.Sex;
				this.edFPatr.Text = this.fNameEntry.F_Patronymic;
				this.edMPatr.Text = this.fNameEntry.M_Patronymic;
			}
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.fNameEntry.Name = this.edName.Text;
				this.fNameEntry.Sex = (TGEDCOMSex)this.edSex.SelectedIndex;
				this.fNameEntry.F_Patronymic = this.edFPatr.Text;
				this.fNameEntry.M_Patronymic = this.edMPatr.Text;
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmNameEdit.Accept(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void edName_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (e.KeyChar == '/')
			{
				e.Handled = true;
			}
		}

		public TfmNameEdit(IBase aBase)
		{
			this.InitializeComponent();
			this.fBase = aBase;

			for (TGEDCOMSex sx = TGEDCOMSex.svNone; sx <= TGEDCOMSex.svLast; sx++)
			{
				this.edSex.Items.Add(GKUtils.SexStr(sx));
			}

			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.Text = LangMan.LS(LSID.LSID_Name);
			this.Label2.Text = LangMan.LS(LSID.LSID_Name);
			this.Label4.Text = LangMan.LS(LSID.LSID_Sex);
			this.GroupBox1.Text = LangMan.LS(LSID.LSID_Patronymic);
			this.Label3.Text = LangMan.LS(LSID.LSID_PatFemale);
			this.Label1.Text = LangMan.LS(LSID.LSID_PatMale);
		}
	}
}
