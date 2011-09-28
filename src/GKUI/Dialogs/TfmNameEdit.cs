using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;

namespace GKUI
{
	public partial class TfmNameEdit : Form
	{
		private TNamesTable.TName FIName;

		public TNamesTable.TName IName
		{
			get { return this.FIName; }
			set { this.SetIName(value); }
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

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.FIName.Name = this.edName.Text;
				this.FIName.Sex = (TGEDCOMSex)this.edSex.SelectedIndex;
				this.FIName.F_Patronymic = this.edFPatr.Text;
				this.FIName.M_Patronymic = this.edMPatr.Text;
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmNameEdit.Accept(): " + E.Message);
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

		public TfmNameEdit()
		{
			this.InitializeComponent();
			TGEDCOMSex sx = TGEDCOMSex.svNone;
			do
			{
				this.edSex.Items.Add(TGenEngine.SexStr(sx));
				sx++;
			}
			while (sx != (TGEDCOMSex)4);
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
