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
	public partial class TfmPersonNew : Form
	{
		private TGEDCOMIndividualRecord FTarget;
		private TTargetMode FTargetMode;

		public TGEDCOMIndividualRecord Target
		{
			get { return this.FTarget; }
			set { this.SetTarget(value); }
		}

		public TTargetMode TargetMode
		{
			get { return this.FTargetMode; }
			set { this.FTargetMode = value; }
		}

		private void SetTarget(TGEDCOMIndividualRecord Value)
		{
			try
			{
				this.FTarget = Value;

				if (this.FTarget != null)
				{
					string iFamily, iName, iPatronymic;
					this.FTarget.aux_GetNameParts(out iFamily, out iName, out iPatronymic);
					this.edFamily.Text = iFamily;
					NamesTable names = GKUI.TfmGEDKeeper.Instance.NamesTable;
					TGEDCOMSex sx = (TGEDCOMSex)this.EditSex.SelectedIndex;

					switch (this.FTargetMode) {
						case TTargetMode.tmParent:
							if (sx == TGEDCOMSex.svFemale) {
								this.edFamily.Text = NamesTable.GetRusWifeSurname(iFamily);
							}
							this.edPatronymic.Items.Add(names.GetPatronymicByName(iName, TGEDCOMSex.svMale));
							this.edPatronymic.Items.Add(names.GetPatronymicByName(iName, TGEDCOMSex.svFemale));
							this.edPatronymic.Text = names.GetPatronymicByName(iName, sx);
							break;

						case TTargetMode.tmChild:
							switch (sx) {
								case TGEDCOMSex.svMale:
									this.edName.Text = names.GetNameByPatronymic(iPatronymic, TGEDCOMSex.svMale);
									break;
								case TGEDCOMSex.svFemale:
									this.edFamily.Text = "(" + NamesTable.GetRusWifeSurname(iFamily) + ")";
									break;
							}
							break;
							
						case TTargetMode.tmWife:
							this.edFamily.Text = "(" + NamesTable.GetRusWifeSurname(iFamily) + ")";
							break;
					}
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmPersonNew.SetTarget("+this.FTargetMode.ToString()+"): " + E.Message);
			}
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			base.DialogResult = DialogResult.OK;
		}

		private void edFamily_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Down && e.Control)
			{
				TextBox tb = (sender as TextBox);
				tb.Text = GKUtils.SetAsName(tb.Text);
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

			for (TGEDCOMSex sx = TGEDCOMSex.svNone; sx <= TGEDCOMSex.svUndetermined; sx++)
			{
				this.EditSex.Items.Add(GKUtils.SexStr(sx));
			}

			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.Text = LangMan.LSList[102];
			this.Label1.Text = LangMan.LSList[84];
			this.Label2.Text = LangMan.LSList[85];
			this.Label3.Text = LangMan.LSList[86];
			this.Label4.Text = LangMan.LSList[87];
		}
	}
}
