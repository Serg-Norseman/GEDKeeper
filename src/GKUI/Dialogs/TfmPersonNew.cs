using System;
using System.Runtime.InteropServices;
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
		private TGenEngine.TTargetMode FTargetMode;

		public TGEDCOMIndividualRecord Target
		{
			get { return this.FTarget; }
			set { this.SetTarget(value); }
		}

		public TGenEngine.TTargetMode TargetMode
		{
			get { return this.FTargetMode; }
			set { this.FTargetMode = value; }
		}

		private void SetTarget([In] TGEDCOMIndividualRecord Value)
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

					switch (this.FTargetMode) {
						case TGenEngine.TTargetMode.tmParent:
							{
								this.edPatronymic.Items.Add(names.GetPatronymicByName(iName, TGEDCOMSex.svMale));
								this.edPatronymic.Items.Add(names.GetPatronymicByName(iName, TGEDCOMSex.svFemale));
								break;
							}

						case TGenEngine.TTargetMode.tmChild:
							{
								TGEDCOMSex sx = (TGEDCOMSex)this.EditSex.SelectedIndex;
								if (sx != TGEDCOMSex.svMale)
								{
									if (sx == TGEDCOMSex.svFemale)
									{
										this.edFamily.Text = "(" + this.edFamily.Text + ")";
									}
								} else {
									this.edName.Text = names.GetNameByPatronymic(iPatronymic, TGEDCOMSex.svMale);
								}
								break;
							}
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
				tb.Text = TGenEngine.SetAsName(tb.Text);
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
				this.EditSex.Items.Add(TGenEngine.SexStr(sx));
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
