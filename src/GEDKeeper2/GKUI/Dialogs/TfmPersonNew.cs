using System;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKUI.Dialogs
{
	/// <summary>
	/// 
	/// </summary>
	public partial class TfmPersonNew : Form, IBaseEditor
	{
		private readonly IBaseWindow fBase;
		private GEDCOMIndividualRecord fTarget;
		private TargetMode fTargetMode;

        public IBaseWindow Base
		{
			get { return this.fBase; }
		}

		public GEDCOMIndividualRecord Target
		{
			get { return this.fTarget; }
			set { this.SetTarget(value); }
		}

		public TargetMode TargetMode
		{
			get { return this.fTargetMode; }
			set { this.fTargetMode = value; }
		}

		private void SetTarget(GEDCOMIndividualRecord value)
		{
			try
			{
				this.fTarget = value;

				if (this.fTarget != null)
				{
					string iFamily, iName, iPatronymic;
					this.fTarget.GetNameParts(out iFamily, out iName, out iPatronymic);
					this.edFamily.Text = iFamily;
					INamesTable names = TfmGEDKeeper.Instance.NamesTable;
					GEDCOMSex sx = (GEDCOMSex)this.EditSex.SelectedIndex;

					switch (this.fTargetMode) {
						case TargetMode.tmParent:
							if (sx == GEDCOMSex.svFemale) {
								this.edFamily.Text = GKUtils.GetRusWifeSurname(iFamily);
							}
							this.edPatronymic.Items.Add(names.GetPatronymicByName(iName, GEDCOMSex.svMale));
							this.edPatronymic.Items.Add(names.GetPatronymicByName(iName, GEDCOMSex.svFemale));
							this.edPatronymic.Text = names.GetPatronymicByName(iName, sx);
							break;

						case TargetMode.tmChild:
							switch (sx) {
								case GEDCOMSex.svMale:
									this.edName.Text = names.GetNameByPatronymic(iPatronymic);
									break;
								case GEDCOMSex.svFemale:
									this.edFamily.Text = "(" + GKUtils.GetRusWifeSurname(iFamily) + ")";
									break;
							}
							break;
							
						case TargetMode.tmWife:
							this.edFamily.Text = "(" + GKUtils.GetRusWifeSurname(iFamily) + ")";
							break;
					}
				}
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmPersonNew.SetTarget("+this.fTargetMode.ToString()+"): " + ex.Message);
			}
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			base.DialogResult = DialogResult.OK;
		}

		private void edFamily_KeyDown(object sender, KeyEventArgs e)
		{
            TextBox tb = (sender as TextBox);
            
            if (tb != null && e.KeyCode == Keys.Down && e.Control)
			{
				tb.Text = GEDCOMUtils.NormalizeName(tb.Text);
			}
		}

		private void edFamily_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (e.KeyChar == '/')
			{
				e.Handled = true;
			}
		}

		public TfmPersonNew(IBaseWindow aBase)
		{
			this.InitializeComponent();

			this.fBase = aBase;

			for (GEDCOMSex sx = GEDCOMSex.svNone; sx <= GEDCOMSex.svUndetermined; sx++)
			{
				this.EditSex.Items.Add(GKUtils.SexStr(sx));
			}

			// SetLang()
			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.Text = LangMan.LS(LSID.LSID_WinPersonNew);
			this.Label1.Text = LangMan.LS(LSID.LSID_Surname);
			this.Label2.Text = LangMan.LS(LSID.LSID_Name);
			this.Label3.Text = LangMan.LS(LSID.LSID_Patronymic);
			this.Label4.Text = LangMan.LS(LSID.LSID_Sex);
		}
	}
}
