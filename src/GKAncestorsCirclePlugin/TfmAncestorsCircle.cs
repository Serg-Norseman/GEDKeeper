using System;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore.Interfaces;

namespace GKAncestorsCirclePlugin
{
	public partial class TfmAncestorsCircle : Form, ILocalization
	{
		private AncestorsCircle fAncestorsCircle;
		private IBase fBase;
		private ILangMan fLangMan;
		private IPlugin fPlugin;
		private GEDCOMIndividualRecord fRootPerson;

		public AncestorsCircleOptions Options
		{
			get { return this.fAncestorsCircle.Options; }
		}
		
		public TfmAncestorsCircle(IPlugin plugin)
		{
			this.InitializeComponent();
			//this.MdiParent = TfmGEDKeeper.Instance;
			this.ShowInTaskbar = true;
			
			this.fPlugin = plugin;
			this.fLangMan = plugin.LangMan;
			
			this.fBase = plugin.Host.GetCurrentFile();
			this.fRootPerson = fBase.GetSelectedPerson();

			this.fAncestorsCircle = new AncestorsCircle(this.fBase.Tree, this.fRootPerson);
			this.fAncestorsCircle.Dock = DockStyle.Fill;
			
			this.Controls.Add(this.fAncestorsCircle);

			(this as ILocalization).SetLang();
		}

		void miOptions_Click(object sender, EventArgs e)
		{
			using (ACOptionsDialog dlg = new ACOptionsDialog(this.fPlugin)) {
				dlg.Options = this.fAncestorsCircle.Options;

				if (dlg.ShowDialog() == DialogResult.OK) {
					this.fAncestorsCircle.Invalidate();
				}
			}
		}

		void ILocalization.SetLang()
		{
			this.Text = fLangMan.LS(ACLS.LSID_AncestorsCircle);
		}
	}
}
