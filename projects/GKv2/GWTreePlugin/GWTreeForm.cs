using System;
using System.Windows.Forms;
using GKCore.Interfaces;

namespace GWTreePlugin
{
    public partial class GWTreeForm : Form, ILocalizable
    {
        private readonly Plugin fPlugin;
        private IBaseWindow fBase;

        public GWTreeForm(Plugin plugin)
        {
            InitializeComponent();

            fPlugin = plugin;

            SetLocale();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fPlugin.CloseForm();
            }
            base.Dispose(disposing);
        }

        private void GWTreeForm_Load(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetShow(fPlugin);
            BaseChanged(fPlugin.Host.GetCurrentFile());
        }

        private void GWTreeForm_Closed(object sender, EventArgs e)
        {
            BaseChanged(null);
            fPlugin.Host.WidgetClose(fPlugin);
        }

        public void BaseChanged(IBaseWindow baseWin)
        {
            if (fBase != baseWin) {
                fBase = baseWin;
                UpdateView();
            }
        }

        private void UpdateView()
        {
            try {
                if (fBase != null) {
                    gwTreeView.Context = fBase.Context;
                    gwTreeView.Model.Load(fBase.GetSelectedPerson());
                }
            } catch (Exception ex) {
                MessageBox.Show(ex.Message);
            }
        }

        public void SetLocale()
        {
            //Text = fPlugin.LangMan.LS(PLS.LSID_Title);
        }

        private void GWTreeForm_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) Close();
        }
    }
}
