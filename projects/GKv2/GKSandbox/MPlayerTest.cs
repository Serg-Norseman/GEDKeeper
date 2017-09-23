using System;
using System.Windows.Forms;

namespace GKSandbox
{
    /// <summary>
    /// 
    /// </summary>
    public partial class MPlayerTest : Form
    {
        public MPlayerTest()
        {
            InitializeComponent();
        }

        private void btnOpen_Click(object sender, EventArgs e)
        {
            using (var ofd = new OpenFileDialog())
            {
                if (ofd.ShowDialog() == DialogResult.OK)
                {
                    txtFileName.Text = ofd.FileName;
                    mediaPlayer1.MediaFile = ofd.FileName;
                }
            }
        }
    }
}
