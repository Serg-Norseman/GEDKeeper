#pragma warning disable IDE1006 // Naming Styles

using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class RecordInfoDlg
    {
        private HyperView hyperView1;

        private void InitializeComponent()
        {
            hyperView1 = new HyperView();
            hyperView1.Location = new Point(0, 0);
            hyperView1.Height = Dim.Fill();
            hyperView1.Width = Dim.Fill();
            hyperView1.OnLink += HyperViewLink;

            Size = new Size(64, 48);
            Add(hyperView1);
            KeyDown += Form_KeyDown;
        }
    }
}
